library('move')
library('shiny')
library('raster')
library('foreach')
library('sf')
library('fasterize')
library('rgeos')

shinyModuleUserInterface <- function(id, label, grid = 50000) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Raster map of migration density"),
    sliderInput(inputId = ns("grid"), 
                label = "Choose a raster grid size in m", 
                value = grid, min = 1000, max = 300000),
    plotOutput(ns("map"))
  )
}

shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  configuration <- list()

  print(ns('grid'))

  configuration["grid"] <- input[[ns('grid')]]

  configuration
}

shinyModule <- function(input, output, session, data, grid = 50000) {
  dataObj <- reactive({ data })
  current <- reactiveVal(data)
  
  migrasterObj <- reactive({
    data.split <- move::split(dataObj())
    L <- foreach(datai = data.split) %do% {
      print(namesIndiv(datai))
      Line(coordinates(datai))
    }
    names(L) <- names(data.split)
    
    Ls <-  Lines(L,"ID"="segm")
    sLs <- SpatialLines(list(Ls),proj4string=CRS("+proj=longlat +ellps=WGS84"))
  })
  
  migrasterObjT <- reactive({
    sLsT <- spTransform(migrasterObj(),CRSobj="+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
    
    outputRaster <- raster(ext=extent(sLsT), resolution=grid, crs = "+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", vals=NULL)
    
    sLsT.poly <- buffer(sLsT,width=input$grid/2)
    sLsT.sf <- st_as_sf(sLsT.poly)
    fasterize(sLsT.sf,outputRaster,fun="count")
    
    #rasterize(sLsT,outputRaster,fun=function(x,...) sum(length(x)),update=TRUE)
  })  

  coastlinesObj <- reactive({
    coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
    coastlinesC <- crop(coastlines,extent(migrasterObj()))
    spTransform(coastlinesC,CRSobj="+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  })
  
  output$map <- renderPlot({
    plot(migrasterObjT(),colNA=NA,axes=FALSE,asp=1)  
    plot(coastlinesObj(), add = TRUE)
  })
  
  return(reactive({ current() }))
}
