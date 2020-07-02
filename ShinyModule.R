library(move)
library(shiny)
library(raster)
library(foreach)

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
    data.split <- split(dataObj()) #change to check if MoveStack...
    L <- foreach(datai = data.split) %do% {
      print(namesIndiv(datai))
      Line(coordinates(datai))
    }
    names(L) <- names(data.split)
    
    Ls <-  Lines(L,"ID"="segm")
    sLs <- SpatialLines(list(Ls),proj4string=CRS("+proj=longlat +ellps=WGS84"))
    sLsT <- spTransform(sLs,CRSobj="+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
    rasterize(sLsT,raster(ext=extent(sLsT), resolution=input$grid, crs = "+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", vals=NULL),fun=function(x,...) sum(length(x)),update=TRUE)
  })
  
  coastlinesObj <- reactive({
    unzip(zipfile = "coastlines.zip", exdir = 'ne-coastlines-10m')
    coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
    spTransform(coastlines,CRSobj="+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  })
  
  output$map <- renderPlot({
    plot(migrasterObj(),colNA=NA,axes=FALSE,asp=1)  
    plot(crop(x=coastlinesObj(), y=migrasterObj()), add = TRUE)
  })
  
  return(reactive({ current() }))
}
