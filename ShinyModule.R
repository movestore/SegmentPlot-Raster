library('move')
library('shiny')
library('raster')
library('foreach')
library('sf')
library('fasterize')
library('rgeos')

#setwd("/root/app/")

shinyModuleUserInterface <- function(id, label, grid = 50000, meth="fast") {
  ns <- NS(id)
  
  tagList(
    titlePanel("Raster map of migration density"),
    sliderInput(inputId = ns("grid"), 
                label = "Choose a raster grid size in m", 
                value = grid, min = 1000, max = 300000),
    radioButtons(inputId = ns("meth"),
                 label = "Select rasterizing method",
                 choices = c("fasterize with buffer (slow for dense data)" = "fast", "rasterize as lines (slow for large data)"="rast"),
                 selected = meth, inline = TRUE),
    plotOutput(ns("map"))
  )
}

shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  configuration <- list()

  print(ns('grid'))
  configuration["grid"] <- input[[ns('grid')]]

  print(ns('meth'))
  configuration["meth"] <- input[[ns('meth')]]
  
  configuration
}

shinyModule <- function(input, output, session, data, grid = 50000, meth="fast") {
  current <- reactiveVal(data)
  
    data.split <- move::split(data)
    
    #remove all move objects with less than 2 positions
    data.split_nozero <- data.split[unlist(lapply(data.split, length) > 1)]
    if (length(data.split_nozero)==0) logger.info("Warning! Error! There are no segments (or at least 2 positions) in your data set. No rasterization of the tracks possible.") # this is very unlikely, therefore not adaption in the below code for it.

    L <- foreach(datai = data.split_nozero) %do% {
      print(namesIndiv(datai))
      Line(coordinates(datai))
    }
    names(L) <- names(data.split_nozero)
    
    Ls <-  Lines(L,"ID"="segm")
    sLs <- SpatialLines(list(Ls),proj4string=CRS("+proj=longlat +ellps=WGS84"))

    sLsT <- spTransform(sLs,CRSobj="+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
    
    outputRaster <- reactive({
      raster(ext=extent(sLsT), resolution=input$grid, crs = "+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", vals=NULL)
    })
    
    out <- reactive({
      if (input$meth=="fast")
      {
        logger.info(paste("fasterize() for fast raster plotting. Calculated buffer polygon of width",input$grid/4,". Buffer slow if dense points."))
        
      sLsT.poly <- gBuffer(sLsT,width=input$grid/4) #this seems to be a bottleneck for dense data
      sLsT.sf <- st_as_sf(sLsT.poly)
      res <- fasterize(sLsT.sf,outputRaster(),fun="count")
      if (length(res)==1 & is.na(values(res)[1]))
        {
        values(res) <- 1
        logger.info("Output is just one raster cell with NA density. Likely not enough data points or too large grid size. Return single cell raster with value 1.")
        }
      } else
      {
      logger.info("rasterize() for more flexible and correct, but slow raster plotting. No buffer.")
      res <- rasterize(sLsT,outputRaster(),fun=function(x,...) sum(length(x)),update=TRUE)
      if (length(res)==1 & is.na(values(res)[1]))
        {
        values(res) <- 1
        logger.info("Output is just one raster cell with NA density. Likely not enough data points or too large grid size. Return single cell raster with value 1.")
        }
      }
      res
    })
    
  #coastlinesObj <- reactive({
    coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
    #if (raster::area(gEnvelope(migrasterObj())) > input$grid) 
    coastlinesC <- crop(coastlines,extent(sLs)) 
    #else coastlinesC <- coastlines
    coast <- spTransform(coastlinesC,CRSobj="+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  #  coast
  #})
  
  output$map <- renderPlot({
    plot(out(),colNA=NA,axes=FALSE,asp=1) 
    plot(coast, add = TRUE)
  })
  
  return(reactive({ current() }))
}


