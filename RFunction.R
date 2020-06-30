library('move')
library('raster')

rFunction <- function(data,resol=NULL) #resolution in m
{
  if (is.null(resol))
  {
    logger.info("You have not chosen a raster resolution. Please change. Return lines plot.")
    data.split <- split(data)
    
    L <- foreach(datai = data.split) %do% {
      logger.info(namesIndiv(datai))
      Line(coordinates(datai))
    }
    names(L) <- names(data.split)
    
    Ls <-  Lines(L,"ID"="segm")
    sLs <- SpatialLines(list(Ls),proj4string=CRS("+proj=longlat +ellps=WGS84"))
    sLsT <- spTransform(sLs,CRSobj="+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
    
    plot(sLsT,col="red")
    
    download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", destfile = 'coastlines.zip') # not sure if this is not too much dependency, maybe copy file somewhere
    unzip(zipfile = "coastlines.zip", exdir = 'ne-coastlines-10m')
    coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
    coastlinesT <- spTransform(coastlines,CRSobj="+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
    plot(coastlinesT,add=TRUE)
  } else
  {
    logger.info("You have not chosen ",resol,"m as raster resolution.")
    data.split <- split(data)
    
    L <- foreach(datai = data.split) %do% {
      logger.info(namesIndiv(datai))
      Line(coordinates(datai))
    }
    names(L) <- names(data.split)
    
    Ls <-  Lines(L,"ID"="segm")
    sLs <- SpatialLines(list(Ls),proj4string=CRS("+proj=longlat +ellps=WGS84"))
    sLsT <- spTransform(sLs,CRSobj="+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
    
    outputRaster <- raster(ext=extent(sLsT), resolution=resol, crs = "+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", vals=NULL) #empty raster to put lines into
    MigRaster <- rasterize(sLsT,outputRaster,fun=function(x,...) sum(length(x)),update=TRUE) #update=TRUE seems to make it faster; fasterize() if convert to sf and polygon
    plot(MigRaster,colNA=NA,axes=FALSE)
    
    download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", destfile = 'coastlines.zip') # not sure if this is not too much dependency, maybe copy file somewhere
    unzip(zipfile = "coastlines.zip", exdir = 'ne-coastlines-10m')
    coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
    coastlinesT <- spTransform(coastlines,CRSobj="+proj=aeqd +lat_0=53 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
    coastlinesT_crop <- crop(x = coastlinesT, y=MigRaster)
    plot(coastlinesT_crop, add = TRUE)
  }
}
