# Segment Raster Plot
MoveApps

Github repository: *github.com/movestore/SegmentPlot-Raster*

## Description
Maps movement tracks on a (user defined) raster and plots it into a shiny UI with coastlines overlaid. Developed to plot large data amounts of e.g. migration tracks. Proper performance for up to 50,000 locations, comfortable performance less than 10,000 locations.

## Documentation
For a set of movement tracks an interactive raster map is generated. For better visibility the raster is overlaid with  (10 m resolution) coastlines that were downloaded from https://www.naturalearthdata.com/.

The creation of the raster map starts with a transformation of the tracks into spatial lines and a transformation of those lines to the `aeqd`(area equal distance) projection. This is necessary for regular raster cells. Next, if the fasterize method is selected, buffers with width 1/4 of the grid size are calculated around the lines and transformed into a raster. If the rasterize method is selected, the spatial lines are directly transformed into a raster with values being the number of lines passing through the respective cell.

:warning: Input segments of length < 2 are removed. The case that there are no segments of at least 2 locations in the input data set is indicated by a warning, but the code is still attempted to run. This will lead to an error.

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format
Shiny user interface (UI)

### Artefacts
none

### Parameters 
`grid`: Number of grid cells for the x- as well as y-axis of the raster plot. Only quadratic plots allowed.

`meth`: Rasterisation method to use. The options are `fasterize` and `rasterize`. Both create raster objects of a set of location data. Fasterize is much faster, but uses a spatial buffer around the points, rasterize takes exact lines between locations but is slow for larger data sets.

### Null or error handling:
**Parameter `grid`:** The default grid size is 50,000. If this value does not fit the data, usually only one giant grid cell is determiend, a warning is given. If the grid size is too large, the analyses might take a very long time.

**Parameter `meth`:** For the Radiobutton options no error or null input for `method`are possible. Long run times are possible for `rasterize`.

**Data:** For the use in further Apps, the input data set is returned.

