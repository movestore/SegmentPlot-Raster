# Segment Raster Plot
MoveApps

Github repository: *github.com/movestore/SegmentPlot-Raster*

## Description
Maps movement tracks on a (user defined) raster and plots it into a shiny UI with coastlines overlaid. Developed to plot large data amounts of e.g. migration tracks. Proper performance for up to 50,000 locations, comfortable performance less than 10,000 locations.

## Documentation
For a set of movement tracks an interactive raster map is generated. For better visibility the raster is overlaid with  (10 m resolution) coastlines that were downloaded from https://www.naturalearthdata.com/.

The creation of the raster map starts with a transformation of the tracks into spatial lines and a transformation of those lines to the `aeqd`(area equal distance) projection. This is necessary for regular raster cells. 

The next steps depend on the selected rasterization method.
1) sf_rasterize transforms the data into a line segment object and determines which cells are crossed or touched by the line. Here simply the events of touching/crossing are summed up over the individuals.
2)For the fasterize method, first buffers with width 1/4 of the grid size are calculated around the lines. The resulting polygons are then transformed into a raster.
3) If the rasterize method is selected, the spatial lines are directly transformed into a raster with values being the number of lines passing through the respective cell. Here the lengths of the line segments passing through each cell are summed.

:warning: Input segments of length < 2 are removed. The case that there are no segments of at least 2 locations in the input data set is indicated by a warning, but the code is still attempted to run. This will lead to an error.

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format
Shiny user interface (UI)

### Artefacts
none

### Parameters 
`Choose a raster grid size in `: Number of grid cells for the x- as well as y-axis of the raster plot. Only quadratic plots allowed.

### Null or error handling:
**Parameter `Choose a raster grid size in `:** The default grid size is 50,000. If this value does not fit the data, usually only one giant grid cell is determiend, a warning is given. If the grid size is too large, the analyses might take a very long time.

**Data:** For the use in further Apps, the input data set is returned.

