# This code generates queen's and rook's based neighbors from scratch,
# .. first a a regular spatial grid is constructed.
#
library(raster)
library(sf)
library(spdep)
#
e <- as(raster::extent(0, 100, 0, 100), "SpatialPolygons") %>% 
  st_as_sf()
grd_lrg <- st_make_grid(e, cellsize = c(10, 10))
coords <- st_make_grid(e, cellsize = c(10, 10), what = "centers")
#
plot(grd_lrg)
plot(coords, col = "red", add=T)
#
## queen neighbors
nb_queen <- poly2nb(grd_lrg, queen = T)
#
plot(grd_lrg)
plot(nb_queen, coords, col = "red", add = T)
#
## rook neighbors
# (queen = F does not seem to work properly here, as of 8.8.2019)
nb_rook <- dnearneigh(coords, d1=0, d2=11)
#
plot(grd_lrg)
plot(nb_rook, coords, col = "red", add = T)
#
#
## adapted from https://gis.stackexchange.com/questions/273233/create-spatial-polygon-grid-from-spatial-points-in-r
