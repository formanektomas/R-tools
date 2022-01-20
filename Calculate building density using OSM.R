# Building Density in R with OSM,
# NUTS3 region Karlovarský kraj (CZ041)
# adapted from
# https://greggsaldutti.netlify.app/post/2021-05-05-calculating-building-density-in-r-with-osm-data/
#
#loading packages
library(osmdata)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
#
#
#retrieve bounding box for region of interest
My_bbox <- getbb("Karlovarský kraj", format_out = "polygon")
#
# retrieve level 8 administrative boundaries ... 
# note this is some 3GB !!
My_boundary <- opq(My_bbox) %>%
  add_osm_feature(key = "admin_level", value = "8") %>%
  osmdata_sf()

My_polys <- My_boundary$osm_multipolygons

#remove digits from any distirct name 
My_polys$name <- gsub('[[:digit:]]+', '', My_polys$name)
#remove . from any district name
My_polys$name <- gsub("[.]", '', My_polys$name)
#trim whitespace
My_polys$name  <- trimws(My_polys$name, "both")
#factorize 
My_polys$name <- as.factor(My_polys$name)

#calculate polygon areas for later analysis and append to new column
My_polys$poly_area <- st_area(My_polys)

#remove original osmdata object
rm(My_boundary)

ggplot(My_polys) +
  geom_sf()

My_buildings <- opq(My_bbox) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

build_polys <- My_buildings$osm_polygons
rm(My_buildings)

#drop unecessary columns
colnames(build_polys)
build_polys <- build_polys %>%
  select(osm_id, geometry)

# take a quick look at what we have so far.
ggplot(My_polys) +
  geom_sf() +
  geom_sf(data = build_polys[1:5000,])

#calculate surface area of buildings
build_polys$area <- sf::st_area(build_polys)
#calculate centroids
build_cents <- sf::st_centroid(build_polys)

#create a shape object out of original bounding polygon
My_bbox_geom <- 
  sfheaders::sf_polygon(as.data.frame(My_bbox),
                        x="V1",
                        y="V2"
  )
#make sure that the projection matches the other data
st_crs(My_bbox_geom) <- 4326

#plot
ggplot(My_polys) +
  geom_sf() +
  geom_sf(data=My_bbox_geom, col="red", fill=NA)

#filtering join with a points df, polygon df
clipped <- st_join(build_cents, My_bbox_geom, join = st_within)

# The column id is created, with a 1 indiciating that a point falls within the polygon. Let’s check the result.
clipped %>%
  filter(id == 1) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data=My_bbox_geom, color = 'red', fill = NA)

# It worked, so now I’ll filter the right points, and then create a new dataframe from the iz_polys dataframe, but with the filtered points.
clipped <- clipped %>%
  filter(id == 1)
joined <- st_join(clipped, My_polys)

#aggregating and summing total building area
density_calc <- aggregate(joined$area, list(joined$osm_id.y),FUN = sum)
#rename columns
colnames(density_calc) <- c("osm_id", "area")

#create final df that contains district polygons and building area
bounds_blds_sf <- merge(My_polys, density_calc) 

#calculate building density
bounds_blds_sf <- bounds_blds_sf %>%
  mutate(b_dens = area/poly_area * 100)
#
bounds_blds_sf$b_dens
#
tmap_mode('plot')
tm_basemap("Stamen.TonerLite") +
  tm_shape(bounds_blds_sf) +
  tm_polygons(col="b_dens",
              id="name",
              title= "Building Density as % of Land Area",
              alpha=.8) 
