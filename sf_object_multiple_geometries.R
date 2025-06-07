library(sf)

# Example: polygon and point
polygon_geom <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))
point_geom <- st_sfc(st_point(c(0.5, 0.5)))

# Create sf object with polygon as main geometry
sf_obj <- st_sf(name = "Building A", geometry = polygon_geom)

# Add entrance point as a separate geometry column
sf_obj$entrance <- point_geom

# View structure
print(sf_obj)
#
# You can store more than one geometry in an sf object by keeping one as the active geometry and others as regular list-columns of class sfc (simple feature column). 
