library(ggplot2)
library(dplyr)
library(spdep)
library(eurostat)
library(giscoR)
library(sf)
rm(list = ls())
# Get the spatial data 
options(readr.default_locale=readr::locale(tz="Europe/Berlin"))
map1 <- giscoR::gisco_get_countries()
map2 <- st_crop(map1, xmin = -10, ymin = 35.5, xmax = 29.5, ymax = 61.5)
map2 <- map2 %>% 
  select(CNTR_ID)
# Countries to display neighborhood structure
my_states <- data.frame(State=c("AT","BE","CZ","DK","FR",
                                "DE","HU","IT","LT","LU",
                                "NL","PL","SK","SI","CH"),
                        NB_Level=c(1,2,0,2,2,1,2,2,2,2,2,1,1,2,2))
#
my_states <- my_states %>% 
  left_join(map2,by = c("State"="CNTR_ID")) %>% 
  st_as_sf() %>% 
  select(State,NB_Level)
#
# Centroids (just to visualize connections)
My_centroids <- sf::st_centroid(my_states)
My_coords <- sf::st_coordinates(My_centroids)
colnames(My_coords) <- c("long","lat")
My_centroids$NB_order <- as.factor(My_centroids$NB_Level)
#
# Neigborhoods
NB_contig <- poly2nb(my_states, snap = 0.04)
NB_contig
#
neighbors_sf <- as(nb2lines(NB_contig, coords = My_centroids$geometry), 'sf') 
#
ggplot() + 
  geom_sf(data=map2, fill = 'lightgrey', color = 'darkgrey') +
  geom_sf(data=my_states, fill="lightblue", color = "darkgrey") +
  geom_sf(data = neighbors_sf, linewidth = 0.2) +
  geom_sf(data=My_centroids,aes(color = NB_order),size=3) +
  theme_minimal() +
  ylab("Latitude") +
  xlab("Longitude")

