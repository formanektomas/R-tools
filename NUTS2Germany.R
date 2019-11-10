## Maps and choropleths with {ggplot2}
#
library(dplyr)
library(eurostat)
library(sf)
library(ggplot2)
library(RColorBrewer)
rm(list = ls())
#
#
#### Simple maps with {ggplot2}
#
# Get the spatial data for NUTS regions in {sf} format
options(readr.default_locale=readr::locale(tz="Europe/Berlin"))
df60 <- eurostat::get_eurostat_geospatial(resolution = 60)
#
#
# Example of map for Germany - NUTS 2
Germany60 <- df60 %>%   
  dplyr::filter(LEVL_CODE == 2 & CNTR_CODE %in% ("DE")) %>% 
  dplyr::select(NUTS_ID)
# ggplot2 map
ggplot() + 
  geom_sf(data = Germany60)
#
GE.sp <- as_Spatial(Germany60)
#
library(rgeos) # install.packages("rgeos")
library(rgdal) # install.packages("rgdal")
#
######################################################
#
#
# Centroids - representative points for areal regions
#
Centroids <- as.data.frame(as.character(GE.sp$NUTS_ID))
Centroids <- cbind(Centroids, coordinates(GE.sp))
colnames(Centroids) <- c("NUTS_ID","long","lat")
Centroids[Centroids$NUTS_ID=="DE40",2:3] <- c(13.397932, 53) # amend for name overlap
#
ggplot() + 
  geom_sf(data = Germany60)+
  geom_text(data = Centroids, aes(label = NUTS_ID, x = long, y = lat, group=NUTS_ID), size = 2.5) + 
  # ggtitle("NUTS2 regions for selected countries: AT, CZ, DE, HU, PL, SK") +
  theme_minimal()
#
