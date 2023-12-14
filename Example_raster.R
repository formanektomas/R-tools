library(sf)
library(exactextractr)
library(RColorBrewer)
library(RCzechia)
library(terra)
library(dplyr)
library(ggplot2)
library(ggthemes)
rm(list = ls())
#
# Step 1: Download a pre-processed image from Copernicus
# example is for buil-up area but can be used for air pollution, etc.
#
# https://lcviewer.vito.be/download , select Built-up , 2019 
# & save file as "2019.tif in your working directory
# .. for the following code to work, data for Central Europe have to be downloaded. 
#
downloaded_raster <- "2019.tif"
year_2019 <- terra::rast(downloaded_raster)
# base plot
plot(year_2019)
# highest res adm. units, makes more sense than rect./hex. grids
# for your MT, you'll probably work with NUTS2-level regions, ideally across more countries
CZ <- RCzechia::republika(resolution = "low") # whole CR
counties <- RCzechia::okresy(resolution = "low")
# plot(counties[,1]) # NAZ_LAU1 has the name of the county (LAU1)
cropped <- terra::crop(year_2019,CZ)
masked <- terra::mask(cropped,CZ)
plot(masked)
plot(counties$geometry, add = T)
#-----
#
#
# calculate average buitup area (proportion) at county level
# can be done similarly for NUTS2, etc.
#
counties$builtup <- exactextractr::exact_extract(
  x = year_2019, # source
  y = counties, # target
  fun = "mean", # calculation of average built-up - can be used for air pollution, etc.
  weights = "area"
) 
# Prague:    ObecKod == "554782"

ggplot(data = counties, aes(fill = builtup, label = round(builtup))) +
  geom_sf(lwd = 1/3) +
  scale_fill_gradientn('PPS/Hab', colours=brewer.pal(9, "PuRd"))+
  labs(title = "Build-up relative area as of 2019 (latest data)",
       caption = " (c) Copernicus Service Information 2019") +
  theme_economist()
# ---------------------------------------
#
# more detailed grid of observations
#
CZcartesian <- st_transform(CZ,3857)
grid1 <- st_make_grid(CZcartesian, square = F, n = c(80,40))
plot(grid1)
grid1 <- st_transform(grid1,4326) # back to WGS 84
plot(grid1)
CZgrid <- st_intersection(grid1,CZ)
plot(CZgrid)
CZpolygons <- st_as_sf(CZgrid)
CZpolygons$PolID <- 1:nrow(CZpolygons)
CZpolygons <- st_cast(CZpolygons[1:2634,], "POLYGON") # remove point at the end, remove hexagons cut into pieces at intersection with boundary (keep one part only)
CZpolygons <- as(CZpolygons,"Spatial") # fix sf structure
CZpolygons <- st_as_sf(CZpolygons) # fix sf structure of data.frame
plot(CZpolygons) # 2634 hexagons instead of 77 LAU1 regions
#
# built-up for each hexagon
CZpolygons$builtup <- exactextractr::exact_extract(
  x = year_2019, # source
  y = CZpolygons, # target
  fun = "mean", # calculation of average built-up - can be used for air pollution, etc.
  weights = "area"
) 
ggplot(data = CZpolygons, aes(fill = builtup, label = round(builtup))) +
  geom_sf(lwd = 1/3) +
  labs(title = "Build-up relative area as of 2019 (latest data)",
       caption = " (c) Copernicus Service Information 2019") +
  scale_fill_gradientn('PPS/Hab', colours=brewer.pal(9, "PuRd"))+
  theme_economist()
dev.off()
plot(masked) # previous plot for comparison
#
#-----
# https://maps.s5p-pal.com/no2/
# download NO2 pollution image data using the icon on lower-right of the map
# change name of the NetCDF to some simple name, eg. "NO2data.nc"
# Note: File has approx. 1GB
rNO2 <- terra::rast("NO2data.nc")
rNO2
plot(rNO2)
plot(rNO2[[1]])
#
cropped <- terra::crop(rNO2[[1]],CZ)
masked <- terra::mask(cropped,CZ)
plot(masked)
plot(counties$geometry, add = T)
#
CZpolygons$NO2 <- exactextractr::exact_extract(
  x = masked, # source
  y = CZpolygons, # target
  fun = "mean", # calculation of average built-up - can be used for air pollution, etc.
  weights = "area"
) 
ggplot(data = CZpolygons, aes(fill = NO2, label = round(NO2))) +
  geom_sf(lwd = 1/3) +
  labs(title = "NO2 air pollution (DATE)",
       caption = " (c) Copernicus Service Information 2019") +
  scale_fill_gradientn('NO2', colours=brewer.pal(9, "PuRd"))+
  theme_economist()
#
#-----
library("corrplot")
library("Hmisc")
res <- CZpolygons %>% 
  st_drop_geometry() %>% 
  select(builtup,NO2) %>% 
  tidyr::drop_na() %>% 
  as.matrix() %>% 
  rcorr()
res
# not very interesting for two variables, but you can insert more than builtup and NO2...
corrplot(res$r,
         method = 'circle',
         p.mat = res$P,
         sig.level = 0.01, # select sig. level to show in plot
         insig = 'blank')
#
summary(lm(NO2~builtup, CZpolygons))
#
#
#----
