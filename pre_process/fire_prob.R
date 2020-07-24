##### dealing with fire probability raster file
library(raster)
library(sf)
library(tidyverse)
library(RODBC)

options(scipen = 999)

### read in fire prob raster
pfire <- raster("input/fireprobtif/Pfire_Annual_2026_2050_Pct1.tif")
plot(pfire)
crs(pfire)

pfire_wgs <- st_as_sf(pfire)

## plot using ggplot
pfire_df <- raster::as.data.frame(pfire, xy = T)

names(pfire_df)[3] <- "pfire"
str(pfire_df)

ggplot() +
  geom_raster(data = pfire_df , aes(x = x, y = y, fill = pfire)) +
  scale_fill_viridis_c() +
  coord_quickmap()



#### get lat long data
conn <- odbcConnectAccess2007("input/master_edf.mdb")
plot_m <- sqlFetch(conn, "plot", as.is = T)
odbcCloseAll()


plot_locs <- plot_m %>% 
  select(biosum_plot_id, lat, lon) %>% 
  distinct()

plot_locs_sf <- st_as_sf(plot_locs, coords = c("lon", "lat"), crs = 4326)

### counties data
ca_counties <- read_sf("spatial_data/CA_Counties", layer = "CA_Counties_TIGER2016")
ca_counties_crs <- st_transform(ca_counties, crs = 4326)


### fire prob data

r_sp <- as(pfire, "SpatialPixelsDataFrame")
r_sf <- st_as_sf(r_sp)
r_crs <- st_transform(r_sf, crs = 4326)


## plot
ggplot() +
  geom_sf(data = ca_counties_crs) +
  geom_sf(data = plot_locs_sf) 
  #geom_sf(data = r_crs) 



test <- st_join(plot_locs_sf, r_crs)


coordinates(plot_locs) <- ~ lon + lat
plot(plot_locs)

test2 <- raster::extract(pfire, plot_locs, buffer = 20, df = T)



###############################
### new approach

pfire_poly <- rasterToPolygons(pfire)
