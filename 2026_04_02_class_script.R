# 2026-04-02
# JEL

library(tidyverse)
# install.packages("raster")
library(raster)
# install.packages("mapdata")
library(mapdata)
# install.packages("marmap")
library(marmap)

# load the data
# July 2002 - 2025
chl_raster = raster("data/AQUA_MODIS.20020701_20250731.L3m.MC.CHL.chlor_a.9km.nc")
class(chl_raster)
chl_raster

names(chl_raster) = "chl_a"

# this gives class "SpatialPointsDataFrame"
chl_pts = raster::rasterToPoints(chl_raster, spatial = T)
class(chl_pts)

# we want just a data frame
chl_df = data.frame(chl_pts)
summary(chl_df)

# map it !!!! (ugly first) 

global_chl_map = ggplot(data = chl_df) +
  geom_raster(aes(x = x, y = y, fill = chl_a))

ggsave(global_chl_map, filename = "figures/ugly_chl_map.png", height = 5, width = 9)
# this map is ugly bc we did not add coast lines and the color bar is not working
# chlorophyll is typically viewed on a log scale

# histogram of chl data
# lots of data points are really low so hard to visualize
hist(chl_df$chl_a)

# chl is usually displayed as log 10
# this histogram is alot nicer and normalized
hist(log10(chl_df$chl_a))

# create new colors for color bar
# NASA uses reverse rainbow with blue meaning low and red meaning high
cols = rainbow(n = 7)

# to reverse the rainbow to do this:
# [-1] removes purple bc we are taking away the first color from the reverse order
cols = rainbow(n = 7, rev = T)[-1]

global_chl_map_log = ggplot(data = chl_df) +
  geom_raster(aes(x = x, y = y, fill = log10(chl_a))) +
  scale_fill_gradientn(colors = cols, 
                      limits = c(-1.75, 0.75),
                      oob = scales::squish,
                      name = "log_10(chl_a)")

ggsave(global_chl_map_log, filename = "figures/log_chl_map.png", height = 5, width = 9)

# Zoom into the Gulf of Maine, Erin's whales feed here ish

lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

chl_GoM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds)))
chl_GoM_raster
chl_GoM_df = data.frame(rasterToPoints(chl_GoM_raster, spatial = T))
head(chl_GoM_df)

world_map = map_data("worldHires")
head(world_map)

GoM_chl_map = ggplot() +
  geom_raster(data = chl_GoM_df, aes(x = x, y = y, fill = log10(chl_a))) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black") +
  coord_fixed(ratio = 1.3, xlim = lon_bounds, y = lat_bounds, expand = F) + 
  scale_fill_gradientn(colors = cols, 
                      limits = c(-1, 1.75),
                      oob = scales::squish,
                      name = "log_10(chl_a)") +
  theme_bw()

ggsave(GoM_chl_map, filename = "figures/GoM_chl_map.png", height = 5, width = 9)


########## BATHYMETRY !!!!!!!!

lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], lon2 = lon_bounds[2], 
                                   lat1 = lat_bounds[1], lat2 = lat_bounds[2])
class(bath_m_raw)

bath_m_df = marmap::fortify.bathy(bath_m_raw)
head(bath_m_df)
summary(bath_m_df)

# get rid of elevation from land

bath_m = bath_m_df %>%
  mutate(depth_m = ifelse(z > 20, NA, z)) %>%
  dplyr::select(-z)

head(bath_m)
summary(bath_m)

GOM_bath_map = ggplot() +
  geom_raster(data = bath_m, aes(x = x, y = y, fill = depth_m)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black") +
  coord_fixed(ratio = 1.3, xlim = lon_bounds, ylim = lat_bounds, expand = F) + 
  theme_bw()
ggsave(GOM_bath_map, filename = "figures/GOM_bath_map1.png", height = 5, width = 9)
