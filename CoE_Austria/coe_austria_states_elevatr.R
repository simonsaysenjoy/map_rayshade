#########################################
## Austrian States DEM Map
##
#########################################
# Author: Simon Landauer
# Last Update: 2022-01-15
#########################################
# Description:
# The following code is an example how to construct a shaded elevation map with 
# official state flag colors for Austria. 
# The code aims to inspire - but can and should be adjusted as needed. 
#########################################

## _____________________
## ----- Libraries -----
# Standard
library(dplyr)
# Geodata
library(elevatr)
library(raster)
library(sf)
# Plotting
library(ggplot2)
library(rayshader)

# set working directory
setwd(".")

## __________________________________________
## ----- Loading Data -----
rm(list = ls())

## ----- State Borders -----
# (I use the official state borders from OGD - but you can obviously also other
# databases to pull vector data directly into )

# Get the Austrian Dataset here: 
# https://www.data.gv.at/katalog/dataset/verwaltungsgrenzen-vgd-stichtagsdaten-grundstucksgenau#resources

# !!! Get the Data first !!!
# Download VGD_Oesterreich_gst_20220403.zip - and extract it into the ./data folder

states <- st_read("./data/VGD.shp")
states <- states %>% group_by(BL) %>% summarise

# For Central European maps I use ETRS89 - EPSG4258
states <- st_transform(states, st_crs("EPSG:4258"))
ggplot(data = states) +
  geom_sf(aes(fill = BL)) +
  geom_sf_label(aes(label = BL))

# Calculate country outline for faster DEM download (delete later)
country <- states %>% summarize

## ----- State Borders -----
# Choose resolution for elevation data pulled from elevatr (1-14)
z_elevatr <- 9

# DEM aquisition
dem <-  get_elev_raster(country,
                        z = z_elevatr, 
                        clip = "locations")


# In case it is not projected correctly
dem <- projectRaster(dem, crs = CRS(SRS_string = "EPSG:4258"))
plot(dem)

# Check approximate resolution for raster in meters
latcenter <- yFromRow(dem, nrow(dem)/2)
res <- (cos(latcenter * pi/180) * 2 * pi * 6378137) / (256 * 2^z_elevatr)
print(sprintf("Resolution for DEM is ~ %01.1f m", res))

rm(country)

## __________________________________________
## ----- Data Pre-processing -----

## Covnert to matrix for rayshader
mat_aut <- raster_to_matrix(dem)

## ----- States -----
# Get prepared file with HEX codes and accorind state names 
# State names need to correspond to the ones used in the SHP
def_states <- read.csv("./data/aut_flag_colors.csv", sep = ",",
                       encoding="UTF-8", row.names = NULL)

# Calculate overlay for each state (with corresponding color - order!)
for (s in 1:nrow(def_states)) {
  name_s <- def_states[s, "Bundesland"]
  col_s <- def_states[s, "HEX"]
  col_s <- unlist(strsplit(col_s, " - "))
  print(paste("Processing", name_s, "..."))
  
  # subset (mask and matrix)
  sub <- subset(states, BL == name_s)
  tmp1 <- mask(dem, sub)
  tmp2 <- raster_to_matrix(tmp1)
  
  if (s == 1) {
    ov <- height_shade(tmp2, texture = (grDevices::colorRampPalette(col_s))(256))
  } else {
    ov <- ov - 1 + height_shade(tmp2, texture = (grDevices::colorRampPalette(col_s))(256))
  }
}

# Remove unnecessary datasets
rm(sub, tmp1, tmp2, col_s, name_s, s)
rm(dem, states, def_states, z_elevatr, latcenter)

# Clear all plots
dev.off(dev.list()["RStudioGD"]) 

# Collect garbage
gc()

## __________________________________________
## ----- Render Preparation -----

# Windowsize
w <- nrow(mat_aut)
h <- ncol(mat_aut)
# Dimension scales
wr <- w / max(c(w,h))
hr <- h / max(c(w,h))


amet <- "blend"
adeg <- 0.85

# RGL
# Experiment with parameters
mat_aut %>% 
  sphere_shade(sunangle = 45, texture = "bw") %>% 
  add_overlay(ov, alphamethod = amet, alphalayer = adeg) %>%
  plot_3d(mat_aut,
          windowsize = c(1000*wr, 1000*hr),
          solid = TRUE,
          solidcolor = "grey80",
          solidlinecolor = "grey80",
          zscale = res / 4,
          phi = 55,
          zoom = 0.45,
          theta = 0)

# In case of camera adjustment
render_camera(
  theta = 0,
  phi = 55,
  zoom = 0.45)

## __________________________________________
## ----- Render -----
# Render the Image
img_name <- sprintf("aut_states_col_%1.0fm", round(res, digits = -2))

if (!dir.exists("images")) {
  dir.create("images")
  print("Directory 'images' created")
}

# Render with HDR
{
  start_time <- Sys.time()
  print("Start rendering:")
  print(start_time, "\n")
  Sys.sleep(2)
  render_highquality(
    filename = paste0("images/", img_name, "_hdr", ".png"),
    samples=256,
    parallel = TRUE,
    light = FALSE,
    interactive = FALSE,
    environment_light="./hdr/phalzer_forest_01_4k.hdr",
    intensity_env = 1.5,
    rotate_env = 270,
    cache_filename = paste0("images/", "tmp_", img_name),
    width = round(3000 * wr),
    height = round(3000 * hr)
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  print("...Done.")
  print(diff)
}
