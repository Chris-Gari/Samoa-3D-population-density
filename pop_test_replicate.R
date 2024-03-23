library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)

# load kontur data

samoa_data <- st_read("data/kontur_population_WS_20220630.gpkg") # population data
samoa_bounds <- st_read("data/kontur_boundaries_WS_20230628.gpkg") # boundaries


# filter for samoa

samoa <- samoa_bounds |> 
  filter(name_en == "Samoa") |> 
  st_transform(crs = st_crs(samoa_data))

# check with map

samoa |> 
  ggplot() +
  geom_sf()

# do intersection on data to limit kontur to florida

st_samoa <- st_intersection(samoa_data, samoa)

# define aspect ratio based on bounding box

bb <- st_bbox(st_samoa)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(samoa_data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(samoa_data))

# check by plotting points

samoa |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(samoa_data))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

# convert to raster so we can then convert to matrix

size <- 1000

samoa_rast <- st_rasterize(st_samoa, 
                             nx = floor(size * w_ratio),
                             ny = floor(size * h_ratio))

mat <- matrix(samoa_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))

# create color palette

# palette chart https://github.com/BlakeRMills/MetBrewer/blob/main/PaletteImages/Examples/AllPalettes.png

c1 <- met.brewer(name = "Hokusai3", n = 60, direction = c(-1))
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)

# plot that 3d thing!

rgl::close3d()

mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 100 / 5,
          solid = TRUE,
          shadowdepth = 0)

render_camera(theta = -55, phi = 60, zoom = .8)

outfile <- "images/final_plot.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 280,
    lightaltitude = c(20, 80),
    lightcolor = c(c1[2], "white"),
    lightintensity = c(600, 100),
    samples = 450,
    width = 6000,
    height = 6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}


