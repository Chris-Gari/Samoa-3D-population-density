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

size <- 3000

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
{
rgl::close3d()


mat |> 
  height_shade(texture = texture) |>
  #add_water(detect_water(mat), color = "pink") %>% 
  #sphere_shade(texture = "desert") %>%
  plot_3d(heightmap = mat,
          phi = 45,
          zscale = 100 / 10,
          #solid = TRUE,
          soliddepth = 0,
          shadowdepth = 0,
          soil = TRUE,
          soil_levels = length(c1),
          soil_color_light = c1[40],
          soil_color_dark = c1[1],
          soil_gradient = 4,
          soil_gradient_darken = 0.6,
         # shadow = TRUE,
          #water = TRUE,
          #soil = FALSE,
          #background = "white",
          windowsize = c(600, 600),
          #asp = 1
         )

render_camera(theta = -55, 
              phi = 45, 
              zoom = .8,
              fov = 0,
              shift_vertical = 0)
}

render_snapshot(title_text = "Samoa Population Density, 2020",
                title_color = "white",
                title_bar_color = "blue",
                vignette = TRUE)

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
    lightdirection = 80,
    lightaltitude = c(20, 80),
    lightcolor = c(c1[27], "skyblue"),
    lightintensity = c(600, 100),
    # lightaltitude = 20,
    # lightcolor = "skyblue",
    # lightintensity = 600,
    lightsize = NULL,
    samples = 450,
    width = 3508,
    height = 2480,
    #cache_scene = TRUE
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}


