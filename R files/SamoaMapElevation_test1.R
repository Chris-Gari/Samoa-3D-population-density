library(rayshader)

#Here, I load a map with the raster package.
loadzip = tempfile() 
download.file("https://git.gari-homelab.party/CA_Irag/Samoa-Density-Population/raw/branch/main/Samoa-Elevation-Map-768x358.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "Samoa-Elevation-Map-768x358.tif"))
unlink(loadzip)

#And convert it to a matrix:
elmat = raster_to_matrix(localtif)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

#sphere_shade can shift the sun direction:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()

#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  plot_map()

#And we can add a raytraced layer from that sun direction as well:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  plot_map()

#And we can add a raytraced layer from that sun direction as well:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.8) %>%
  plot_map()

#And here we add an ambient occlusion shadow layer, which models 
#lighting from atmospheric scattering:

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_map()

{
elmat %>%
  sphere_shade(texture = "imhof1") %>%
  add_water(detect_water(elmat,  min_area = 10000), color = "mintcream") %>%
  add_shadow(ray_shade(elmat, zscale = 2), 1) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat,
          baseshape = "rectangle",
          solid = TRUE,
          zscale = 9, 
          fov = 0, 
          theta = -55, 
          zoom = 0.8, 
          phi = 45, 
          windowsize = c(1200, 1200),
          background = "green")
# render_clouds(elmat, zscale = 10, start_altitude = 800, end_altitude = 1000,
#                 sun_altitude = 45, attenuation_coef = 2, offset_y = 300,
#                 cloud_cover = 0.40, frequency = 0.02, scale_y=1, fractal_levels = 32, clear_clouds = T)
   
#render_scalebar(limits=c(0, 30, 70),label_unit = "km",position = "W", y=50,
                  #scale_length = c(0.05,1))
#render_compass(position = "E")
render_camera(theta = -55, 
              phi = 45, 
              zoom = .8,
              fov = 0,
              shift_vertical = 0)



Sys.sleep(0.2)
render_snapshot()
}


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
    lightaltitude = 20,
    lightcolor =  "mintcream",
    lightintensity = 100,
    # lightaltitude = 20,
    # lightcolor = "skyblue",
    # lightintensity = 600,
    lightsize = NULL,
    samples = 450 / 4,
    width = 3508 / 4,
    height = 2480/ 4,
    #cache_scene = TRUE
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}

