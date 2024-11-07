######################################
# Based on:
# Mapping travel catchment areas
# Milos Popovic 2023/10/10
######################################

# https://github.com/milos-agathon/isochrone_maps/blob/main/R/main.r

# install.packages("remotes")
# remotes::install_github(
#   "GIScience/openrouteservice-r"
# )
# 
# libs <- c(
#   "tidyverse", "openrouteservice",
#   "sf", "leaflet", "maptiles",
#   "tidyterra"
# )
# 
# installed_libs <- libs %in% rownames(
#   installed.packages()
# )
# 
# if(any(installed_libs == F)){
#   install.packages(
#     libs[!installed_libs]
#   )
# }
# 
# invisible(
#   lapply(
#     libs,
#     library,
#     character.only = T
#   )
# )

library(tidyverse)
library(openrouteservice)
library(mapview)
library(sf)
library(fasterize)
library(leaflet)

# 1. DEFINE MAIN PARAMETERS

openrouteservice::ors_profile()

## coordinates
tor_lat <- 43.75778976617083
tor_lon <- -79.39326677619596

lhr_lat <- 31.502425248570344
lhr_lon <- 74.30409916687087


# 2. QUERY

tor_coords <- data.frame(tor_lon, tor_lat)
lhr_coords <- data.frame(lhr_lon, lhr_lat)

cycling_ams_tor <- openrouteservice::ors_isochrones(
  locations = tor_coords,
  profile = "cycling-regular",
  range = 3600,
  interval = 600,
  api_key = Sys.getenv("ORS_API_KEY"),
  output = "sf"
)

cycling_ams_lhr <- openrouteservice::ors_isochrones(
  locations = lhr_coords,
  profile = "cycling-regular",
  range = 3600,
  interval = 600,
  api_key = Sys.getenv("ORS_API_KEY"),
  output = "sf"
)


# 3. DATA TRANSFORMATION

sf::sf_use_s2(FALSE)

create_cycling_ams_cropped <- function(cycling_ams){
  cycling_ams$mins <- cycling_ams$value / 60
  # cycling_ams$mins <- factor(
  #   cycling_ams$mins
  # )
  
  cycling_ams |>
    dplyr::group_by(mins) |>
    sf::st_intersection() |>
    dplyr::ungroup()
}

cycling_ams_cropped_tor <- create_cycling_ams_cropped(cycling_ams_tor)
cycling_ams_cropped_lhr <- create_cycling_ams_cropped(cycling_ams_lhr)


# 4. INTERACTIVE MAP OF CYCLING CATCHMENT AREA

sf::st_crs(cycling_ams_cropped_tor)$units #NULL
sf::st_crs(cycling_ams_cropped_lhr)$units

# For the City of Toronto, a common choice is the NAD83 / Ontario MNR Lambert (EPSG: 3161) projected CRS, as it’s well-suited for Ontario. This projection provides measurements in meters, which can be more accurate for spatial analysis at the city scale.

# For Lahore, Pakistan, a good choice would be the WGS 84 / UTM zone 43N (EPSG: 32643), as Lahore falls within UTM Zone 43N.

cycling_ams_cropped_tor_proj <- sf::st_transform(cycling_ams_cropped_tor, crs = 3161)
cycling_ams_cropped_lhr_proj <- sf::st_transform(cycling_ams_cropped_lhr, crs = 32643)

sf::st_crs(cycling_ams_cropped_lhr_proj)$units # m



# We then generate a 100m resolution raster using the raster() function. This raster represents a “template raster” object defining the extent, resolution and CRS of the output for fasterize().

template_tor <- raster(cycling_ams_cropped_tor_proj, resolution = 100)
template_lhr <- raster(cycling_ams_cropped_lhr_proj, resolution = 100)

# Then use the fasterize() function to allocate the minimum overlapping value from our isochrones to each grid cell.
iso_surface_tor <- fasterize(cycling_ams_cropped_tor_proj, template_tor, 
                             field = "mins", fun = "min")


# mapview::mapview(iso_surface_tor)

pal_tor <- colorNumeric("viridis", cycling_ams_cropped_tor_proj$mins, na.color = "transparent")


leaflet::leaflet() |>
  leaflet::addProviderTiles(
    "CartoDB.Positron"
  ) |>
  addRasterImage(iso_surface_tor, colors = pal_tor, opacity = 0.5) |> 
  addLegend(
    values = sort(as.numeric(as.character(cycling_ams_cropped_tor$mins))), 
    pal = pal_tor,
    title = "Cycling time in Toronto"
  )


iso_surface_lhr <- fasterize(cycling_ams_cropped_lhr_proj, template_lhr, 
                             field = "mins", fun = "min")


# mapview::mapview(iso_surface_tor)

pal_lhr <- colorNumeric("viridis", cycling_ams_cropped_lhr_proj$mins, na.color = "transparent")


leaflet::leaflet() |>
  leaflet::addProviderTiles(
    "CartoDB.Positron"
  ) |>
  addRasterImage(iso_surface_lhr, colors = pal_lhr, opacity = 0.5) |> 
  addLegend(
    values = cycling_ams_cropped_lhr$mins, 
    pal = pal_lhr,
    title = "Cycling time in Lahore"
  )


# pal_fact <- leaflet::colorFactor(
#   "RdPu",
#   domain = cycling_ams_cropped_tor$mins,
#   reverse = TRUE,
#   na.color = "transparent"
# )
# 
# leaflet::leaflet(
#   cycling_ams_cropped_tor
# ) |>
#   leaflet::addPolygons(
#     fill = TRUE,
#     stroke = TRUE,
#     color = pal_fact,
#     weight = .3,
#     fillColor = ~pal_fact(mins),
#     fillOpacity = .2
#   ) |>
#   leaflet::addProviderTiles(
#     "CartoDB.Positron"
#   ) |>
#   leaflet::addLegend(
#     "bottomright",
#     pal = pal_fact,
#     values = cycling_ams_cropped_tor$mins,
#     labels = cycling_ams_cropped_tor$mins,
#     opacity = .5,
#     title = "Cycling distance in Toronto"
#   )




# 5. STATIC MAP OF CYCLING CATCHMENT AREA
cycling_ams_merc <- sf::st_transform(
  cycling_ams_cropped,
  # 3857
  4326
)

ams_layer <- maptiles::get_tiles(
  cycling_ams_merc,
  provider = "CartoDB.Positron",
  zoom = 11
)


cycling_map <- ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = ams_layer
  ) +
  geom_sf(
    data = cycling_ams_merc,
    aes(
      fill = factor(mins),
      color = factor(mins),
      geometry = geometry
    ),
    size = .2,
    alpha = .5,
    inherit.aes = F
  ) +
  scale_fill_manual(
    name = "Minutes",
    values = hcl.colors(
      6, "RdPu"
    )
  ) +
  scale_color_manual(
    values = hcl.colors(
      6, "RdPu"
    )
  ) +
  guides(
    color = "none",
    fill = guide_legend(
      nrow = 1,
      byrow = T,
      keyheight = unit(5, "mm"),
      keywidth = unit(5, "mm"),
      title.position = "top",
      label.position = "bottom",
      label.hjust = .5
    )
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    plot.margin = unit(
      c(
        t = 0, r = 0,
        b = 0, l = 0
      ), "lines"
    )
  ) +
  labs(
    title = "Cycling distance in Toronto"
  )

# 6. QUERY MULTIPLE TRAVEL MODES

openrouteservice::ors_profile()

travel_modes <- c(
  "foot-walking",
  "cycling-regular",
  "driving-car"
)

travel_list <- c()

for(mode in travel_modes){
  travel_list[[mode]] <- {
    travel_ams <- openrouteservice::ors_isochrones(
      locations = coords,
      profile = mode,
      range = 1800,
      interval = 600,
      api_key = Sys.getenv(ors_api_key),
      output = "sf"
    )
    travel_ams
  }
}

travel_mode_ams <- do.call(
  rbind, travel_list
)

# 7. PANEL MAP OF TRAVEL MODES

travel_mode_ams$mode <- factor(
  rownames(travel_mode_ams),
  labels = c(
    "cycling",
    "driving",
    "walking"
  )
)

travel_ams_merc <- sf::st_transform(
  travel_mode_ams,
  4326
)

travel_map <- ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = ams_layer
  ) +
  geom_sf(
    data = travel_ams_merc,
    aes(
      fill = factor(mode),
      color = factor(mode),
      geometry = geometry
    ),
    size = .2,
    alpha = .5,
    inherit.aes = F
  ) +
  scale_fill_manual(
    name = "Travel mode",
    values = hcl.colors(
      3, "Set 2"
    )
  ) +
  scale_color_manual(
    values = hcl.colors(
      3, "Set 2"
    )
  ) +
  facet_wrap(~mode) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(
      c(
        t = 0, r = 0,
        b = 0, l = 0
      ), "lines"
    ),
    plot.title = element_text(
      size = 14,
      face = "bold",
      color = "grey10",
      hjust = .5
    ),
    strip.text = element_text(
      size = 12,
      color = "grey40",
      hjust = .5
    )
  ) +
  labs(
    title = "Travel distance in Toronto"
  )
