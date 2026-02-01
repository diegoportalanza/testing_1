# libraries
pacman::p_load(
  osmdata, sf, dplyr, rayrender
)

# AOI (Guayaquil, Ecuador - Centro urbano, lejos del río)
# Zona de Urdesa/Kennedy/Alborada - áreas más densas tierra adentro
lon <- -79.9100
lat <- -2.1700

ctr_wgs <- sf::st_sfc(
  sf::st_point(
    c(lon, lat)
  ), crs = 4326
)

utm_epsg <- function(lon, lat) {
  stopifnot(
    is.finite(lon), is.finite(lat)
  )
  if (lat >= 84) {
    return(32661)
  } # UPS North
  if (lat <= -80) {
    return(32761)
  } # UPS South
  zone <- floor((lon + 180) / 6) + 1
  if (lat >= 0) 32600 + zone else 32700 + zone
}

crs_m <- utm_epsg(lon, lat)
ctr_m <- sf::st_transform(ctr_wgs, crs_m)
radius <- 2000  # De vuelta a 2000m ahora que evitamos el río
aoi_m <- sf::st_buffer(ctr_m, radius)
aoi_wgs <- sf::st_transform(aoi_m, 4326)
bb <- sf::st_bbox(aoi_wgs)

# OSM pulls (helpers with retry logic)
get_polys <- function(q, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch({
      Sys.sleep(2)  # Wait 2 seconds between requests
      x <- osmdata::osmdata_sf(q)$osm_polygons
      if (is.null(x)) {
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      } else {
        return(sf::st_make_valid(x))
      }
    }, error = function(e) {
      if (i == max_retries) {
        cat("Failed after", max_retries, "attempts:", conditionMessage(e), "\n")
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }
      cat("Attempt", i, "failed, retrying...\n")
      Sys.sleep(5)  # Wait longer before retry
    })
  }
}

get_lines <- function(q, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch({
      Sys.sleep(2)  # Wait 2 seconds between requests
      x <- osmdata::osmdata_sf(q)$osm_lines
      if (is.null(x)) {
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      } else {
        return(sf::st_make_valid(x))
      }
    }, error = function(e) {
      if (i == max_retries) {
        cat("Failed after", max_retries, "attempts:", conditionMessage(e), "\n")
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }
      cat("Attempt", i, "failed, retrying...\n")
      Sys.sleep(5)  # Wait longer before retry
    })
  }
}

# Set a different Overpass server (try multiple if needed)
options(overpass_url = "https://overpass-api.de/api/interpreter")

cat("Fetching buildings...\n")
bld <- get_polys(osmdata::opq(bb, timeout = 180) |>
                   osmdata::add_osm_feature("building"))

cat("Fetching landuse...\n")
landuse <- get_polys(osmdata::opq(bb, timeout = 180) |>
                       osmdata::add_osm_feature("landuse"))

# keep green landuse types
green_cover <- subset(
  landuse, landuse %in% c(
    "grass", "recreation_ground",
    "forest", "greenery", "meadow", "village_green"
  )
)

cat("Fetching parks...\n")
parks <- dplyr::bind_rows(
  green_cover,
  get_polys(osmdata::opq(bb, timeout = 180) |>
              osmdata::add_osm_feature(
                "leisure", "park"
              )),
  get_polys(osmdata::opq(bb, timeout = 180) |>
              osmdata::add_osm_feature(
                "natural", c("wood", "scrub")
              ))
)

cat("Fetching water...\n")
water <- dplyr::bind_rows(
  get_polys(osmdata::opq(bb, timeout = 180) |>
              osmdata::add_osm_feature(
                "natural", "water"
              )),
  get_polys(osmdata::opq(bb, timeout = 180) |>
              osmdata::add_osm_feature(
                "waterway", "river"
              ))
)

cat("Fetching roads...\n")
roads <- get_lines(osmdata::opq(bb, timeout = 180) |>
                     osmdata::add_osm_feature(
                       "highway"
                     ))

cat("Fetching railways...\n")
rails <- get_lines(osmdata::opq(bb, timeout = 180) |>
                     osmdata::add_osm_feature(
                       "railway"
                     ))

# Clip to circle & project to meters
clip_m <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(
      sf::st_sf(
        geometry = sf::st_sfc(
          crs = crs_m
        )
      )
    )
  }
  # Clip to the CIRCULAR aoi_wgs
  x <- suppressWarnings(
    sf::st_intersection(x, aoi_wgs)
  )
  if (nrow(x) == 0) {
    return(
      sf::st_sf(
        geometry = sf::st_sfc(
          crs = crs_m
        )
      )
    )
  }
  sf::st_transform(x, crs_m)
}

cat("Clipping and projecting data...\n")
bld <- clip_m(bld)
parks <- clip_m(parks)
landuse <- clip_m(landuse)
water <- clip_m(water)
roads <- clip_m(roads)
rails <- clip_m(rails)

cat(sprintf("Found %d buildings, %d parks, %d roads\n", nrow(bld), nrow(parks), nrow(roads)))

# Heights & line buffers - MEJORADO para más variedad
if (nrow(bld) > 0) {
  h_raw <- suppressWarnings(
    as.numeric(gsub(",", ".", bld$height))
  )
  levraw <- suppressWarnings(
    as.numeric(gsub(",", ".", bld$`building:levels`))
  )

  # Altura por defecto aumentada a 18m (más realista para edificios urbanos)
  # Y agregamos algo de variación aleatoria para edificios sin datos
  bld$h <- ifelse(!is.na(h_raw), h_raw,
                  ifelse(!is.na(levraw), pmax(levraw, 1) * 3.5,
                         runif(nrow(bld), 12, 24)))  # Variación entre 12-24m

  bld$h <- pmin(bld$h, 120)  # Límite aumentado a 120m
}

roads_buf <- if (nrow(roads) > 0) {
  sf::st_buffer(roads, 4)  # Aumentado de 3 a 4
} else {
  sf::st_sf(geometry = sf::st_sfc(crs = crs_m))
}

rails_buf <- if (nrow(rails) > 0) {
  sf::st_buffer(rails, 3)  # Aumentado de 2 a 3
} else {
  sf::st_sf(geometry = sf::st_sfc(crs = crs_m))
}

# thin "crown" highlight down the middle of roads
roads_crown <- if (nrow(roads) > 0) {
  sf::st_buffer(roads, 1.8)  # Aumentado de 1.3 a 1.8
} else {
  sf::st_sf(geometry = sf::st_sfc(crs = crs_m))
}

# Recenter near origin
center_xy <- sf::st_coordinates(ctr_m)[1, 1:2]
recenter <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(x)
  }
  sf::st_geometry(x) <- sf::st_geometry(x) - center_xy
  x
}

bld <- recenter(bld)
landuse <- recenter(landuse)
parks <- recenter(parks)
water <- recenter(water)
roads_buf <- recenter(roads_buf)
rails_buf <- recenter(rails_buf)
roads_crown <- recenter(roads_crown)

# Add a circular base plate to visualize the boundary
circle_base <- sf::st_transform(aoi_m, crs_m)
circle_base <- recenter(sf::st_sf(geometry = sf::st_geometry(circle_base)))

# Palette/Materials - Colores mejorados para más contraste

col_bld_low <- "#D4A574"    # Más claro para edificios bajos
col_bld_mid <- "#B8884A"    # Color medio
col_bld_high <-"#8B6030"    # Más oscuro para edificios altos
col_landuse <- "#D4C497"    # Landuse más claro
col_park <- "#7CB342"       # Verde más vivo para parques
col_road <- "#8895A3"       # Caminos
col_road_hi <- "#F5F7FA"    # Highlight de caminos
col_water <- "#4FC3F7"      # Agua más brillante
col_base_hi <- "#FBFCFE"
col_base_lo <- "#E0E5EA"    # Base un poco más oscura

mat_landuse <- rayrender::diffuse(col_landuse)
mat_park <- rayrender::diffuse(col_park)
mat_road <- rayrender::diffuse(col_road)
mat_road_hi <- rayrender::diffuse(col_road_hi)
mat_water <- rayrender::metal(
  color = col_water, fuzz = 0.4
)
mat_base <- rayrender::diffuse(col_base_lo)

# Build scene objects
cat("Building 3D scene...\n")
objs <- list()

# Start with circular base
objs <- append(
  objs, list(
    rayrender::extruded_polygon(
      circle_base,
      top = 0.01,
      bottom = -1.0,  # Base más gruesa
      material = mat_base
    )
  )
)

# landuse/parks/water/roads
if (nrow(landuse) > 0) {
  objs <- append(
    objs, list(
      rayrender::extruded_polygon(
        landuse,
        top = 0.8,
        bottom = 0.02,
        material = mat_landuse
      )
    )
  )
}

if (nrow(parks) > 0) {
  objs <- append(
    objs, list(
      rayrender::extruded_polygon(
        parks,
        top = 1.0,  # Parques un poco más elevados
        bottom = 0.02,
        material = mat_park
      )
    )
  )
}

if (nrow(water) > 0) {
  objs <- append(
    objs, list(
      rayrender::extruded_polygon(
        water,
        top = 0.5,
        bottom = -1.5,  # Agua más profunda
        material = mat_water
      )
    )
  )
}

# Combine roads and rails buffers safely
if (nrow(roads_buf) > 0 && nrow(rails_buf) > 0) {
  rr <- dplyr::bind_rows(roads_buf, rails_buf)
} else if (nrow(roads_buf) > 0) {
  rr <- roads_buf
} else if (nrow(rails_buf) > 0) {
  rr <- rails_buf
} else {
  rr <- sf::st_sf(geometry = sf::st_sfc(crs = crs_m))
}

if (nrow(rr) > 0) {
  objs <- append(
    objs, list(
      rayrender::extruded_polygon(
        rr,
        top = 1.2,
        bottom = 0.02,
        material = mat_road
      )
    )
  )
}

if (nrow(roads_crown) > 0) {
  objs <- append(
    objs, list(
      rayrender::extruded_polygon(
        roads_crown,
        top = 1.35,
        bottom = 1.25,
        material = mat_road_hi
      )
    )
  )
}

# Building color by height bins - Más bins para más variedad
bin_buildings <- function(bld, use_quantiles = TRUE) {  # Cambiado a TRUE
  bld <- bld[!is.na(bld$h) & is.finite(bld$h) & bld$h > 0, ]
  if (nrow(bld) == 0) {
    return(bld)
  }
  if (use_quantiles) {
    qs <- stats::quantile(
      bld$h, c(1 / 3, 2 / 3),
      na.rm = TRUE
    )
    brks <- c(-Inf, qs, Inf)
  } else {
    brks <- c(-Inf, 15, 30, Inf)  # Ajustado para nuevas alturas
  }
  bld$bin <- cut(
    bld$h, breaks = brks, labels = c(
      "low", "mid", "high"
    ), include.lowest = TRUE, right = TRUE
  )
  bld
}

bld <- bin_buildings(bld, use_quantiles = TRUE)
pal <- c(
  low = col_bld_low, mid = col_bld_mid,
  high = col_bld_high
)
mat_map <- setNames(lapply(pal, rayrender::diffuse),
                    names(pal))

add_buildings <- function(scene, b) {
  if (is.null(b) || nrow(b) == 0) {
    return(scene)
  }
  for(lev in levels(b$bin)) {
    sel <- b[b$bin == lev, ]
    if (nrow(sel)) {
      scene <- rayrender::add_object(
        scene,
        rayrender::extruded_polygon(
          sel, data_column_top = "h",
          scale_data = 1,
          material = mat_map[[lev]]
        )
      )
    }
  }
  scene
}

# build scene and add buildings
if (length(objs) == 0) {
  stop("No objects to render - check OSM data availability")
}

scene <- objs[[1]]
if (length(objs) > 1) for (i in 2:length(objs))
  scene <- rayrender::add_object(
    scene, objs[[i]]
  )
scene <- add_buildings(scene, bld)

# Camera: Centrada perfectamente para ver todo el círculo
lookfrom <- c(-3500, 2500, -3500)  # Posición de la cámara
lookat <- c(0, 0, 0)  # Mirando exactamente al centro (0,0,0)

# Lighting mejorada - Dos luces para mejor iluminación
scene <- rayrender::add_object(
  scene,
  rayrender::sphere(
    x = -2500, y = 3500,
    z = -2500, radius = 600,
    material = rayrender::light(
      intensity = 20
    )
  )
)

# Luz secundaria para reducir sombras
scene <- rayrender::add_object(
  scene,
  rayrender::sphere(
    x = 2500, y = 3500,
    z = 2500, radius = 400,
    material = rayrender::light(
      intensity = 10
    )
  )
)

# Render
cat("Rendering scene (this may take several minutes)...\n")
rayrender::render_scene(
  scene = scene,
  lookfrom = lookfrom,
  lookat = lookat,
  fov = 45,  # FOV ligeramente más amplio para capturar todo el círculo
  width = 2400, height = 2400,  # Mayor resolución
  samples = 150,  # Más samples para mejor calidad
  sample_method = "sobol",
  aperture = 0,
  denoise = TRUE,
  ambient_light = TRUE,
  clamp_value = 1,
  min_variance = 1e-15,
  backgroundlow = "#FFFFFF",
  parallel = TRUE,
  interactive = FALSE,
  filename = "guayaquil_detallado.png"
)

cat("Done! Image saved as guayaquil_detallado.png\n")
