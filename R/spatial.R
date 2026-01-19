#' Clip a CMIP6 NEXT GGPD NetCDF file to a shapefile
#'
#' Use a polygon shapefile to crop and mask a daily NetCDF dataset.
#'
#' @param nc_path Path to a NetCDF file.
#' @param shape_path Path to a polygon shapefile or GeoPackage.
#' @param out_path Output path for the clipped NetCDF.
#' @param var_name Optional NetCDF variable name to load.
#' @param overwrite Logical indicating whether to overwrite the output.
#'
#' @return The output path, invisibly.
#' @export
clip_ggpd_to_shape <- function(nc_path,
                               shape_path,
                               out_path,
                               var_name = NULL,
                               overwrite = FALSE) {
  if (missing(nc_path) || missing(shape_path) || missing(out_path)) {
    stop("nc_path, shape_path, and out_path are required.")
  }

  if (file.exists(out_path) && !overwrite) {
    stop("Output already exists. Set overwrite = TRUE to replace it.")
  }

  raster <- if (is.null(var_name)) {
    terra::rast(nc_path)
  } else {
    terra::rast(nc_path, varname = var_name)
  }

  shape <- sf::st_read(shape_path, quiet = TRUE)
  vect <- terra::vect(shape)

  clipped <- terra::crop(raster, vect)
  masked <- terra::mask(clipped, vect)

  terra::writeRaster(masked, out_path, overwrite = overwrite)
  invisible(out_path)
}

#' Calculate raster-based extreme indices by year
#'
#' Compute yearly extreme indices for a daily raster time series.
#'
#' @param nc_path Path to a daily NetCDF file.
#' @param var_name Variable name inside the NetCDF.
#' @param index Index name: one of "txx", "tnn", "rx1day", "r10mm", or "r95p".
#' @param shape_path Optional path to a polygon shapefile for clipping.
#'
#' @return A SpatRaster with one layer per year.
#' @export
calculate_extreme_indices_raster <- function(nc_path,
                                             var_name,
                                             index = c("txx", "tnn", "rx1day", "r10mm", "r95p"),
                                             shape_path = NULL) {
  index <- match.arg(index)

  raster <- terra::rast(nc_path, varname = var_name)
  if (!is.null(shape_path)) {
    shape <- sf::st_read(shape_path, quiet = TRUE)
    vect <- terra::vect(shape)
    raster <- terra::mask(terra::crop(raster, vect), vect)
  }

  times <- terra::time(raster)
  if (is.null(times)) {
    stop("NetCDF time information is required for yearly indices.")
  }

  years <- format(times, "%Y")
  unique_years <- unique(years)

  threshold <- NULL
  if (index == "r95p") {
    threshold <- terra::app(raster, function(x) stats::quantile(x, 0.95, na.rm = TRUE))
  }

  yearly_layers <- lapply(unique_years, function(year_value) {
    layers <- which(years == year_value)
    raster_year <- raster[[layers]]

    if (index %in% c("txx", "rx1day")) {
      terra::app(raster_year, max, na.rm = TRUE)
    } else if (index == "tnn") {
      terra::app(raster_year, min, na.rm = TRUE)
    } else if (index == "r10mm") {
      terra::app(raster_year, function(x) sum(x >= 10, na.rm = TRUE))
    } else {
      exceed <- raster_year * (raster_year > threshold)
      terra::app(exceed, sum, na.rm = TRUE)
    }
  })

  output <- terra::rast(yearly_layers)
  names(output) <- unique_years
  output
}

#' Plot an extreme index map
#'
#' @param index_raster A SpatRaster produced by [calculate_extreme_indices_raster()].
#' @param year Optional numeric year or layer name to plot.
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_extreme_index_map <- function(index_raster, year = NULL, title = NULL) {
  if (!inherits(index_raster, "SpatRaster")) {
    stop("index_raster must be a terra SpatRaster.")
  }

  layer <- index_raster
  if (!is.null(year)) {
    year_label <- as.character(year)
    if (year_label %in% names(index_raster)) {
      layer <- index_raster[[year_label]]
    } else {
      stop("Requested year not found in raster layer names.")
    }
  } else if (terra::nlyr(index_raster) > 1) {
    layer <- index_raster[[1]]
  }

  data <- terra::as.data.frame(layer, xy = TRUE, na.rm = TRUE)
  value_col <- names(data)[3]

  ggplot2::ggplot(data, ggplot2::aes_string(x = "x", y = "y", fill = value_col)) +
    ggplot2::geom_raster() +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, fill = value_col)
}
