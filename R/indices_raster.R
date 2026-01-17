#' Calculate yearly extreme climate indices on a raster time series
#'
#' Computes selected ETCCDI-style extreme indices for each year from a daily
#' NetCDF raster stack (e.g., from NEX-GDDP-CMIP6).
#'
#' @param nc_path Character. Path to daily NetCDF file.
#' @param var_name Character. Variable name in NetCDF (e.g., "tasmax", "pr").
#' @param indices Character vector. Indices to compute: "txx", "tnn", "rx1day", "r10mm", "r95p".
#' @param baseline_years Integer vector of length 2 (default: 1961–1990). Used for r95p threshold.
#' @param shape_path Character (optional). Polygon to clip/mask the raster first.
#'
#' @return SpatRaster with one layer per year per index (names like "txx_2050").
#'
#' @export
calculate_extreme_indices_raster <- function(
    nc_path,
    var_name,
    indices = c("txx", "tnn", "rx1day", "r10mm", "r95p"),
    baseline_years = c(1961, 1990),
    shape_path = NULL
) {
  indices <- match.arg(indices, several.ok = TRUE)

  r <- terra::rast(nc_path, lyrs = var_name)
  times <- terra::time(r)
  if (is.null(times) || !inherits(times, "Date")) {
    stop("NetCDF must have valid time dimension (Date class).")
  }

  # Optional clip/mask
  if (!is.null(shape_path)) {
    v <- terra::vect(shape_path)
    if (!terra::same.crs(v, r)) v <- terra::project(v, r)
    r <- terra::mask(terra::crop(r, v), v)
  }

  years <- as.integer(format(times, "%Y"))
  unique_years <- sort(unique(years))

  # Compute baseline threshold for r95p if requested
  r95_threshold <- NULL
  if ("r95p" %in% indices) {
    if (is.null(baseline_years) || length(baseline_years) != 2) {
      stop("baseline_years must be length 2 when computing r95p")
    }
    baseline_mask <- years >= baseline_years[1] & years <= baseline_years[2]
    if (sum(baseline_mask) < 365 * 5) {
      warning("Baseline period has <5 years of data → threshold unreliable.")
    }
    baseline_r <- r[[which(baseline_mask)]]
    r95_threshold <- terra::quantile(baseline_r, probs = 0.95, na.rm = TRUE)
  }

  # Build list of rasters
  out_list <- list()

  for (yr in unique_years) {
    idx <- which(years == yr)
    if (length(idx) < 300) {
      warning(sprintf("Year %d has only %d days → indices may be unreliable.", yr, length(idx)))
    }
    r_yr <- r[[idx]]

    if ("txx" %in% indices) {
      out_list[[paste0("txx_", yr)]] <- terra::app(r_yr, max, na.rm = TRUE)
    }
    if ("tnn" %in% indices) {
      out_list[[paste0("tnn_", yr)]] <- terra::app(r_yr, min, na.rm = TRUE)
    }
    if ("rx1day" %in% indices) {
      out_list[[paste0("rx1day_", yr)]] <- terra::app(r_yr, max, na.rm = TRUE)
    }
    if ("r10mm" %in% indices) {
      out_list[[paste0("r10mm_", yr)]] <- terra::app(r_yr, function(x) sum(x >= 10, na.rm = TRUE))
    }
    if ("r95p" %in% indices) {
      exceed <- r_yr > r95_threshold
      out_list[[paste0("r95p_", yr)]] <- terra::app(exceed, sum, na.rm = TRUE)
    }
  }

  result <- terra::rast(out_list)
  terra::set.names(result, names(out_list))
  result
}
