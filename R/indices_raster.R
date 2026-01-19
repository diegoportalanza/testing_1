#' Calculate yearly extreme climate indices on a raster time series
#'
#' Computes selected ETCCDI-style extreme indices for each year from a daily
#' NetCDF raster stack (typically from NEX-GDDP-CMIP6 after downloading and merging years).
#'
#' @param nc_path Character. Path to daily NetCDF file (must contain time dimension).
#' @param var_name Character. Variable name in NetCDF (e.g., "tasmax", "pr").
#' @param indices Character vector. Indices to compute. Supported:
#'   "txx" (max tasmax), "tnn" (min tasmin), "rx1day" (max 1-day precip),
#'   "r10mm" (days ≥10 mm precip), "r95p" (precip above 95th percentile),
#'   "rx5day" (max 5-day precip), "prcptot" (total annual precip)
#' @param baseline_years Integer vector of length 2. Years used for r95p threshold
#'   (default: 1961–1990). Must be present in the NetCDF.
#' @param shape_path Character (optional). Path to polygon shapefile/GeoPackage for clipping.
#' @param cores Integer. Number of CPU cores for parallel processing (default 1).
#'   Requires terra >= 1.7-0.
#'
#' @return SpatRaster with one layer per year per index (layer names like "txx_2050").
#'
#' @export
#'
#' @examples
#' \dontrun{
#' extremes <- calculate_extreme_indices_raster(
#'   nc_path = "tasmax_merged_2030_2100.nc",
#'   var_name = "tasmax",
#'   indices = c("txx", "tnn", "r95p"),
#'   baseline_years = c(1995, 2014),
#'   shape_path = "ecuador_basin.gpkg",
#'   cores = 4
#' )
#' plot(extremes[[1]])  # first layer
#' }
calculate_extreme_indices_raster <- function(
    nc_path,
    var_name,
    indices = c("txx", "tnn", "rx1day", "r10mm", "r95p"),
    baseline_years = c(1961, 1990),
    shape_path = NULL,
    cores = 1
) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.")
  }

  indices <- match.arg(indices, several.ok = TRUE)

  # Read raster
  r <- terra::rast(nc_path, lyrs = var_name)
  times <- terra::time(r)

  if (is.null(times) || !inherits(times, "Date")) {
    stop("NetCDF file must have a valid time dimension (Date class).")
  }

  # Optional clip/mask first (saves memory if large area)
  if (!is.null(shape_path)) {
    if (!file.exists(shape_path)) stop("shape_path not found: ", shape_path)
    v <- terra::vect(shape_path)
    if (!terra::same.crs(v, r)) {
      v <- terra::project(v, r)
    }
    r <- terra::crop(r, v, snap = "out")
    r <- terra::mask(r, v)
  }

  # Extract years
  years <- as.integer(format(times, "%Y"))
  unique_years <- sort(unique(years))

  if (length(unique_years) == 0) {
    stop("No valid years found in time dimension.")
  }

  # Baseline threshold for r95p
  r95_threshold <- NULL
  if ("r95p" %in% indices) {
    if (is.null(baseline_years) || length(baseline_years) != 2) {
      stop("baseline_years must be a vector of length 2 for r95p.")
    }
    baseline_mask <- years >= baseline_years[1] & years <= baseline_years[2]
    if (sum(baseline_mask) < 365 * 5) {
      warning("Baseline period has fewer than 5 years of data → threshold may be unreliable.")
    }
    if (sum(baseline_mask) == 0) {
      stop("No data in baseline period ", baseline_years[1], "-", baseline_years[2])
    }
    baseline_r <- r[[which(baseline_mask)]]
    r95_threshold <- terra::quantile(baseline_r, probs = 0.95, na.rm = TRUE)
  }

  # Prepare output list
  out_list <- list()

  # Loop over years
  for (yr in unique_years) {
    idx <- which(years == yr)
    if (length(idx) < 300) {
      warning(sprintf("Year %d has only %d days → indices may be unreliable.", yr, length(idx)))
    }

    r_yr <- r[[idx]]

    # Temperature indices
    if ("txx" %in% indices) {
      out_list[[paste0("txx_", yr)]] <- terra::app(r_yr, max, na.rm = TRUE, cores = cores)
    }
    if ("tnn" %in% indices) {
      out_list[[paste0("tnn_", yr)]] <- terra::app(r_yr, min, na.rm = TRUE, cores = cores)
    }

    # Precipitation indices
    if ("rx1day" %in% indices) {
      out_list[[paste0("rx1day_", yr)]] <- terra::app(r_yr, max, na.rm = TRUE, cores = cores)
    }
    if ("rx5day" %in% indices) {
      # Simple rolling sum (5 days) - note: this can be slow; consider slider or exact method
      rx5 <- terra::app(r_yr, function(x) {
        if (length(x) < 5) NA_real_ else max(zoo::rollsum(x, 5, na.rm = TRUE, align = "right"))
      }, cores = cores)
      out_list[[paste0("rx5day_", yr)]] <- rx5
    }
    if ("r10mm" %in% indices) {
      out_list[[paste0("r10mm_", yr)]] <- terra::app(r_yr, function(x) sum(x >= 10, na.rm = TRUE), cores = cores)
    }
    if ("prcptot" %in% indices) {
      out_list[[paste0("prcptot_", yr)]] <- terra::app(r_yr, sum, na.rm = TRUE, cores = cores)
    }
    if ("r95p" %in% indices) {
      exceed <- r_yr > r95_threshold
      out_list[[paste0("r95p_", yr)]] <- terra::app(exceed, sum, na.rm = TRUE, cores = cores)
    }
  }

  # Combine into single SpatRaster
  if (length(out_list) == 0) {
    stop("No indices computed — check selected indices and data.")
  }

  result <- terra::rast(out_list)
  terra::set.names(result, names(out_list))

  # Add some metadata (optional but nice)
  terra::set.ext(result, terra::ext(r))
  terra::crs(result) <- terra::crs(r)

  result
}
