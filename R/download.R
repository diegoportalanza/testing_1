#' Build NASA NEX-GDDP-CMIP6 file URL
#'
#' Constructs a direct URL to a daily NetCDF file from NASA's NEX-GDDP-CMIP6
#' dataset on the THREDDS server. Files contain bias-corrected, statistically
#' downscaled (0.25° resolution) daily data from CMIP6 GCMs.
#'
#' @details
#' Official source: <https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6>
#'
#' Typical file structure:
#'   base/model/scenario/ensemble/variable/variable_day_model_scenario_ensemble_grid_year.nc
#'
#' Common values (as of 2026):
#' - Models: ACCESS-CM2, CESM2, CNRM-CM6-1, GFDL-ESM4, IPSL-CM6A-LR, MIROC6, MRI-ESM2-0, etc. (full list varies; ~35 models total)
#' - Scenarios: historical, ssp126, ssp245, ssp370, ssp585
#' - Variables: tas, tasmax, tasmin, pr, huss, hurs, rlds, rsds, sfcWind (not all available for every model/scenario)
#' - Ensemble: usually "r1i1p1f1" (some models use others like r2i1p1f1)
#' - Grid: "gn" (native grid) or "gr" in rare cases
#'
#' @param model Character. CMIP6 model short name (e.g., "GFDL-ESM4", "ACCESS-CM2").
#' @param scenario Character. Experiment (e.g., "historical", "ssp585").
#' @param variable Character. Climate variable (e.g., "tasmax", "pr").
#' @param ensemble Character. Ensemble member (default: "r1i1p1f1").
#' @param grid Character. Grid label (default: "gn").
#' @param year Integer. Year of data (e.g., 2050). For "historical", use 1950–2014.
#' @param base_url Character. THREDDS base path. Override via option
#'   `options(cmip6extremes.nex_gddp_base_url = "...")`.
#'
#' @return A character string with the full URL to the .nc file.
#'
#' @export
#'
#' @examples
#' # Future tasmax for GFDL-ESM4 under SSP5-8.5
#' build_nex_gddp_url("GFDL-ESM4", "ssp585", "tasmax", year = 2050)
#'
#' # Historical precipitation for ACCESS-CM2
#' build_nex_gddp_url("ACCESS-CM2", "historical", "pr", year = 2000)
#'
#' # Override base if needed (rare)
#' options(cmip6extremes.nex_gddp_base_url = "https://example-alternate.org/thredds/fileServer/...")
build_nex_gddp_url <- function(
    model,
    scenario,
    variable,
    ensemble = "r1i1p1f1",
    grid     = "gn",
    year,
    base_url = getOption("cmip6extremes.nex_gddp_base_url",
                         "https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6")
) {
  if (missing(model) || missing(scenario) || missing(variable) || missing(year)) {
    stop("model, scenario, variable, and year are required.")
  }
  if (!is.numeric(year) || length(year) != 1 || year %% 1 != 0) {
    stop("year must be a single integer value.")
  }
  if (year < 1950 || year > 2100) {
    warning("Year outside typical NEX-GDDP-CMIP6 range (1950–2100); URL may 404.")
  }

  # Normalize scenario names (some users write "SSP585" → "ssp585")
  scenario <- tolower(scenario)

  sprintf(
    "%s/%s/%s/%s/%s/%s_day_%s_%s_%s_%s_%d.nc",
    base_url,
    model,
    scenario,
    ensemble,
    variable,
    variable,
    model,
    scenario,
    ensemble,
    grid,
    as.integer(year)
  )
}


#' Download a NASA NEX-GDDP-CMIP6 NetCDF file
#'
#' Downloads the file from the constructed URL to a local destination.
#' Uses [utils::download.file()] under the hood.
#'
#' @param url Character. Full URL (usually from [build_nex_gddp_url()]).
#' @param destfile Character. Local path/filename to save (e.g., "tasmax_2050.nc").
#' @param overwrite Logical. If `TRUE`, replace existing file (default: `FALSE`).
#' @param mode Character. Transfer mode ("wb" for binary/NetCDF, default).
#' @param ... Additional arguments passed to [utils::download.file()] (e.g., `method = "curl"`).
#'
#' @return Invisibly returns the `destfile` path on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' url <- build_nex_gddp_url("GFDL-ESM4", "ssp585", "tasmax", year = 2050)
#' download_nex_gddp(url, "tasmax_GFDL-ESM4_ssp585_2050.nc", overwrite = TRUE)
#' }
download_nex_gddp <- function(url, destfile, overwrite = FALSE, mode = "wb", ...) {
  if (missing(url) || missing(destfile)) {
    stop("url and destfile are required.")
  }
  if (file.exists(destfile) && !overwrite) {
    stop("Destination file exists. Use overwrite = TRUE to replace.")
  }

  result <- tryCatch(
    utils::download.file(url = url, destfile = destfile, mode = mode, ...),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    stop(
      "Download failed (HTTP error or invalid URL?).\n",
      "URL: ", url, "\n",
      "Tip: Check if the model/scenario/variable/year combination exists on ",
      "https://ds.nccs.nasa.gov/thredds/catalog/AMES/NEX/GDDP-CMIP6/catalog.html\n",
      "Original error: ", result$message
    )
  }

  message("Downloaded: ", destfile)
  invisible(destfile)
}
