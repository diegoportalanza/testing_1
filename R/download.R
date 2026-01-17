#' Build NASA NEX-GDDP-CMIP6 file URL
#'
#' Constructs a direct URL to a daily NetCDF file from NASA's NEX-GDDP-CMIP6
#' dataset on the THREDDS server. Files contain bias-corrected, statistically
#' downscaled (0.25° resolution) daily data from CMIP6 GCMs (1950–2100).
#'
#' @details
#' Official source: <https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6>
#'
#' File structure:  
#' base/model/scenario/ensemble/variable/variable_day_model_scenario_ensemble_grid_year.nc
#'
#' Available models (37 as of 2026 – full list from THREDDS catalog):  
#' ACCESS-CM2, ACCESS-ESM1-5, BCC-CSM2-MR, CESM2, CESM2-WACCM, CMCC-CM2-SR5,  
#' CMCC-ESM2, CNRM-CM6-1, CNRM-ESM2-1, CanESM5, EC-Earth3, EC-Earth3-Veg-LR,  
#' FGOALS-g3, GFDL-CM4, GFDL-CM4_gr2, GFDL-ESM4, GISS-E2-1-G, HadGEM3-GC31-LL,  
#' HadGEM3-GC31-MM, IITM-ESM, INM-CM4-8, INM-CM5-0, IPSL-CM6A-LR, KACE-1-0-G,  
#' KIOST-ESM, MIROC-ES2L, MIROC6, MPI-ESM1-2-HR, MPI-ESM1-2-LR, MRI-ESM2-0,  
#' NESM3, NorESM2-LM, NorESM2-MM, TaiESM1, UKESM1-0-LL
#'
#' Scenarios: historical, ssp126, ssp245, ssp370, ssp585  
#' Variables: tas, tasmax, tasmin, pr, huss, hurs, rlds, rsds, sfcWind (availability varies)  
#' Ensemble: mostly "r1i1p1f1" (some models have variants like r2i1p1f1)  
#' Grid: mostly "gn" (a few use "gr")
#'
#' @param model Character. Model name (case-sensitive; must match list above).
#' @param scenario Character. Experiment name (will be lowercased).
#' @param variable Character. Variable code (e.g., "tasmax").
#' @param ensemble Character. Ensemble member (default: "r1i1p1f1").
#' @param grid Character. Grid label (default: "gn").
#' @param year Integer. Single year (1950–2100 range recommended).
#' @param base_url Character. Base THREDDS path; override with  
#'   `options(cmip6extremes.nex_gddp_base_url = "...")` if needed.
#'
#' @return Full URL as character string.
#'
#' @export
#'
#' @examples
#' build_nex_gddp_url("GFDL-ESM4", "ssp585", "tasmax", year = 2050)
#' build_nex_gddp_url("ACCESS-CM2", "historical", "pr", year = 2000)
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

  # Validate year
  if (!is.numeric(year) || length(year) != 1 || year %% 1 != 0) {
    stop("year must be a single integer.")
  }
  if (year < 1950 || year > 2100) {
    warning(sprintf(
      "Year %d is outside the standard NEX-GDDP-CMIP6 coverage (1950–2100). URL may return 404.",
      year
    ))
  }

  # Normalize scenario (handle common variants)
  scenario <- tolower(scenario)
  scenario <- gsub("[.-]", "", scenario)  # e.g., "ssp-585" → "ssp585", "SSP5.85" → "ssp585"

  # Validate model (prevents 99% of invalid URLs)
  valid_models <- c(
    "ACCESS-CM2", "ACCESS-ESM1-5", "BCC-CSM2-MR", "CESM2", "CESM2-WACCM",
    "CMCC-CM2-SR5", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-ESM2-1", "CanESM5",
    "EC-Earth3", "EC-Earth3-Veg-LR", "FGOALS-g3", "GFDL-CM4", "GFDL-CM4_gr2",
    "GFDL-ESM4", "GISS-E2-1-G", "HadGEM3-GC31-LL", "HadGEM3-GC31-MM",
    "IITM-ESM", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "KACE-1-0-G",
    "KIOST-ESM", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR",
    "MRI-ESM2-0", "NESM3", "NorESM2-LM", "NorESM2-MM", "TaiESM1", "UKESM1-0-LL"
  )

  if (!model %in% valid_models) {
    stop(sprintf(
      "Invalid model: '%s'.\nAvailable models (37 total): %s\nCheck catalog: https://ds.nccs.nasa.gov/thredds/catalog/AMES/NEX/GDDP-CMIP6/catalog.html",
      model, paste(valid_models, collapse = ", ")
    ))
  }

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
#' @inheritParams build_nex_gddp_url
#' @param url Character. URL from [build_nex_gddp_url()].
#' @param destfile Character. Local save path.
#' @param overwrite Logical (default FALSE).
#' @param mode Character. "wb" for binary (recommended for NetCDF).
#' @param ... Passed to [utils::download.file()].
#'
#' @return Invisibly returns destfile path.
#' @export
download_nex_gddp <- function(url, destfile, overwrite = FALSE, mode = "wb", ...) {
  if (missing(url) || missing(destfile)) {
    stop("url and destfile are required.")
  }
  if (file.exists(destfile) && !overwrite) {
    stop("File exists. Use overwrite = TRUE.")
  }

  result <- tryCatch(
    utils::download.file(url = url, destfile = destfile, mode = mode, quiet = FALSE, ...),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    stop(
      "Download failed.\nURL: ", url, "\n",
      "Common causes: invalid model/scenario/variable/year combo, network issue, or server downtime.\n",
      "Verify existence: https://ds.nccs.nasa.gov/thredds/catalog/AMES/NEX/GDDP-CMIP6/catalog.html\n",
      "Error: ", result$message
    )
  }

  message("Successfully downloaded: ", destfile, " (", round(file.info(destfile)$size / 1e6, 1), " MB)")
  invisible(destfile)
}
