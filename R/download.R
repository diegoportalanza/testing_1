#' Build NASA NEX-GDDP-CMIP6 file URL (using reliable AWS S3 bucket)
#'
#' Constructs a direct download URL for daily NetCDF files from NASA's 
#' public AWS S3 bucket[](https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/NEX-GDDP-CMIP6).
#' This is more reliable than the THREDDS fileServer for full-year downloads.
#'
#' @details
#' Grid label varies by model:
#' - "gn" (default) works for most models (TaiESM1, ACCESS-CM2, MRI-ESM2-0, etc.)
#' - "gr1" is required for some models like GFDL-ESM4
#' Not all model/variable/scenario/year combinations exist.
#' Always verify existence by opening the URL in a browser or using download_nex_gddp().
#'
#' @param model Character. CMIP6 model (case-sensitive, e.g. "GFDL-ESM4", "TaiESM1")
#' @param scenario Character. "historical", "ssp126", "ssp245", "ssp370", "ssp585"
#' @param variable Character. e.g. "tasmax", "tasmin", "pr"
#' @param ensemble Character. Usually "r1i1p1f1" (default)
#' @param grid Character. "gn" (default) or "gr1" (required for some models)
#' @param year Integer. Year of data (1950–2100)
#' @param base_url Character. Override base path (default = AWS S3 bucket)
#' @param version_suffix Character. Rarely needed, e.g. "_v2.0" (default "")
#'
#' @return Character string with the full download URL
#'
#' @export
#'
#' @examples
#' build_nex_gddp_url("TaiESM1", "ssp245", "tasmax", year = 2050)
#' build_nex_gddp_url("GFDL-ESM4", "ssp585", "pr", grid = "gr1", year = 2050)
build_nex_gddp_url <- function(
    model,
    scenario,
    variable,
    ensemble = "r1i1p1f1",
    grid = "gn",
    year,
    base_url = getOption("cmip6extremes.nex_gddp_base_url",
                         "https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/NEX-GDDP-CMIP6"),
    version_suffix = ""
) {
  if (missing(model) || missing(scenario) || missing(variable) || missing(year)) {
    stop("model, scenario, variable, and year are required.")
  }
  if (!is.numeric(year) || length(year) != 1 || year %% 1 != 0) {
    stop("year must be a single integer value.")
  }
  if (year < 1950 || year > 2100) {
    warning("Year outside NEX-GDDP-CMIP6 range (1950–2100); file may not exist.")
  }

  # Normalize scenario (handle SSP585 → ssp585, etc.)
  scenario <- tolower(scenario)
  scenario <- gsub("[.-]", "", scenario)

  # Common model validation (partial list - expand as needed)
  valid_models <- c(
    "ACCESS-CM2", "ACCESS-ESM1-5", "BCC-CSM2-MR", "CESM2", "CNRM-CM6-1",
    "GFDL-ESM4", "MRI-ESM2-0", "TaiESM1", "UKESM1-0-LL" # add more
  )
  if (!model %in% valid_models) {
    warning("Model '", model, "' not in known list; may not exist in NEX-GDDP-CMIP6.")
  }

  # Grid warning for known cases
  if (model == "GFDL-ESM4" && grid == "gn") {
    warning("GFDL-ESM4 usually uses grid = 'gr1' (not 'gn'). Trying anyway.")
  }

  file_name <- sprintf(
    "%s_day_%s_%s_%s_%s_%d%s.nc",
    variable,
    model,
    scenario,
    ensemble,
    grid,
    as.integer(year),
    version_suffix
  )

  sprintf(
    "%s/%s/%s/%s/%s/%s",
    base_url,
    model,
    scenario,
    ensemble,
    variable,
    file_name
  )
}


#' Download a NEX-GDDP-CMIP6 NetCDF file from AWS S3
#'
#' @param url Character. URL from [build_nex_gddp_url()]
#' @param destfile Character. Local filename/path
#' @param overwrite Logical. Overwrite if exists? (default FALSE)
#' @param mode Character. "wb" for binary/NetCDF (recommended)
#' @param ... Additional arguments to [utils::download.file()]
#'
#' @return Invisibly returns destfile path
#'
#' @export
download_nex_gddp <- function(
    url,
    destfile,
    overwrite = FALSE,
    mode = "wb",
    ...
) {
  if (missing(url) || missing(destfile)) {
    stop("url and destfile are required.")
  }
  if (file.exists(destfile) && !overwrite) {
    stop("File already exists: ", destfile, ". Use overwrite = TRUE.")
  }

  result <- tryCatch(
    utils::download.file(url = url, destfile = destfile, mode = mode, quiet = FALSE, ...),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    stop(
      "Download failed (HTTP error or file does not exist).\n",
      "URL: ", url, "\n",
      "Common causes:\n",
      "  - Invalid model/scenario/variable/grid/year combination\n",
      "  - Try grid = 'gr1' for GFDL-ESM4 and similar models\n",
      "  - File may not be available in NEX-GDDP-CMIP6\n",
      "Check existence: open the URL in browser or use bucket browser\n",
      "Original error: ", result$message
    )
  }

  message("Successfully downloaded: ", destfile,
          " (", round(file.info(destfile)$size / 1e6, 1), " MB)")
  invisible(destfile)
}
