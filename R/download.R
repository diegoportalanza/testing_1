#' Build a CMIP6 NEXT GGPD dataset URL
#'
#' Construct a URL for a CMIP6 NEXT GGPD file based on model metadata.
#'
#' @param model Character name of the CMIP6 model (e.g., "GFDL-ESM4").
#' @param scenario Character scenario name (e.g., "ssp585").
#' @param variable Character variable name (e.g., "tasmax").
#' @param year Integer year for the file.
#' @param base_url Base URL for the GGPD data repository. Defaults to
#'   `getOption("cmip6extremes.base_url", "https://cmip6-next.ggpd.org")`.
#'
#' @return A character URL pointing to the requested file.
#' @export
build_ggpd_url <- function(model,
                           scenario,
                           variable,
                           year,
                           base_url = getOption("cmip6extremes.base_url", "https://cmip6-next.ggpd.org")) {
  if (missing(model) || missing(scenario) || missing(variable) || missing(year)) {
    stop("model, scenario, variable, and year are required.")
  }

  if (!is.numeric(year) || length(year) != 1) {
    stop("year must be a single numeric value.")
  }

  sprintf(
    "%s/%s/%s/%s/%s_%s_%s_%s.nc",
    base_url,
    scenario,
    model,
    variable,
    variable,
    model,
    scenario,
    year
  )
}

#' Build a NEX-GDDP-CMIP6 THREDDS dataset URL
#'
#' Construct a direct NetCDF file URL from the NASA NCCS THREDDS catalog.
#'
#' @param model Character name of the CMIP6 model (e.g., "ACCESS-CM2").
#' @param scenario Character scenario name (e.g., "historical", "ssp585").
#' @param variable Character variable name (e.g., "pr", "tasmax").
#' @param ensemble Character ensemble member (e.g., "r1i1p1f1").
#' @param grid Character grid label (e.g., "gn").
#' @param year Integer year for the daily file.
#' @param base_url Base THREDDS data server URL. Defaults to
#'   `getOption("cmip6extremes.nex_gddp_base_url", "https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6")`.
#'
#' @return A character URL pointing to the requested NetCDF file.
#' @export
build_nex_gddp_url <- function(model,
                               scenario,
                               variable,
                               ensemble,
                               grid,
                               year,
                               base_url = getOption(
                                 "cmip6extremes.nex_gddp_base_url",
                                 "https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6"
                               )) {
  if (missing(model) || missing(scenario) || missing(variable) ||
      missing(ensemble) || missing(grid) || missing(year)) {
    stop("model, scenario, variable, ensemble, grid, and year are required.")
  }

  if (!is.numeric(year) || length(year) != 1) {
    stop("year must be a single numeric value.")
  }

  sprintf(
    "%s/%s/%s/%s/%s/%s_day_%s_%s_%s_%s_%s.nc",
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
    year
  )
}

#' Download a CMIP6 NEXT GGPD dataset
#'
#' Download a GGPD file from a URL to a local path.
#'
#' @param url Character URL to download.
#' @param destfile Character path for the downloaded file.
#' @param overwrite Logical indicating whether to overwrite an existing file.
#' @param ... Additional arguments passed to [utils::download.file()].
#'
#' @return The destination file path, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- build_ggpd_url("GFDL-ESM4", "ssp585", "tasmax", 2050)
#' download_cmip6_next_ggpd(url, "tasmax_2050.nc")
#' }
download_cmip6_next_ggpd <- function(url, destfile, overwrite = FALSE, ...) {
  if (missing(url) || missing(destfile)) {
    stop("url and destfile are required.")
  }

  if (file.exists(destfile) && !overwrite) {
    stop("Destination file already exists. Set overwrite = TRUE to replace it.")
  }

  result <- tryCatch(
    utils::download.file(url = url, destfile = destfile, ...),
    error = function(err) err
  )

  if (inherits(result, "error")) {
    stop(
      "Download failed. Check your network connection and base URL. ",
      "You can set options(cmip6extremes.base_url = \"<valid-url>\"). ",
      "Original error: ",
      result$message
    )
  }
  invisible(destfile)
}
