#' Build a CMIP6 NEXT GGPD dataset URL
#'
#' Construct a URL for a CMIP6 NEXT GGPD file based on model metadata.
#'
#' @param model Character name of the CMIP6 model (e.g., "GFDL-ESM4").
#' @param scenario Character scenario name (e.g., "ssp585").
#' @param variable Character variable name (e.g., "tasmax").
#' @param year Integer year for the file.
#' @param base_url Base URL for the GGPD data repository.
#'
#' @return A character URL pointing to the requested file.
#' @export
build_ggpd_url <- function(model,
                           scenario,
                           variable,
                           year,
                           base_url = "https://cmip6-next.ggpd.org") {
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

  utils::download.file(url = url, destfile = destfile, ...)
  invisible(destfile)
}
