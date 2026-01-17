#' Build NASA NEX-GDDP-CMIP6 file URL (using reliable S3 bucket)
#'
#' Constructs URL for daily NetCDF from NASA's public AWS S3 bucket
#' (more reliable for full files than THREDDS fileServer).
#'
#' @param model Character. Model name.
#' @param scenario Character. "historical", "ssp126", etc.
#' @param variable Character. "tasmax", "pr", etc.
#' @param ensemble Character. Default "r1i1p1f1".
#' @param grid Character. "gn" (default) or "gr1" (for some models like GFDL-ESM4).
#' @param year Integer.
#' @param base_url Character. Defaults to AWS S3; change to THREDDS if needed.
#' @param version_suffix Character. Sometimes "_v2.0" — leave empty unless needed.
#'
#' @export
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
  # ... keep your existing checks for required args, year, model validation ...

  scenario <- tolower(gsub("[.-]", "", scenario))

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
