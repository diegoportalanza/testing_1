#' Clip NEX-GDDP-CMIP6 NetCDF file to a polygon boundary
#'
#' Crops and masks a NetCDF raster (daily or multi-layer) to a polygon shapefile
#' or GeoPackage boundary using terra.
#'
#' @param nc_path Character. Path to the input NetCDF file.
#' @param shape_path Character. Path to polygon shapefile (.shp) or GeoPackage (.gpkg).
#' @param out_path Character. Path where the clipped NetCDF will be saved.
#' @param var_name Character (optional). Specific variable name to read from NetCDF.
#' @param overwrite Logical. Overwrite existing output file? (default: FALSE)
#'
#' @return Invisibly returns the output path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clip_nex_gddp_to_shape("tasmax_2050.nc", "ecuador_provinces.gpkg", "tasmax_2050_ecuador.nc")
#' }
clip_nex_gddp_to_shape <- function(nc_path,
                                   shape_path,
                                   out_path,
                                   var_name = NULL,
                                   overwrite = FALSE) {
  if (missing(nc_path) || missing(shape_path) || missing(out_path)) {
    stop("nc_path, shape_path, and out_path are required.")
  }
  if (!file.exists(nc_path)) stop("Input NetCDF not found: ", nc_path)
  if (!file.exists(shape_path)) stop("Shapefile/GeoPackage not found: ", shape_path)
  if (file.exists(out_path) && !overwrite) {
    stop("Output file exists. Use overwrite = TRUE.")
  }

  # Read raster (can be multi-layer)
  r <- if (is.null(var_name)) {
    terra::rast(nc_path)
  } else {
    terra::rast(nc_path, lyrs = var_name)  # more explicit
  }

  # Read vector
  v <- terra::vect(shape_path)

  # Reproject vector to raster CRS if needed
  if (!terra::same.crs(v, r)) {
    v <- terra::project(v, terra::crs(r))
  }

  # Crop + mask
  r_crop <- terra::crop(r, v, snap = "out")
  r_masked <- terra::mask(r_crop, v)

  # Write (NetCDF format preserved)
  terra::writeRaster(r_masked, filename = out_path, overwrite = overwrite,
                     filetype = "CDF", gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"))

  message("Clipped file saved: ", out_path, " (", terra::nlyr(r_masked), " layers)")
  invisible(out_path)
}
