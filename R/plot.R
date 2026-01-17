#' Plot a map of an extreme climate index from a SpatRaster
#'
#' Creates a ggplot2 raster map of one layer (year) from an extreme indices raster.
#'
#' @param index_raster SpatRaster. Output from [calculate_extreme_indices_raster()].
#' @param year Integer or character. Year or layer name to plot (defaults to first layer).
#' @param title Character (optional). Plot title.
#' @param palette Character. Color scale ("viridis", "turbo", "magma", etc.).
#'
#' @return A ggplot2 object.
#'
#' @export
plot_extreme_index_map <- function(index_raster,
                                   year = NULL,
                                   title = NULL,
                                   palette = "viridis") {
  if (!inherits(index_raster, "SpatRaster")) {
    stop("index_raster must be a SpatRaster.")
  }

  # Select layer
  if (is.null(year)) {
    layer <- index_raster[[1]]
    layer_name <- names(index_raster)[1]
  } else {
    layer_name <- if (is.numeric(year)) as.character(year) else year
    if (!layer_name %in% names(index_raster)) {
      stop("Year/layer '", layer_name, "' not found.")
    }
    layer <- index_raster[[layer_name]]
  }

  # Convert to df efficiently
  df <- terra::as.data.frame(layer, xy = TRUE, na.rm = TRUE)
  if (nrow(df) == 0) stop("No non-NA values in selected layer.")
  val_col <- names(df)[3]

  # CRS info
  crs_label <- terra::crs(layer, describe = TRUE)$name
  if (is.na(crs_label)) crs_label <- "unknown"

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = .data[[val_col]])) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_c(option = palette, na.value = "grey80") +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(
      title = title %||% paste("Index:", toupper(sub("_.*", "", layer_name)), "-", layer_name),
      subtitle = paste("CRS:", crs_label),
      fill = sub(".*_", "", layer_name),
      caption = "Data: NEX-GDDP-CMIP6"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 9),
      plot.title = ggplot2::element_text(face = "bold")
    )
}
