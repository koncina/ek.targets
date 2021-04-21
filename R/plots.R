#' Save a ggplot as a standalone file using ggsave
#'
#' @importFrom ggplot2 ggsave
#' @importFrom fs dir_create
#'
#' @param x a ggplot
#' @param filename The basename of the output file
#'
#' @export
write_plot <- function(x, filename, ...) {
  f <- shared_path("reports", file.path(get_rel_path(), "plots", filename))
  dir_create(dirname(f))
  ggsave(f, plot = x, ...)
  f
}

#' Save a ComplexHeatmap as a standalone png file
#'
#' @importFrom ragg agg_png
#' @importFrom fs dir_create
#'
#' @param x a ComplexHeatmap object
#' @param filename The basename of the output file
#' @param width,height,units,res The settings for the output file
#'
#' @export
write_heatmap_png <- function(x, filename, width = 500, height = 500, units = "px", res = 150, ...) {
  stopifnot(str_detect(filename, "\\.png$"))
  f <- shared_path("reports", file.path(get_rel_path(), "plots", filename))
  dir_create(dirname(f))
  agg_png(f, width = width, height = height, units = units, res = res)
  draw(x, ...)
  invisible(dev.off())
  f
}

#' Save a ComplexHeatmap as a standalone pdf file
#'
#' @importFrom grDevices pdf
#' @importFrom fs dir_create
#'
#' @param x a ComplexHeatmap object
#' @param filename The basename of the output file
#' @param width,height The settings for the output file
#'
#' @export
write_heatmap <- function(x, filename, width = 8, height = 8) {
  stopifnot(str_detect(filename, "\\.pdf$"))
  f <- shared_path("reports", file.path(get_rel_path(), "plots", filename))
  dir_create(dirname(f))
  pdf(f, width = width, height = height)
  draw(x)
  dev.off()
  f
}
