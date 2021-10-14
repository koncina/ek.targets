#' Save a ggplot as a standalone file using ggsave
#'
#' @importFrom ggplot2 ggsave
#' @importFrom fs dir_create
#'
#' @param x a ggplot
#' @param filename The basename of the output file
#'
#' @export
write_plot <- function(x, filename, device = grDevices::png, ...) {
  f <- shared_path("reports", file.path(get_rel_path(remove = 0), filename))
  dir_create(dirname(f))
  ggsave(f, plot = x, device = device, ...)
  f
}

get_heatmap_size <- function(ht, units = "px", ...) {
  ht <- draw(ht, ...)
  w  <-  ComplexHeatmap:::width(ht)
  w <-  convertX(w, units, valueOnly = TRUE)
  h <-  ComplexHeatmap:::height(ht)
  h <-  convertY(h, units, valueOnly = TRUE)
  list(width = w,
       height = h)
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
write_heatmap_png <- function(x, filename, ..., width = NA, height = NA, units = "mm", res = 150) {
  stopifnot(str_detect(filename, "\\.png$"))
  f <- shared_path("reports", file.path(get_rel_path(remove = 0), filename))
  dir_create(dirname(f))

  if (is.na(width) | is.na(height)) {
    ht_size <- get_heatmap_size(x, units = units, ...)
    width <- ht_size$width
    height <- ht_size$height
  }

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
write_heatmap <- function(x, filename, ..., width = NA, height = NA) {
  stopifnot(str_detect(filename, "\\.pdf$"))
  f <- shared_path("reports", file.path(get_rel_path(remove = 0), filename))
  dir_create(dirname(f))

  if (is.na(width) | is.na(height)) {
    ht_size <- get_heatmap_size(x, units = "inches", ...)
    width <- ht_size$width
    height <- ht_size$height
  }

  pdf(f, width = width, height = height)
  draw(x, ...)
  dev.off()
  f
}
