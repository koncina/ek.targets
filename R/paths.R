get_rel_path <- function(remove = 1) {
  h <- here()
  h <- path_split(h)[[1]]
  h <- h[(which(h == "projects") + 1):(length(h) - remove)]
  path_join(h)
}

#' @importFrom purrr accumulate
#' @importFrom fs path_split path_rel file_exists
#'
#' @export
rel_project <- function(marker_file = ".projects", from = TRUE) {
  x <- accumulate(path_split(getwd())[[1]], file.path)

  if (isTRUE(from)) {
    path_rel(x[length(x)],
             x[max(which(file_exists(file.path(x, ".projects"))))])
  } else {
    x[max(which(file_exists(file.path(x, ".projects"))))]
  }
}

shared_path <- function(share_folder, filename) {
  f <- file.path(user_data_dir(share_folder), filename)
  dir_create(dirname(f))
  f
}
