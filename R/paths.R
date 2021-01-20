get_rel_path <- function() {
  h <- here()
  h <- path_split(h)[[1]]
  h <- h[(which(h == "projects") + 1):(length(h) - 1)]
  path_join(h)
}

shared_path <- function(share_folder, filename) {
  f <- file.path(user_data_dir(share_folder), filename)
  dir_create(dirname(f))
  f
}
