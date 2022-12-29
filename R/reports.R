NULL

#' @importFrom fs file_copy dir_create path_home
#'
#' @export
share_report <- function(report) {
  f <- file.path(path_home("Documents"),
                 "reports",
                 paste0(rel_project(), ".pdf"))
  dir_create(dirname(f))
  file_copy(report[1],
            f,
            overwrite = TRUE)
}
