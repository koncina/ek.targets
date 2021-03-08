NULL

is_draft <- function(rmd) {
  x <- rmarkdown::yaml_front_matter(rmd)
  isTRUE(x[["draft"]])
}

#' @importFrom rappdirs user_data_dir
#' @importFrom fs file_copy dir_create
#'
#' @export
share_report <- function(report) {
  f <- file.path(user_data_dir("reports"),
                 paste0(rel_project(), ".pdf"))
  dir_create(dirname(f))
  file_copy(report[1],
            f,
            overwrite = TRUE)
}


#' Knit report within a targets pipeline
#'
#' Knit the report and store the rendered file in the shared output folder.
#' Adapted from Aurélien's \href{https://git-r3lab.uni.lu/aurelien.ginolhac/rworkshop}{rworkshop repository code} ()
#'
#' @importFrom rmarkdown render
#' @importFrom here here
#' @importFrom fs file_move
#' @importFrom callr r
#' @importFrom targets tar_cancel
#' @importFrom stringr str_extract
#'
#' @param rmd The Rmd file to be rendered
#' @param output_format The rmarkdown output format to be used
#'
#' @export
knit_report <- function(rmd, output_format) {
  tar_cancel(is.null(rmd))

  if (is_draft(rmd)) {
    message("Skipping ", basename(rmd), " (draft)")
    tar_cancel()
  }

  message("Knitting ", basename(rmd))
  if (!file.exists(rmd)) stop("Rmd file doesn't exist!", call. = FALSE)

  # callr by gabenbuie https://github.com/rstudio/gt/issues/297#issuecomment-497778735
  output_file <- callr::r(function(...) rmarkdown::render(...),
                          args = list(input = rmd,
                                      output_format = output_format,
                                      quiet = FALSE,
                                      knit_root_dir = here::here())
  )


  dated_output_file <- basename(output_file)
  if (is.na(str_extract(dated_output_file, "^\\d{4}-\\d{2}-\\d{2}-"))) {
    rmd_date <- created(output_file)
    dated_output_file <- paste0(rmd_date,
                                "-",
                                dated_output_file)
  }


  file_move(output_file, shared_path("reports", file.path(get_rel_path(),
                                                          dated_output_file)))
}
