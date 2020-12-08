NULL

is_draft <- function(rmd) {
  rmarkdown::yaml_front_matter(rmd) %>%
    pluck("draft") %>%
    isTRUE()
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
#'
#' @param rmd The Rmd file to be rendered
#' @param output_format The rmarkdown output format to be used
#'
#' @export
knit_report <- function(rmd, output_format) {
  tar_cancel(is.null(rmd))

  if (is_draft(rmd)) {
    message("Skipping ", rmd_date, "-", basename(rmd), " (draft)")
    tar_cancel()
  }

  rmd_date <- created(rmd)
  message("Knitting ", rmd_date, "-", basename(rmd))
  if (!file.exists(rmd)) stop("Rmd file doesn't exist!", call. = FALSE)

  # callr by gabenbuie https://github.com/rstudio/gt/issues/297#issuecomment-497778735
  output_file <- callr::r(function(...) rmarkdown::render(...),
                          args = list(input = rmd,
                                      output_format = output_format,
                                      quiet = FALSE,
                                      knit_root_dir = here::here())
  )

  file_move(output_file, shared_path("reports", file.path(get_rel_path(),
                                                          paste0(rmd_date,
                                                                 "-",
                                                                 basename(output_file)))))
}

