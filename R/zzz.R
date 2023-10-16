#' @importFrom dplyr .data
NULL

if (FALSE) {

  # make and export the render function
  # ggplot2 and rmarkdown as suggests

  assessments <- readRDS("./assessments.rds")
  rmarkdown::render(
    system.file("rmd/assessment.Rmd", package = "swfcalib"),
    params = list(assessments = assessments)
  )

}
