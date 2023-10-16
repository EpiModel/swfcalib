#' @ImportFrom dplyr .data
NULL

if (FALSE) {

  assessments <- readRDS("./assessments.rds")
  rmarkdown::render(
    system.file("rmd/assessment.Rmd", package = "swfcalib"),
    params = list(assessments = assessments)
  )

}
