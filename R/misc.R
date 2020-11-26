#' Ensure the path is absolute.
#'
#' @param path A character of path, absolute or relative.
#'
#' @return A character of absolute path.
#' @export
#'
#' @examples none
rootadjustPath <- function(path){
  .root <- rprojroot::is_rstudio_project$make_fix_file()
  stringr::str_extract(.root(), "[^/]+$") -> projName

  stringr::str_extract(path, glue::glue("(?<={projName})[:graph:]+")) -> relativePath

  if(is.na(relativePath)) # the path is already relative
  {
    relativePath <- path
  }

  absolutePath <-
    file.path(
      .root(), relativePath
    )
  absolutePath
}
