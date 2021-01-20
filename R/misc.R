#' Categorise a character vector's element names by its element values
#'
#' @param rmdNamed_char. A named vector.
#'
#' @return
#' @export
#'
#' @examples
#'
#' x <- sample(1:5, 10, replace = T)
#' names(x) <- sample(LETTERS, 10)
#' cat_x <-
#'   categorise_elementNames_ByElementValues(x)
categorise_elementNames_ByElementValues <- function(rmdNamed_char) {
  rmdNames <- names(rmdNamed_char)
  types <- unique(rmdNamed_char)
  purrr::map(
    types,
    ~{
      list(
        el_names = rmdNames[rmdNamed_char==.x],
        el_values = .x,
        comment=character(0),
        grade=0
      )
    }
  ) -> catResults

  catResults
}

#' Safely unlist a list where element with zero length will be list(0)
#'
#' @param listObj A list
#'
#' @return
#' @export
#'
#' @examples none
safe_unlist <- function(listObj){
  require(dplyr)
  require(purrr)
  map(
    listObj,
    ~{ifelse(length(.x)==0,0,.x)}
  ) %>%
    unlist()
}
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
