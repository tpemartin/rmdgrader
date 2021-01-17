#' Generate a list of file.edit for each student Rmd
#'
#' @param studentRmds A character vector of student Rmds
#' @param submissionPath A character value specifying the path of the above files
#'
#' @return A list of file.edit functions
#' @export
#'
#' @examples none
#' \dontrun{
#'   studentRmds <- allRmds[-1]
#'   submissionPath <-
#'     file.path(
#'       .root(),
#'       params$submissionFolder,
#'       params$title
#'     )
#'   mfile.edit <- mfile.editFunctional(studentRmds, submissionPath)
#' }
mfile.editFunctional <- function(studentRmds, submissionPath){
  basename_rmds <- basename(studentRmds)
  purrr::map(
    basename_rmds,
    function(.x){
      return(
        function(){
          file.edit(
            file.path(submissionPath, .x)
          )
        }
      )
    }
  ) -> list_file.edit
  names(list_file.edit) <- basename_rmds
  return(list_file.edit)
}

