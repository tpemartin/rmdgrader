#' Create inspection instance when df_grades in the global environment
#'
#' @param path A path to the folder where all the synthesized Rmds are.
#'
#' @return
#' @export
#'
#' @examples none.
Inspect <- function(path){
  assertthat::assert_that(
    exists("df_grades", envir=.GlobalEnv),
    msg="No df_grades in your global environment."
  )
  dsp <- new.env(parent = .GlobalEnv)
  list_rmds <- list.files(path, full.names=T)
  dsp$list_rmds <- list_rmds
  dsp$file.edit <- create_file.edit(list_rmds = list_rmds, .dsp=dsp)
  dsp$file.edit_next <- function(){
    whichIsCurrent <- which(names(dsp$file.edit)==dsp$current)
    if(
      length(whichIsCurrent)!=0 &&
        whichIsCurrent != length(dsp$list_rmds)){
      dsp$file.edit[[whichIsCurrent+1]]()
      dsp$file.edit_previous <- dsp$file.edit[[whichIsCurrent]]
    } else if(length(whichIsCurrent)==0){
      dsp$file.edit[[1]]()
    } else {
      message("This is already the last.")
      dsp$file.edit_previous <- dsp$file.edit[[length(dsp$list_rmds)]]
    }
  }

  dsp$update_grade <- create_update.grade(list_rmds)
  return(dsp)
}
# helpers -----------------------------------------------------------------

create_update.grade <- function(list_rmds){
  purrr::map(
    list_rmds,
    create_updateGrade4rmdx
  ) |>
    setNames(basename(list_rmds))
}
create_updateGrade4rmdx <- function(rmdX){
  function(){
    rmdlines = xfun::read_utf8(rmdX)
    fmatterX <- rmarkdown::yaml_front_matter(rmdX)
    fmatterX$grade |> data.frame() -> df_gradeX_new
    names(df_gradeX_new)[[1]] <- "sum"
    whichIsTheRmd <- which(.GlobalEnv$df_grades$rmd==basename(rmdX))
    .GlobalEnv$df_grades[whichIsTheRmd, -1] <- df_gradeX_new
  }
}
create_file.edit <- function(list_rmds, .dsp){
  purrr::map(
    list_rmds,
    ~{
      function(){
        file.edit(.x)
        .dsp$current <- basename(.x)
      }
    }
  ) |>
    setNames(basename(list_rmds))
}

