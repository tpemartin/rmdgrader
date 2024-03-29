#' Generate return instance to return Rmd in Google classroom
#'
#' @param path a path to the folder where all the Rmds there are to be returned.
#' @param download an instance of Download.
#'
#' @return
#' @export
#'
#' @examples none
Return2 <- function(path, download){
  rt <- new.env()
  # _return$return <- return_rmds_functional(return, path, download)
  get_returnable(path, download) -> rt$returnable
  # rt$get_returnable <- get_returnable_functional(rt, path, download)

  rt$return_all <- return_rmd_functional(returnable = rt$returnable)
  rt$returnable$file2return |> basename() -> studentRmds
  return_individuals <- purrr::map(
    seq_along(studentRmds),
    ~{
      return_rmd_functional(
        returnable = {
          rt$returnable[.x, ]
        }
      )
    }
  )
  names(return_individuals) <- studentRmds
  rt$individual <- return_individuals
  return(rt)
}

return_rmd_functional <- function(returnable){
  function(){
    return_rmds(returnable)
  }
}

return_rmds <- function(returnable) {

  # View(df_returns)
  for(.x in seq_along(returnable$file2return)){
    cat("returning ", returnable$file2return[[.x]])
    googledrive::drive_update(
      file=returnable$id[[.x]],
      media=file.path(returnable$file2return[[.x]])
    )
  }

}

get_returnable_functional <- function(return, path, download){
  function(){
    get_returnable(return, path, download) -> return$returnable
  }
}

get_returnable <- function(path, download){
  rmds2return <- list.files(path)
  rmds2return |> stringr::str_extract("(?<=-)[0-9]+(?=\\.)") |>
    as.integer() -> schoolIds2return
  download$courseWork$submissions %>%
    dplyr::filter(學號 %in% schoolIds2return) %>%
    arrange(學號, desc(createdTime)) %>%
    group_by(學號) %>%
    slice(1) -> files2return
  data.frame(
    file2return = file.path(path,rmds2return),
    schoolId = schoolIds2return |> as.character()
  ) %>%
    left_join(
      files2return %>%
        mutate(學號=as.character(學號)) %>%
        select(學號,id),
      by=c("schoolId"="學號")
    ) -> df_returns
  return(df_returns)
}
