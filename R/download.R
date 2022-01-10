#' Generate a download instance
#'
#' @description the instance is equipped with get_courseWork method which requires a google drive folder URL, class roster with 姓名, 學號 and Google classroom login email (the name of this column is also required). Once applied get_courseWork method, user can Download$courseWork$download(overwrite=F, validSchoolIds=all school Ids) to download those work from validSchoolIds
#'
#' @param title the title of the course work.
#' @param path the path where files downloaded to.
#'
#' @return
#' @export
#'
#' @examples none
Download <- function(title, path)
{
  download <- new.env()
  download$title <- title
  download$path <- {
    if(!dir.exists(path)) dir.create(path, recursive = T)
    path
  }
  download$get_courseWork <- function(gd_url, roster, colname_googleClassLogin){
    courseWork <-
      get_courseWorkInfoFromGoogleDriveFolderUrl(gd_url)
    courseWork <-
      appendRosterInfo2courseWork(courseWork, roster, colname_googleClassLogin)

    download$courseWork <- courseWork

    download$courseWork$submissions$googleLogin |>
      tolower() ->
      download$courseWork$submissions$googleLogin

    download$courseWork$download <-
      function(
        overwrite=F,
        validSchoolIds = download$courseWork$submissions$學號){
        require(dplyr)
        download$courseWork$submissions %>%
          arrange(學號, desc(createdTime)) %>%
          group_by(學號) %>%
          slice(1) -> download$courseWork$download_meta
        download$courseWork$download_meta %>%
          mutate(
            isRmd=stringr::str_detect(filename,"\\.[rR][mM][dD]$")
          ) -> download$courseWork$download_meta
        pick_schoolIds <- download$courseWork$download_meta$學號 %in% validSchoolIds
        purrr::map_chr(
          seq_along(download$courseWork$download_meta$學號[pick_schoolIds]),
          ~{

            download$courseWork$download_meta$學號[pick_schoolIds][[.x]] -> schoolId

            tryCatch(
              {
                filenameX <- file.path(
                  download$path,
                  stringr::str_remove_all(glue::glue("{download$title}-{schoolId}.Rmd"), "\\s")
                )
                googledrive::drive_download(
                  file=download$courseWork$download_meta$id[pick_schoolIds][[.x]], path=filenameX,
                  overwrite=overwrite
                )
                filenameX
              },
              error=function(e){
                filenameX = "error"
                filenameX
              }
            ) -> filenameX


            filenameX
          }
        ) -> download$courseWork$download_meta$studentRmds[pick_schoolIds]
      }
  }

  download
}

# helpers -----------------------------------------------------------------


appendRosterInfo2courseWork <- function(courseWork, roster, colname_googleClassLogin){
  # colname_googleClassLogin = rlang::enquo(colname_googleClassLogin)
  courseWork$submissions %>%
    left_join(
      roster %>% select(姓名, 學號, colname_googleClassLogin),
      by=c("googleLogin"=colname_googleClassLogin)
    ) -> courseWork$submissions
  courseWork
}

get_courseWorkInfoFromGoogleDriveFolderUrl <- function(gd_courseWorkUrl){
  courseWork <- list()
  courseWork$url <- gd_courseWorkUrl
  courseWork$dribble <- googledrive::as_dribble(courseWork$url)
  courseWork$list <- googledrive::drive_ls(courseWork$dribble)

  purrr::map_lgl(
    seq_along(courseWork[["list"]][["drive_resource"]]),
    ~{courseWork[["list"]][["drive_resource"]][[.x]][["ownedByMe"]][[1]]}
  ) -> flags_ownedByMe


  purrr::map_dfr(
    seq_along(courseWork$list$drive_resource),
    ~{
      # print(.x)
      data.frame(
        googleLogin=ifelse(
          flags_ownedByMe[[.x]],
          courseWork[["list"]][["drive_resource"]][[.x]][["sharingUser"]][["emailAddress"]],
          courseWork[["list"]][["drive_resource"]][[.x]][["owners"]][[1]][["emailAddress"]]
        ),
          # courseWork$list$drive_resource[[.x]]$lastModifyingUser$emailAddress,
        createdTime={
          courseWork$list$drive_resource[[.x]]$createdTime |>
            lubridate::ymd_hms() |> lubridate::with_tz(tz="Asia/Taipei")
        },
        filename=courseWork$list$name[[.x]],
        filesize={
          courseWork$list$drive_resource[[.x]]$size -> size
          ifelse(is.null(size), NA, size)
        },
        id=courseWork$list$id[[.x]]
      )
    }
  ) -> courseWork$submissions
  return(courseWork)
}

