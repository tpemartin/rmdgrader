#' Review instance initiator
#'
#' @param tb_grades A dataframe of grades from each ansXXX. If need to be recale, recale them before input here.
#' @param title A character of title, normally from params$title
#' @param googleDrivePath A character of path to the drive folder, i.e. path after My Drive/
#'
#' @return A review instance
#' @export
#'
#' @examples None
Review <- function(tb_grades, title, googleDrivePath){
  ge <- new.env()
  ge$source_JSON <- {
    records_gradeComment <-
      generate_recordGradeCommentsFromTb_gradesWithoutComments(tb_grades)

    names_studentRmds <- names(records_gradeComment)

    purrr::map(
      names_studentRmds,
      ~ {
        Xstudent <- .x # names_studentRmds[[.x]]
        Xrecord_gradeComment <- records_gradeComment[[Xstudent]]

        setNames(
          list(
            list(
              grade = as.numeric(NA),
              PR = as.numeric(NA),
              raw = Xrecord_gradeComment,
              tb_grade = subset(tb_grades, tb_grades$name == Xstudent)
            )
          ), title
        )
      }
    ) -> source_gradeJSON
    setNames(source_gradeJSON, names_studentRmds)
  }

  ge$compute_GradePR <- compute_GradePRFuns(ge, names_studentRmds)

  ge$compute_GradePR()

  dribble <- as_dribble(googleDrivePath)
  purrr::walk(
    seq_along(names_studentRmds),
    ~{
      Xstudent <- names_studentRmds[[.x]]
      XallAnsLabels <- stringr::str_subset(
        names(ge$source_JSON[[Xstudent]][[title]]$raw), "ans"
      )
      for(.YansLabel in XallAnsLabels){
        ge$source_JSON[[Xstudent]][[title]]$raw[[.YansLabel]]$gradeUpdate =
          generate_gradeUpdateFuns(ge, Xstudent, title, .YansLabel, dribble)
        ge$source_JSON[[Xstudent]][[title]]$raw[[.YansLabel]]$commentUpdate =
          generate_commentUpdateFuns(ge, Xstudent, title, .YansLabel)
      }
    }
  )
  ge$upload_update <- generate_grade4JSONuploadUpdateFun(ge, dribble, title)

  ge$notifyStudentRevisions <- generate_emailBodyContent(ge, title)

  return(ge)
}

# helpers -----------------------------------------------------------------
generate_gradeUpdateFuns <- function(ge, Xstudent, title, .YansLabel, dribble){
  YansLabel <- .YansLabel
  function(newGrade){
    oldGrade <- ge$source_JSON[[Xstudent]][[title]]$raw[[YansLabel]][[1]]$grade
    ge$source_JSON[[Xstudent]][[title]]$raw[[YansLabel]][[1]]$grade <- newGrade
    ge$source_JSON[[Xstudent]][[title]]$tb_grade[[YansLabel]] <- newGrade

    ge$source_JSON[[Xstudent]][[title]]$raw[[YansLabel]][[1]]$comment -> comment
    if(!exists("revisionHistory", rv)){
      ge$upload_revisionHistory <- generate_revisionHistory_uploadUpdateFun(ge,title, dribble)
    }

    ge$revisionHistory[[Xstudent]][[title]][[YansLabel]] <-
      list(
        old=oldGrade,
        new=newGrade,
        comment=comment
      )

  }
}
generate_commentUpdateFuns <- function(ge, Xstudent, title, .YansLabel){
  YansLabel <- .YansLabel
  function(newComment){
    ge$source_JSON[[Xstudent]][[title]]$raw[[YansLabel]][[1]]$comment <- newComment
    ge$revisionHistory[[Xstudent]][[title]][[YansLabel]]$comment <- newComment
  }
}
compute_GradePRFuns <- function(ge, names_studentRmds){
  function(){
    title <- names(ge$source_JSON[[1]])
    # old info
    names_student <- names(ge$revisionHistory)
    for(.x in seq_along(names_student)){
      ge$revisionHistory[[.x]][[title]]$grade$old <- ge$source_JSON[[names_student[[.x]]]][[title]]$grade
      ge$revisionHistory[[.x]][[title]]$PR$old <- ge$source_JSON[[names_student[[.x]]]][[title]]$PR
    }

    for(Xstudent in names_studentRmds){
      ge$source_JSON[[Xstudent]][[title]]$tb_grade %>%
        select(contains("ans")) %>%
        rowwise() %>%
        mutate(
          total=sum(c_across())
        ) -> ge$source_JSON[[Xstudent]][[title]]$tb_grade

      ge$source_JSON[[Xstudent]][[title]]$tb_grade$total ->
        ge$source_JSON[[Xstudent]][[title]]$grade

      purrr::map_dbl(
        ge$source_JSON,
        ~{
          Xgrade <- .x[[title]]$grade
          ifelse(is.na(Xgrade), 0, Xgrade)}
      ) -> allGrades
      PR <- round(pmin((100-percent_rank(allGrades)*100)+1, 100),0)
      names(PR) <- names(ge$source_JSON)
      purrr::walk(
        seq_along(ge$source_JSON),
        ~{
          ge$source_JSON[[.x]][[title]]$PR <- PR[[.x]]
        }
      )
    }

    # Update revision
    for(.x in seq_along(names_student)){
      ge$revisionHistory[[.x]][[title]]$grade$new <- ge$source_JSON[[names_student[[.x]]]][[title]]$grade
      ge$revisionHistory[[.x]][[title]]$PR$new <- ge$source_JSON[[names_student[[.x]]]][[title]]$PR
    }
  }
}
googleDrive_uploadAsJson <- function(grade4JSON, dribble, title, filename=NULL){
  if(is.null(filename)){
   filename <- file.path(tempdir(), paste0("grade_",title,".json"))
  }

  xfun::write_utf8(
    jsonlite::toJSON(grade4JSON), con=filename
  )
  googledrive::drive_upload(
    filename,
    path = dribble,
    verbose = F, overwrite = T
  )
}
googleDrive_updateGradeAllJson <- function(grade4JSON, dribble, title){

  googleDrive_downJsonfileAsListFromDribble(dribble, filename="grade_all.json") -> grade_all

  # update/upload grade_all
  updateUpload2GoogleDrive(dribble,
                           driveFilename="grade_all.json",
                           listObject=grade_all,
                           whichElement=title,
                           whatValue=grade4JSON)
  # names(grade_all) -> currentNames
  # if(title %in% currentNames){
  #   # update
  #   grade_all[[title]] <- grade4JSON
  # } else {
  #   # append
  #   setNames(append(grade_all,
  #                   list(grade4JSON)), c(currentNames, title)) ->  grade_all
  # }
  #
  # googleDrive_uploadAsJson(
  #   grade_all, dribble, "grade_all.json"
  # )
  #
  # upload update revision

}
generate_grade4JSONuploadUpdateFun <- function(rv, dribble, .title){
  title <- .title
  function(){
    names_studentRmds <- names(rv$source_JSON)
    setNames(
      purrr::map(
        seq_along(names_studentRmds),
        ~{
          Xname <- names_studentRmds[[.x]]
          Xgrade=rv$source_JSON[[Xname]][[title]]$grade
          XPR=rv$source_JSON[[Xname]][[title]]$PR
          Xtb_grade=rv$source_JSON[[Xname]][[title]]$tb_grade
          list(
            grade=Xgrade,
            PR=XPR,
            tb_grade=Xtb_grade
          )
        }
      ),
      names_studentRmds
    ) -> grade4JSON

    googleDrive_uploadAsJson(grade4JSON, dribble, title=title)
    googleDrive_updateGradeAllJson(grade4JSON, dribble, title=title)
  }
}
generate_revisionHistory_uploadUpdateFun <- function(rv, title, dribble){
  function(){
    filename = paste0("revisionHistory_", title,".json")
    googleDrive_uploadAsJson(rv$revisionHistory, dribble, filename=filename)
  }
}

googleDrive_downJsonfileAsListFromDribble <- function(dribble, filename){
  list_gfiles <- googledrive::drive_ls(dribble)
  if(!any(list_gfiles$name==filename)){
    grade_all <- list()
    googleDrive_uploadAsJson(
      grade_all, dribble, filename
    )
  } else {
    dribble_gradeAll <- subset(list_gfiles, list_gfiles$name==filename)
    tempfileJson <- tempfile(fileext=".json")
    googledrive::drive_download(
      file=dribble_gradeAll,
      path=tempfileJson
    )
    jsonlite::fromJSON(tempfileJson) -> grade_all
  }
}
updateUpload2GoogleDrive <- function(dribble, driveFilename, listObject, whichElement, whatValue){
  names(listObject) -> currentNames
  if(whichElement %in% currentNames){
    # update
    listObject[[whichElement]] <- whatValue
  } else {
    # append
    setNames(append(listObject,
                    list(whatValue)), c(currentNames, whichElement)) ->  listObject
  }
  googleDrive_uploadAsJson(
    listObject, dribble, title=whichElement, filename=driveFilename
  )
}
generate_emailBodyContent <- function(rv, title){
  function(tb_nameEmail){
    IDs <- stringr::str_extract(names(rv$revisionHistory), "[0-9]{9}")

    list_emailBody <- vector("list", length(rv$revisionHistory))
    for(Xstudent in seq_along(rv$revisionHistory)){
      Xdata <- dplyr::filter(tb_nameEmail, ID==IDs[[Xstudent]])
      Xname <- Xdata$studentName
      bodyContent <- glue::glue('{Xname}同學好，

依據你在Github對{title}成績的dispute，最後有產生以下成績更動：\n
')
      .allLabels <- names(rv$revisionHistory[[Xstudent]][[title]])
      .ansLabels <- stringr::str_subset(.allLabels, "ans")
      purrr::reduce(
        append(list(bodyContent), as.list(c(.ansLabels, "grade","PR"))),
        function(oldContent, XansLabel){
          c(oldContent,
            XansLabel,
            paste0(rv$revisionHistory[[Xstudent]][[title]][[XansLabel]]$old,
                   " --> ",
                   rv$revisionHistory[[Xstudent]][[title]][[XansLabel]]$new),
            paste0(
              rv$revisionHistory[[Xstudent]][[title]][[XansLabel]]$comment
            ),
            "         ***         \n\n")

        }
      ) -> list_emailBody[[Xstudent]]$body
      list_emailBody[[Xstudent]]$email <- Xdata$email
    }
    rv$revisionEmails <- list_emailBody
    setNames(rv$revisionEmails, names(rv$revisionHistory))

    notifyStudentRevision(rv)
  }
}
