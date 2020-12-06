#' Create individual return folders under class google drive folder
#'
#' @param classDriveFolderPath A character of the url to the class google drive folder
#' @param roster A data frame with 學號, used to create individual return gd folder
#'
#' @return A list of created gd folder details
#' @export
#'
#' @examples none
create_individualGoogleDriveReturnFolder <- function(classDriveFolderPath, roster){
  drive_mkdir(
    name="Return",
    path=classDriveFolderPath
  ) -> gd_returnFolder

  # create individual return folders
  {
    map(
      seq_along(roster$學號),
      ~{
        drive_mkdir(
          name=roster$學號[[.x]],
          path=gd_returnFolder$drive_resource[[1]]$webViewLink
        ) -> driveResource
        list(
          driveResource=driveResource,
          gmail=roster$Gmail帳號[[.x]]
        )
      }
    )-> gd_returnFolderIndividuals
    names(gd_returnFolderIndividuals) <- roster$學號
  }
  gd_returnFolderIndividuals
}

#' Share individual google return folder to each student
#'
#' @param gd_returnFolderIndividuals A list of return from create_individualGoogleDriveReturnFolder
#'
#' @return
#' @export
#'
#' @examples none
share_gdReturnFolders <- function(gd_returnFolderIndividuals)
{
  gd_returnFolderIndividuals_shared <-
    vector("list", length(gd_returnFolderIndividuals))
  for(.x in seq_along(gd_returnFolderIndividuals))
  {
    gd_returnFolderIndividuals[[.x]]$
      driveResource$
      drive_resource[[1]]$
      webViewLink -> folder2share
    if(is.na(gd_returnFolderIndividuals[[.x]]$gmail)){
      "No valid gmail"
    } else {
      drive_share(
        file=folder2share,
        role="reader",
        emailAddress = gd_returnFolderIndividuals[[.x]]$gmail
      )
    } -> gd_returnFolderIndividuals_shared[[.x]]
  }
  names(gd_returnFolderIndividuals_shared) <- names(gd_returnFolderIndividuals)
  gd_returnFolderIndividuals_shared
}

#' Generate student-correct answer synthesized Rmd and copy to each student's return folder
#'
#' @param title A character of exercise title such as hw1, midterm1, etc
#' @param roster A data frame with 學號, Gmail帳號
#' @param correctAnsTemplateFilename A character of filepath to the correct answer Rmd template to used be used for synthesizing
#'
#' @return
#' @export
#'
#' @examples none
generateReturnRmds_then_copy2googleDriveReturnFolder2 = function(title, roster, correctAnsTemplateFilename){

  # create_returnFolder
  {
    for(.x in seq_along(roster$學號))
    {
      school_id = roster$學號[[.x]]
      personalReturnFolder <-
        file.path(.root(),"return",school_id,title)
      if(!dir.exists(personalReturnFolder)) dir.create(personalReturnFolder, recursive = T)
    }
  }

  # move return files to return folder
  {
    submissionReturnFolder <- file.path(
      .root(), "studentsSubmission", title, "return"
    )
    listRmdsInReturnFolder <-
      list.files(
        submissionReturnFolder, pattern="\\.Rmd$", full.names=T
      )

    gd_returnRmds = copy_toGoogleDriveReturnFolder(listRmdsInReturnFolder, title)

  }

  {
    cannotSynthesizedFolder <- file.path(submissionReturnFolder, "cannotSynthesized")
    # rstudioapi::selectFile(path=cannotSynthesizedFolder)
    listRmdsInCannotSynthesizedFolder <-
      list.files(
        cannotSynthesizedFolder, pattern="\\.Rmd$", full.names=T
      )
    copy_toGoogleDriveReturnFolder(listRmdsInCannotSynthesizedFolder, title) -> gd_badRmds

    copyCorrectAnsRmdToGDCannotSynthesizedFolder(
      correctAnsTemplateFilename,
      listRmdsInCannotSynthesizedFolder,
      title
    ) -> cannotSynthesizedFiles

  }

  list(
    success_returnRmd = gd_returnRmds,
    cannotSynthesized_Rmd=cannotSynthesizedFiles
  )
}

#' Generate student-correct answer synthesized Rmd and copy to each student's return folder
#'
#' @param title A character of exercise title such as hw1, midterm1, etc
#' @param roster A data frame with 學號, Gmail帳號
#' @param correctAnsTemplateFilename A character of filepath to the correct answer Rmd template to used be used for synthesizing
#' @param localGDReturnFolderPath A character of path to local google drive return folder
#'
#' @return
#' @export
#'
#' @examples none
generateReturnRmds_then_copy2googleDriveReturnFolder = function(title, roster, correctAnsTemplateFilename, localGDReturnFolderPath){

  # move return files to return folder
  {
    submissionReturnFolder <- file.path(
      .root(), "studentsSubmission", title, "return"
    )
    listRmdsInReturnFolder <-
      list.files(
        submissionReturnFolder, pattern="\\.Rmd$", full.names=T
      )

    gd_returnRmds = copy_toGoogleDriveReturnFolder(listRmdsInReturnFolder, title, localGDReturnFolderPath)

  }

  {
    cannotSynthesizedFolder <- file.path(submissionReturnFolder, "cannotSynthesized")
    # rstudioapi::selectFile(path=cannotSynthesizedFolder)
    listRmdsInCannotSynthesizedFolder <-
      list.files(
        cannotSynthesizedFolder, pattern="\\.Rmd$", full.names=T
      )
    copy_toGoogleDriveReturnFolder(listRmdsInCannotSynthesizedFolder, title, localGDReturnFolderPath) -> gd_badRmds

    copyCorrectAnsRmdToGDCannotSynthesizedFolder(
      correctAnsTemplateFilename,
      listRmdsInCannotSynthesizedFolder,
      title,
      localGDReturnFolderPath
    ) -> cannotSynthesizedFiles

  }

  list(
    success_returnRmd = gd_returnRmds,
    cannotSynthesized_Rmd=cannotSynthesizedFiles
  )
}
#' Synthersize student Rmd and answer Rmd template to form return Rmd
#'
#' @param studentRmdfilename A character of student Rmd filepath
#' @param gradeRecord A data frame from the row regarding the student
#' @param correctAnswerSynthesizer A function generated from synthersizeWithCorrectAnsFunctional
#' @param returnFolder A path for return Rmd storage
#' @param needPR default=F. Want return Rmd to include his/her ranking.
#'
#' @return
#' @export
#'
#' @examples none
synthesize_returnRmd <- function(
  studentRmdfilename, gradeRecord, correctAnswerSynthesizer, returnFolder, needPR=F
){
  # Synthesize studentRmd with correct answers
  synthesizedStudentRmd =
    {
      studentRmdfilename %>% xfun::read_utf8() -> Rmdlines

      Rmdlines %>% rmd2drake:::get_chunksTable() -> chunkTable
      rmdgrader:::get_listCodeChunksFromRmdlinesWithChunkTable(Rmdlines, chunkTable) %>% correctAnswerSynthesizer()
    }

  # Attach grades to corresponding hashtag
  {
    # synthesizedStudentRmd %>%
    #   str_which("^#") -> whichHasHashTag
    # synthesizedStudentRmd[whichHasHashTag] %>%
    #   str_which("# [:digit:]{1}") %>%
    #   whichHasHashTag[.] -> whichHasHashTag
    ansLables <- str_extract(names(gradeRecord), "ans[:graph:]+")

    # get prefix hashtag pattern for later regex match
    prefixPattern = {
      ansLabels %>%
        map_chr(
          ~ {
            .x %>%
              str_extract("(?<=ans)[:digit:]+") -> ansDigits
            ansDigits %>%
              str_split("") %>%
              .[[1]] -> digits
            prefixHashtags <-
              ifelse(length(digits) == 2,
                     paste0("^### ", digits[[1]], ".", digits[[2]]),
                     paste0("^## ", digits)
              )
          }
        )
    }

  }
  # Attach grade and form return Rmd
  returnRmd = {
    synthesizedStudentRmd -> returnRmd
    prefixPattern %>%
      map_int(
        ~ {
          str_which(returnRmd, .x)
        }
      ) -> whichHasAnsLabel

    seq_along(ansLabels) %>%
      map_chr(
        ~ {
          paste0(
            returnRmd[
              whichHasAnsLabel[[.x]]
            ],
            " (", round(gradeRecord[[ansLabels[[.x]]]], 4), ")"
          )
        }
      ) -> returnRmd[
        whichHasAnsLabel
      ]

    finalGradeReplacement <- as.character(round(gradeRecord$final,3))
    if(needPR){
      finalGradeReplacement <- paste0(finalGradeReplacement," 在100人中排名第", gradeRecord$PR)
    }

    str_replace(returnRmd, "%rawGrade%",
                as.character(round(gradeRecord$total,3))) %>%
      str_replace("%finalGrade%", finalGradeReplacement
                  ) -> returnRmd

    returnRmd
  }

  # save returnRmd
  baseRmdname <- basename(studentRmdfilename)
  returnRmdfilename <- file.path(returnFolder, baseRmdname)
  returnRmd %>%
    xfun::write_utf8(
      con = returnRmdfilename
    )

  invisible(returnRmd)
}

#' Copy list of return Rmds to their individual google drivel return folders
#'
#' @param listRmdsInReturnFolder A character of the returnRmds with full path
#' @param title A character usually the name of the exercise such as hw1, midterm1, etc...
#'
#' @return
#' @export
#'
#' @examples none
copy_toGoogleDriveReturnFolder2 <- function(listRmdsInReturnFolder, title) {
  gd_returnRmds <- vector('list',length(listRmdsInReturnFolder))
  for (.x in seq_along(listRmdsInReturnFolder))
  {
    returnRmd <- listRmdsInReturnFolder[[.x]]
    school_id <- str_extract(basename(returnRmd), "[:digit:]{9}")
    personalReturnFolder <-
      file.path(.root(), "return", school_id, title)
    if (!dir.exists(personalReturnFolder)) {
      cat("Folder not exist: ", personalReturnFolder, "... creating\n")
      dir.create(personalReturnFolder,
                 recursive=T)
      next
    }
    destfile=file.path(personalReturnFolder, basename(returnRmd))
    file.copy(
      from = returnRmd,
      to = destfile,
      overwrite = T
    )
    gd_returnRmds[[.x]] <- destfile
  }
  invisible(gd_returnRmds)
}

#' Copy list of return Rmds to their individual google drivel return folders
#'
#' @param listRmdsInReturnFolder A character of the returnRmds with full path
#' @param title A character usually the name of the exercise such as hw1, midterm1, etc...
#' @param localGDReturnFolderPath A character of path to local google drive return folder
#'
#' @return
#' @export
#'
#' @examples none
copy_toGoogleDriveReturnFolder <- function(listRmdsInReturnFolder, title, localGDReturnFolderPath) {
  gd_returnRmds <- vector('list',length(listRmdsInReturnFolder))
  for (.x in seq_along(listRmdsInReturnFolder))
  {
    returnRmd <- listRmdsInReturnFolder[[.x]]
    school_id <- str_extract(basename(returnRmd), "[:digit:]{9}")
    personalReturnFolder <-
      file.path(localGDReturnFolderPath, school_id, title)
    if (!dir.exists(personalReturnFolder)) {
      cat("Folder not exist: ", personalReturnFolder, "... creating\n")
      dir.create(personalReturnFolder,
                 recursive=T)
      next
    }
    destfile=file.path(personalReturnFolder, basename(returnRmd))
    file.copy(
      from = returnRmd,
      to = destfile,
      overwrite = T
    )
    gd_returnRmds[[.x]] <- destfile
  }
  invisible(gd_returnRmds)
}
#' Generate ansRmd with \%at\% ans and copy to individual return folder in google drive
#'
#' @param correctAnsTemplateFilename A character of the full path to the correct answer template Rmd
#' @param listRmdsInCannotSynthesizedFolder A character of the full path to the folder storing all cannotSynthesized Rmds
#' @param title A character such as hw1, midterm1, etc
#'
#' @return
#' @export
#'
#' @examples none
copyCorrectAnsRmdToGDCannotSynthesizedFolder2 <- function(
  correctAnsTemplateFilename,
  listRmdsInCannotSynthesizedFolder,
  title
)
{
  correctAnsTemplateFilename %>% xfun::read_utf8() %>%
    rmdgrader::reviseRmd_atAns() -> revisedAnsRmd
  tmpRmd = tempfile(
    str_remove(
      basename(correctAnsTemplateFilename),"\\.Rmd"),
    fileext=".Rmd"
  )
  xfun::write_utf8(
    revisedAnsRmd,
    con=tmpRmd
  )
  cannotSynthesizedFiles <- vector("list", length(listRmdsInCannotSynthesizedFolder))
  for(.x in seq_along(listRmdsInCannotSynthesizedFolder))
  {
    school_id <- str_extract(basename(listRmdsInCannotSynthesizedFolder[[.x]]), "[:digit:]{9}")
    personalReturnFolder <-
      file.path(.root(), "return", school_id, title)
    if (!dir.exists(personalReturnFolder)) {
      cat("Folder not exist: ", personalReturnFolder, "\n")
      next
    }
    returnDestfile = file.path(personalReturnFolder, basename(correctAnsTemplateFilename))
    file.copy(
      from = tmpRmd,
      to = returnDestfile,
      overwrite = T
    )
    cannotSynthesizedFiles[[.x]] <- returnDestfile
  }
  invisible(cannotSynthesizedFiles)
}

#' Generate ansRmd with \%at\% ans and copy to individual return folder in google drive
#'
#' @param correctAnsTemplateFilename A character of the full path to the correct answer template Rmd
#' @param listRmdsInCannotSynthesizedFolder A character of the full path to the folder storing all cannotSynthesized Rmds
#' @param title A character such as hw1, midterm1, etc
#' @param localGDReturnFolderPath A character of path to local google drive return folder
#'
#' @return
#' @export
#'
#' @examples none
copyCorrectAnsRmdToGDCannotSynthesizedFolder <- function(
  correctAnsTemplateFilename,
  listRmdsInCannotSynthesizedFolder,
  title,
  localGDReturnFolderPath
)
{
  correctAnsTemplateFilename %>% xfun::read_utf8() %>%
    rmdgrader::reviseRmd_atAns() -> revisedAnsRmd
  tmpRmd = tempfile(
    str_remove(
      basename(correctAnsTemplateFilename),"\\.Rmd"),
    fileext=".Rmd"
  )
  xfun::write_utf8(
    revisedAnsRmd,
    con=tmpRmd
  )
  cannotSynthesizedFiles <- vector("list", length(listRmdsInCannotSynthesizedFolder))
  for(.x in seq_along(listRmdsInCannotSynthesizedFolder))
  {
    school_id <- str_extract(basename(listRmdsInCannotSynthesizedFolder[[.x]]), "[:digit:]{9}")
    personalReturnFolder <-
      file.path(localGDReturnFolderPath, school_id, title)
    if (!dir.exists(personalReturnFolder)) {
      cat("Folder not exist: ", personalReturnFolder, "\n")
      next
    }
    returnDestfile = file.path(personalReturnFolder, basename(correctAnsTemplateFilename))
    file.copy(
      from = tmpRmd,
      to = returnDestfile,
      overwrite = T
    )
    cannotSynthesizedFiles[[.x]] <- returnDestfile
  }
  invisible(cannotSynthesizedFiles)
}
