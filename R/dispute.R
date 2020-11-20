#' Update tb_grades and target file Rmd under the hood
#'
#' @param tb_grades A data frame from step3 grading
#' @param targetFile A character of full path to a target dispute Rmd
#'
#' @return A data frame tb_grades new
#' @export
#'
#' @examples none.
update_tbGradesAndTargetRmdFile <- function(tb_grades, targetFile)
{
  xfun::read_utf8(
    file.path(targetFile)
  ) -> rmdlines


  disputeFileInfo %>%
    keep(
      ~{str_detect(.x$markRmd, basename(targetFile))}
    ) -> list_detect

  # produce df newGrades
  newGrades <- list()
  for(.x in seq_along(list_detect[[1]]$prefixPattern))
  {
    list_detect[[1]]$prefixPattern[[.x]] -> .pattern
    whichHasPrefixPattern <- str_which(rmdlines, .pattern)
    rmdlines[[whichHasPrefixPattern]] -> targetLine
    if(str_detect(targetLine,"(R)")){
      # already reviewed
      ansLabel <- str_extract(targetLine, "(?<=#[\\s]{0,1})[0-9][\\.]+[0-9]") %>%
        str_remove("\\.") %>%
        paste0("ans",.)
      newGrade <- str_extract(targetLine,"\\([\\.[0-9]]+\\)") %>%
        str_remove_all("[\\(\\)]") %>%
        as.numeric()
      newGrades[[ansLabel]] <- newGrade
    }
  }
  newGrades <- as.data.frame(newGrades)

  # update tb_grades
  if(ncol(newGrades)!=0)
  {
    tb_grades$name %>%
      str_which(basename(targetFile)) -> whichHasTheTargetRecord

    ansLabels <- names(newGrades)
    tb_grades[whichHasTheTargetRecord, ansLabels] <-
      newGrades[ansLabels]
    allAnsLabels <- names(tb_grades) %>% str_subset("ans")
    newTotal <-
      sum(tb_grades[whichHasTheTargetRecord,allAnsLabels])
    tb_grades$total[[whichHasTheTargetRecord]] <- round(newTotal,4)
    newFinal <- 3+newTotal*7/10
    tb_grades$final[[whichHasTheTargetRecord]] <- round(newFinal,4)

    ## update rmdlines
    {
      rmdlines %>%
        str_which("rawGrade:") -> whichHasRawGrade
      rmdlines[[whichHasRawGrade]] %>%
        str_replace('[\\.[0-9]]+', as.character(round(newTotal,4))) -> rmdlines[[whichHasRawGrade]]
      rmdlines %>%
        str_which("finalGrade:") -> whichHasFinalGrade
      rmdlines[[whichHasFinalGrade]] %>%
        str_replace('[\\.[0-9]]+', as.character(round(newFinal,4))) -> rmdlines[[whichHasFinalGrade]]

      xfun::write_utf8(
        rmdlines,
        con=targetFile
      )
    }

  }

  return(tb_grades)
}

#' Generate dispute files from issue comments
#'
#' @param issueComments A list of issue comments returned by obtainIssueComments()
#' @param commentTarget A list of comment return from thread$comments_url$get_comments()
#' @param title A character of exercise title, such as hw1, midterm1
#' @param sourceReturnFolder A character of the path where the returned Rmds are stored
#' @param disputeFolder A character of the path where the generated dispute files are to be placed at
#' @param overwrite A logical vector of 2, first element T= overwrite duplicated return Rmd, second element T= overwrite dispute content Rmd#'
#' @return
#' @export
#'
#' @examples none
generate_disputeFilesFromIssueComments <- function(
  issueComments,
  title,
  sourceReturnFolder,
  disputeFolder,
  overwrite=c("dupReturnRmd"=F, "disputeContentRmd"=F)){
  disputeFileInfo <- vector("list", length(issueComments))
  for(.x in seq_along(issueComments)){
    commentTarget = issueComments[[.x]]
    result =
      generate_disputeFilesFromOneComment(
        commentTarget = commentTarget,
        title = title,
        sourceReturnFolder = sourceReturnFolder,
        disputeFolder = disputeFolder,
        overwrite=overwrite) # overwrite[[1]]=T overwrite copied return Rmd for revision, overwrite[[2]]=T overwrite dispute content Rmd

    disputeFileInfo[[.x]] <- result
  }
  disputeFileInfo %>%
    keep(~{length(.x$markRmd)!=0}) -> disputeFileInfo

  disputeFileInfo
}

#' Obtain issue comments
#'
#' @param owner A character of github username
#' @param repo A character of repo name
#' @param titleKeyword A character of issue title keyword
#' @param .label A character of label of issue to look for, default="grievance".
#'
#' @return A list of comments
#' @export
#'
#' @examples none
obtainIssueComments <- function(
  owner,
  repo,
  titleKeyword,
  .label="dispute"
){
  require(gitterhub)
  gh <- githubService()
  list_issues <- gh$list_issues(
    owner = owner, repo = repo,
    query = list(labels = .label)
  )

  list_issues %>%
    keep(
      ~ {
        str_detect(.x$title, titleKeyword)
      }
    ) -> targetIssue

  issue_number <- targetIssue[[1]]$number

  thread <- get_issueWithGetCommentsAttached(owner = owner, repo = repo, issue_number = issue_number)

  comments <- thread$comments_url$get_comments()
  # str_split(comments[[2]][["body"]], "\n") %>% {
  #   .[[1]]
  # } -> commentVector
  #
  # commentVector

  comments
}

# helpers -----------------------------------------------------------------

#' Generate marked Rmd and its dispute Rmd for dispute resolution
#'
#' @param commentTarget A list of comment return from thread$comments_url$get_comments()
#' @param title A character of exercise title, such as hw1, midterm1
#' @param sourceReturnFolder A character of the path where the returned Rmds are stored
#' @param disputeFolder A character of the path where the generated dispute files are to be placed at
#' @param overwrite A logical vector of 2, first element T= overwrite duplicated return Rmd, second element T= overwrite dispute content Rmd
#'
#' @return
#' @export
#'
#' @examples none
generate_disputeFilesFromOneComment <- function(commentTarget, title, sourceReturnFolder, disputeFolder, overwrite=c(F,F)){
  {
    str_split(commentTarget[["body"]], "\n") %>%
      {.[[1]]} -> commentVector

    # school_id, dispute ansLabels
    {
      whichHasSchoolId <- str_which(commentVector, "學號")
      school_id <- str_extract(commentVector[[whichHasSchoolId]], "[0-9]{9}")
      ansLabels <- str_extract(commentVector, "ans[0-9]+") %>% na.omit() %>% unique()
    }
    result = list(
      markRmd=character(),
      disputeBriefing=character(),
      prefixPattern=list()
    )
    {
      # get prefix hashtag pattern for later regex match
      prefixPattern <- {
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

      result$prefixPattern <-
        as.list(prefixPattern)
    }


    sourceFolder <- file.path(sourceReturnFolder, school_id, title)
    destRmd <-
      file.path(sourceFolder, paste0(params$title, "_", school_id, ".Rmd"))

    # file.edit(destRmd)

    # mark dispute questions

    if(file.exists(destRmd)){
      destRmd %>%
        xfun::read_utf8() -> Rmdlines
# browser()
      for (.x in seq_along(prefixPattern))
      {
        Rmdlines %>%
          str_which(prefixPattern[[.x]]) -> whichHasDispute
        Rmdlines[[whichHasDispute]] %>%
          paste0(" (*)") -> Rmdlines[[whichHasDispute]]
      }

      markRmdfilename <-
        file.path(
          disputeFolder, basename(destRmd)
        )
      if(!file.exists(markRmdfilename) || overwrite[[1]]){
        Rmdlines %>%
          xfun::write_utf8(
            con = markRmdfilename
          )
      }


      result$markRmd=markRmdfilename

    }
  }
  {
    disputeBriefingFilename <-
      file.path(
        disputeFolder,
        paste0(
          title, "Dispute_",
          school_id, ".Rmd"
        )
      )

    if(!file.exists(disputeBriefingFilename) ||  overwrite[[2]]){
      commentVector %>%
        xfun::write_utf8(
          con = disputeBriefingFilename
        )
    }


    result$disputeBriefing=disputeBriefingFilename
    # file.edit(disputeBriefingFilename)
  }

  result
}
