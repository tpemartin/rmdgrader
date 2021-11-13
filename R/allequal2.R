#' Initiate allequal service 2
#' @description This is an enhanced version of allequalService. The instance is equipped with file.edit method to bring out student Rmd, and print_code method to print any particular code of any student's Rmd
#'
#' @param process a Process instance
#' @param path The path where student Rmds sit.
#'
#' @return
#' @export
#'
#' @examples none
allequalService2 <- function(process, path="")
{
  assertthat::assert_that(
    exists("correctValues", envir = .GlobalEnv)
    && exists("studentValues", envir = .GlobalEnv),
    msg="no correctValues or studentValues in the global environment"
  )
  assertthat::assert_that(
    exists("allRmds", envir = .GlobalEnv),
    msg="no allRmds in the global environment"
  )
  ansLabels <- names(correctValues)
  ae <- new.env(parent = .GlobalEnv)

  if(
    !exists("mgetxy", ae, inherits = F)){
    allRmds_noAns <-
      allRmds |>
      stringr::str_subset(
        "ans", negate=T
      )
    .GlobalEnv$mgetxy <-
      rmdgrader::generate_mgetxy(allRmds_noAns)
  }
  for(ansLabel in ansLabels){
    ae[[ansLabel]] <- rmdgrader::allequalService(ansLabel)

    ae[[ansLabel]]$generate_xy4messageGroups(.GlobalEnv$mgetxy)
  }
  rmdgrader:::attach_file.edits(ae, path)

  for(ansLabel in ansLabels){
    attach_file.edits2AnsLabel(ae, ansLabel)
    ae[[ansLabel]]$code <- get_ansCodes(process, ansLabel)
    attach_listCodeByGroup(ae, ansLabel)
  }


  return(ae)
}
generate_file.edits_basedOn_group_Rmds <- function(ae,group_Rmds){
  purrr::map(
    seq_along(group_Rmds),
    ~function(){
      ae$file.edits[[group_Rmds[[.x]]]]()
    }
  ) |>
    setNames(group_Rmds) -> list_file.edits
  return(list_file.edits)
}
generate_file.edits_basedOn_groupNumber_ansLabel <- function(
  ae,
  ansLabel, groupNumber
){
  ae[[ansLabel]]$result$messageGroups[[.x]]$Rmds -> group_Rmds
  groupName <- paste0("G", groupNumber)
  ae[[ansLabel]]$file.edit[[groupName]] <-
    list(
      groupName=list_file.edits
    )
  generate_file.edits_basedOn_group_Rmds(ae, group_Rmds)
  ae[[ansLabel]]$file.edit[[groupName]]$random <- function(){
    sample(ae[[ansLabel]]$file.edit[[groupName]][group_Rmds], 1)[[1]] -> file.editX
    file.editX()
  }
}
generate_file.edits_givenAnsLabel <- function(ae, ansLabel){
  seq_along_groups <-
    seq_along(ae[[ansLabel]]$result$messageGroups)

  for(.x in seq_along_groups){
    generate_file.edits_basedOn_groupNumber_ansLabel(
      ae,
      ansLabel,
      groupNumber = .x
    )
  }

}
file.edit_generate <- function(ae){
  names(ae) -> ansLabels
  for(ansLabel in ansLabels){
    ae[[ansLabel]]$generate_file.edit <- function(){
      generate_file.edits_givenAnsLabel(ae, ansLabel)
    }
  }
}
attach_file.edits2eachAnsLabel <- function(ae){
  names(ae) |> stringr::str_subset("ans") -> ansLabels
  # .x=1
  # seq_ansLabels = seq_along(ansLabels)
  for(.x in seq_along(ansLabels))
  {
    ae[[ansLabels[[.x]]]]$get_file.edits <- function(){
      attach_file.edits2AnsLabel(ae, ansLabels[[.x]])
    }
  }
}
attach_file.edits2AnsLabel <- function(ae, ansLabel){
  seq_groupNumbers <- seq_along(ae[[ansLabel]]$result$messageGroups)
  # .x = seq_groupNumbers[[1]]
  for(.x in seq_groupNumbers){
    groupName = paste0("G", .x)
    ae[[ansLabel]]$file.edits[[groupName]] <- {
      groupRmds <- ae[[ansLabel]]$result$messageGroups[[.x]]$Rmds
      names(ae$file.edits) -> allRmds
      whichBelong2Group <- which(allRmds %in% groupRmds)
      ae[[ansLabel]]$file.edits[[groupName]] <-
        ae$file.edits[whichBelong2Group]
    }
  }
}
print_code_functional <- function(process, studentRmd, ansLabel){
  function(){
      process$studentsRmds[[studentRmd]]$codeChunksProcessed$list_codeChunks[[ansLabel]] |>
        paste(collapse="\n") -> zz

      cat(zz)

      invisible(zz)
  }
}
get_ansCodes <- function(process, ansLabel){
  purrr::map(
    process$studentsRmds,
    ~{
      .x$codeChunksProcessed$list_codeChunks[[ansLabel]] |>
        paste0(collapse = "\n") -> content
      function(){
        cat(content)
        invisible(content)
      }
    }
  ) -> list_codes
  list_codes
}
list_groupsOfAnsLabel <- function(ae, ansLabel){
  ae[[ansLabel]]$result$messageGroups |>
    purrr::map(
      ~{.x$Rmds}
    ) -> list_RmdsByGroup
  groupNames =  paste0("G", seq_along(list_RmdsByGroup))
  names(list_RmdsByGroup) <- groupNames
  return(list_RmdsByGroup)
}
attach_listCodeByGroup <- function(ae, ansLabel){
  list_RmdsByGroup <-
    rmdgrader:::list_groupsOfAnsLabel(ae, ansLabel)
  purrr::map(
    list_RmdsByGroup,
    ~ae[[ansLabel]]$code[.x]
  ) -> list_codesByGroup
  ae[[ansLabel]]$code[names(list_codesByGroup)] <-
    list_codesByGroup
}
