#' Generate synthesizer that sythersizes both correct answer and student answer
#'
#' @description the synthesizer takes in only one input that is the codeChunkTable produced by rmd2drake::get_listCodeChunksFromRmdlines
#' @param correctAnsFilename A character of filepath to the correct answer Rmd
#' @param needToFixFront A logical. Default=T. need to replace the front of your ansfile so to show rawGrade, finalGrade etc
#' @param fullMark A numeric, default=NULL, each ans is 1pt
#'
#' @return
#' @export
#'
#' @examples none
synthersizeWithCorrectAnsFunctional <- function(correctAnsFilename, needToFixFront=T, fullMark=NULL, ...){
  argList <- list(...)
  require(dplyr)
  correctAnsFilename %>%
    xfun::read_utf8() -> rmdlines
  if(needToFixFront){
    whichIsTitle <- stringr::str_which(
      rmdlines, "^title: ")
    whichEndTheInsert <-
      stringr::str_which(
        rmdlines,
        "^請先執以下code chunk")
    rmdlines <- as.list(rmdlines)
    rmdlines[[whichIsTitle+1]] <-
      c(returnTemplate,"\n")
    rmdlines[(whichIsTitle+2):(whichEndTheInsert-1)] <- NULL
    rmdlines <- unlist(rmdlines)
  }
  # browser()
  rmdlines %>%
    rmdgrader::reviseRmd_atAns(turnInBonus=argList$turnInBonus, fullMark)-> ansRmdlines_revised

  ansRmdlines_revised %>%
    rmd2drake:::get_chunksTable() %>%
    filter(
      stringr::str_detect(object, "ans")
    ) %>%
    arrange(
      begin
    )  -> ansChunkTable

  augment_studentRmdsWithAtAnsFunctional(ansRmdlines_revised, ansChunkTable)
}

#' Generate the funtion that can combine correct answer and student answer into one Rmd
#'
#' @param ansRmdAccommodateAtAns A character of lines from ansRmd that already accommodate \%at\% ans
#' @param ansChunkTable A data frame that show code chunk mapping of ansRmdAccommodateAtAns
#'
#' @return
#' @export
#'
#' @examples none
augment_studentRmdsWithAtAnsFunctional <-
  function(ansRmdAccommodateAtAns, ansChunkTable) {
    ansChunkTable$object -> ansLabels
    ansChunkTable$begin -> ansBegins

    function(studentCodeChunks) {
      ansRmd_augmentStudent <- ansRmdAccommodateAtAns

      # .x=1
      for (.x in seq_along(ansLabels)) {
        cutLoc <- ansBegins[[.x]] - 2
        ansLabelNow <- ansLabels[[.x]]
        nLines <- length(studentCodeChunks[[ansLabelNow]])
        # ansRmd_augmentStudent[1:(ansBegins[[.x]])]

        ansRmd_augmentStudent <-
          c(
            ansRmd_augmentStudent[1:cutLoc],
            "",
            paste0("```{r ",ansLabels[[.x]],"me}"),
            studentCodeChunks[[ansLabelNow]],
            "```",
            "",
            ansRmd_augmentStudent[-c(1:cutLoc)]
          )
        ansBegins <-
          ansBegins + nLines + 4
      }
      return(ansRmd_augmentStudent)
    }
  }


#' Revise Rmd to accommodate \%at\% ans
#'
#' @param ansRmdlines A character of Rmd lines
#' @param turnInBonus A numeric
#' @param fullMark A numeric, default=NULL, each ans is 1 pt
#'
#' @return
#' @export
#'
#' @examples none
reviseRmd_atAns <- function(ansRmdlines, turnInBonus, fullMark=NULL)
{
  require(dplyr)
  require(stringr)
  # insert one empty line to assure there is line after the last code chunk.
  ansRmdlines <- c(ansRmdlines,"")
  chunkTable <- ansRmdlines %>% rmd2drake::get_chunksTable(exclude = "(afterMake=T|drake=F)")

  chunkTable$object %>%
    stringr::str_count("ans[0-9]+") %>%
    sum() -> totalAns
  if(is.null(fullMark)) totalAns -> fullMark
  effortTotal <- 10-turnInBonus
  if(turnInBonus==0){
    stringr::str_remove(ansRmdlines, "%turnInBonus% \\+ ") -> ansRmdlines
  } else {
    stringr::str_replace(ansRmdlines, "%turnInBonus%", as.character(turnInBonus)) -> ansRmdlines
  }
  # browser()
  stringr::str_replace(
    ansRmdlines, "%effortTotal%", as.character(effortTotal)
  ) -> ansRmdlines

  ansRmdlines_atAns <-
    ansRmdlines %>%
    augment_atAnsBracket(chunkTable) %>%
    setup_attachAtAns(chunkTable) %>%
    str_replace_all("%fullRawGrade%",as.character(fullMark))

}
augment_atAnsBracket <- function(rmdlines, chunkTable) {
  # content, codeChunk
  chunkTable %>%
    filter(
      stringr::str_detect(object, "ans")
    ) -> ansChunks
  positionMarks <- vector("list", length(ansChunks$object))
  for (.x in seq_along(ansChunks$object)) {
    positionMarks[[.x]] <-
      with(ansChunks, {
        c(
          begin[[.x]] - 1, begin[[.x]],
          end[[.x]], end[[.x]] + 1
        )
      })
  }
  positions <- purrr::flatten_dbl(positionMarks)
  fullPositionMarks <- c(1, positions, length(rmdlines))
  numberOfDivisions <- length(fullPositionMarks) / 2
  dim(fullPositionMarks) <- c(2, numberOfDivisions)
  # browser()
  output <- vector("list", numberOfDivisions)
  for (.x in 1:(numberOfDivisions - 1)) {
    seqX <- fullPositionMarks[1, .x]:fullPositionMarks[2, .x]
    originPart <- rmdlines[seqX]
    attachPart <-
      ifelse(.x %% 2 == 0,
             "} %at% ans",
             "{"
      )
    originPartLastValidLine <- {
      stringr::str_trim(originPart,side="both") -> originPartTemp
      stringr::str_subset(originPartTemp, "^#", negate=T) -> originPartTemp
      originPartTemp[[length(originPartTemp)]]
    }

    if(stringr::str_detect(originPartLastValidLine,"^```")){
      output[[.x]] <- c(
        originPart,
        "{"
      )
    } else {
      output[[.x]] <- c(
        originPart,
        "} %at% ans"
      )
    }

  }
  output[[numberOfDivisions]] <-
    rmdlines[
      fullPositionMarks[1, numberOfDivisions]:
        fullPositionMarks[2, numberOfDivisions]
    ]

  newRmdlines <-
    purrr::flatten_chr(output)
  return(newRmdlines)
}



setup_attachAtAns <- function(rmdlines, chunkTable){
  which(chunkTable$object=="setup") -> whichIsSetup
  c(
    rmdlines[1:chunkTable$end[[whichIsSetup]]],
    "library(econDS)",
    "ans <- new.env()",
    rmdlines[-c(1:chunkTable$end[[whichIsSetup]])]
  )
}


# Return  -----------------------------------------------------------------
#' Return instance environment generator
#'
#' @description Initiate a return process instance.
#'
#' @param pe An process instance from Process instance initiator
#' @param returnFolderpath A path under which all students' return folders lie in
#'
#' @return
#' @export
#'
#' @examples none
Return <- function(pe, returnFolderpath, PR=NULL, extraGradeComputationBatch=""){
  re <- new.env()
  re$template$rmdlines <- xfun::read_utf8(correctAnsFilename)

  re$template$generate_holder <- function(){
    whichHasThePattern <- stringr::str_which(
      re$template$rmdlines,
      "^(---|```|#+\\s+[0-9.]+)"
    )
    whatIsThePattern <- na.omit(stringr::str_extract(
      re$template$rmdlines,
      "^(---|```|#+\\s+[0-9.]+)"
    ))
    data.frame(
      pattern=whatIsThePattern,
      location=whichHasThePattern) -> re$template$placeholder$divisionTable

    re$template$placeholder$divisionTable %>%
      filter(pattern=="```") %>%
      pull(location)-> breaks
    # breaks
    cut(seq_along(re$template$rmdlines),
        breaks=c(-Inf, breaks, Inf),
        right = F) -> rmdlinesLocCuts
    purrr::map(
      levels(rmdlinesLocCuts),
      ~subset(
        re$template$rmdlines,
        rmdlinesLocCuts==.x)
    ) -> list_rmdlines_cut
    purrr::map(
      list_rmdlines_cut,
      ~stringr::str_extract(.x[[1]],
                            "(?<=(```\\{[a-zA-Z]\\s))\\b[:graph:]+\\b")
    ) -> list_rmdlines_cutLabels
    re$template$placeholder$rmdlines_cut <-  setNames(list_rmdlines_cut, list_rmdlines_cutLabels)
  }


  # build each studentRmd environment
  re$studentRmds <- list()
  names_studentRmds <- names(pe$studentsRmds)
  purrr::walk(
    names_studentRmds,
    ~{
      re$studentRmds[[.x]] <- new.env(parent = re)
    }
  )
 # browser()
  re$placeholderAnsElementNames <- {
    re$template$generate_holder()
    na.omit(stringr::str_extract(
      names(re$template$placeholder$rmdlines_cut),
      "^ans[:graph:]+"))
  }
  # .it <-5
  purrr::walk(
    seq_along(names_studentRmds),
    ~{
      re$studentRmds[[names_studentRmds[[.x]]]]$returnRmd_generate <-
        generate_returnRmd(returnFolderpath,re, pe, names_studentRmds[[.x]], .x)
    }
  )

  # attach gradeText_generate methods
  ansLabels <- na.omit(stringr::str_extract(names(re$template$placeholder$rmdlines_cut),"^ans[:graph:]+"))
  # ansLabels
  aeNames <- names(records_gradeComment[[1]])
  aeNames
  setNames(
    purrr::transpose(records_gradeComment),
    stringr::str_replace(aeNames, "ae", "ans")) ->
    tr_records_gradeComment
  purrr::transpose(tr_records_gradeComment) -> records_gradeComment
  # browser()
  purrr::walk(
    names_studentRmds,
    ~{
      X_Pr=ifelse(is.null(PR), NULL, PR[[.x]])
      re$studentRmds[[.x]]$gradeText_generate <-
        generate_gradeSectionContentFunction(
          .x,
          ansLabels,
          re,
          PR=X_Pr
        )
    }
  )

  re$inBatch$generate_returnRmd <- generate_returnRmdInBatch(re, extraGradeComputationBatch=extraGradeComputationBatch)
  re$inBatch$return <- generate_returnInBatch(re)
  re$inBatch$delete <- generate_deleteInBatch(re)


  # browser()
  return(re)
}

#' Record student grades and comments as a list of many students
#'
#' @param all_aeObjects A character vector of ae for each ansXX
#' @param envir An environment where all_aeObjects names exist
#'
#' @return a list of many students, each is a list of many ansXX which has 3 elements, timestamp, grade, comment
#' @export
#'
#' @examples none
record_gradesCommentsWithTimestamp <- function(all_aeObjects, envir){
  # browser()
  purrr::map(
    all_aeObjects,
    ~{
      print(.x)
      Xae <- get(.x, envir)
      extract_grades_commentsX(Xae)
    }
  ) -> allae_grades_comments
  names(allae_grades_comments) <- all_aeObjects
  purrr::transpose(allae_grades_comments) -> tr_allae_grades_comments
  return(tr_allae_grades_comments)
}


extract_grades_commentsX <- function(Xae){
  Xae$result$table_messageGroups
  Xae$result$table_messageGroups -> Xtable
  Xtable_unnest <- tidyr::unnest(Xtable, cols="Rmds")
  Xtable_unnest

  eachRmdResults <- vector("list", length(Xtable_unnest$Rmds))
  setNames(purrr::map(
    seq_along(eachRmdResults),
    ~{
      list(list(
        time=timestamp(quiet = T),
        grade=Xtable_unnest$grade[[.x]],
        comment=ifelse(
          length(Xtable_unnest$comment[[.x]])==0,
          "", Xtable_unnest$comment[[.x]])
      ))
    }
  ), Xtable_unnest$Rmds) -> eachRmdResults

  return(eachRmdResults)
}

generate_returnRmd <- function(returnFolderpath, re, pe, Xnames_studentRmds, .it){
  function(extraGradeComputation =""){
    placeholderAnsElementNames <- re$placeholderAnsElementNames
    re$studentRmds[[Xnames_studentRmds]]$returnRmd <- list()
    re$studentRmds[[Xnames_studentRmds]]$returnRmd$lines <-
      {
        re$template$placeholder$rmdlines_cut -> placeholder0
        placeholder0[placeholderAnsElementNames] <-
          pe$studentsRmds[[Xnames_studentRmds]]$codeChunksProcessed$list_codeChunks[placeholderAnsElementNames]
        placeholder0
      }
    xList <- pe$studentsRmds[[Xnames_studentRmds]]$codeChunksProcessed$list_codeChunks[placeholderAnsElementNames]
    yList <- pe$correctAnsFilename$codeChunksProcessed$list_codeChunks[placeholderAnsElementNames]
    # browser()
    purrr::map2(
        xList,
        yList,
      function(.x, .y){ c(
        .x,
        "\n\n#' 參考解答 ------------------\n",
        "{",
        .y,
        "} %at% ans"
      )}
    ) ->
      re$studentRmds[[Xnames_studentRmds]]$returnRmd$lines[placeholderAnsElementNames]

    XreturnRmd <-re$studentRmds[[Xnames_studentRmds]]$returnRmd$lines
    # remove redundant ```
    purrr::map(
      XreturnRmd,
      ~{
        if(stringr::str_detect(.x[[1]], "```")) .x[-1] else .x
      }
    ) -> XreturnRmd

    # rebuild rmdlines for returnRmd
    names_lines <- na.omit(names(XreturnRmd))
    purrr::map(
      names_lines,
      ~{
        c(
          glue::glue("```{r <<.x>>}", .open="<<", .close = ">>"),
          XreturnRmd[[.x]],
          "```"
        )
      }
    ) -> XreturnRmd[names_lines]
    # browser()
    whichHasGradeSection <- stringr::str_which(XreturnRmd[[1]],"^##\\s*(成績|Grade)")
    if(length(whichHasGradeSection)==1){
      # browser()
      re$studentRmds[[Xnames_studentRmds]]$gradeText_generate(extraGradeComputation)
      XreturnRmd[[1]] <-
        c(
          XreturnRmd[[1]][1:whichHasGradeSection],
          "
```{r grade}
",
          re$studentRmds[[Xnames_studentRmds]]$returnRmd$gradeText,
          "
```
",
          XreturnRmd[[1]][-c(1:whichHasGradeSection)]
        )
    }
    re$studentRmds[[Xnames_studentRmds]]$returnRmd$lines <- paste0(unlist(XreturnRmd))

    # attach returnMethod
    XstudentID <- stringr::str_extract(Xnames_studentRmds,"[0-9]{9,}")
    XreturnFolder4HW <- file.path(returnFolderpath, XstudentID, params$title)
    if(!dir.exists(XreturnFolder4HW)) dir.create(XreturnFolder4HW)
    XreturnRmdfilepath <-
      file.path(XreturnFolder4HW, Xnames_studentRmds)

    re$studentRmds[[Xnames_studentRmds]]$returnRmd$return <- generate_returnMethods(Xnames_studentRmds, re, re$studentRmds[[Xnames_studentRmds]]$returnRmd$lines, XreturnRmdfilepath)

  }
}
generate_gradeSectionContentFunction <-
function(XstudentRmd, ansLabels, re, PR=NULL) {
  records_gradeComment[[XstudentRmd]] -> Xrecords_gradeComment
  function(extraGradeComputation = "") {
    purrr::map(
      seq_along(Xrecords_gradeComment),
      ~ {
        Xrecords_gradeComment[[.x]][[1]][c("grade", "comment")]
      }
    ) -> XgradeComment
    names(XgradeComment) <- ansLabels

    tr_XgradeComment <- purrr::transpose(XgradeComment)
    tr_XgradeComment <- purrr::map(tr_XgradeComment, unlist)
    grade <- as.data.frame(tr_XgradeComment)
    gradeChr <- glue::glue("df_grade <- \n\tjsonlite::fromJSON('{jsonlite::toJSON(grade)}')")

    how2computeTotalGrade <-
      if(extraGradeComputation==""){
        'totalGrade <- list()
within(
  totalGrade,
  {
    rawGrade = sum(df_grade$grade)
    finalGrade = rawGrade/nrow(df_grade)*7+3
  }
) -> totalGrade'
      } else {
        extraGradeComputation
      }
    PRtext <-
      ifelse(is.null(PR), "", glue::glue("PR = {PR}\nprint(PR)"))
    gradeSectionContent <-
      c(
        gradeChr,
        how2computeTotalGrade,
        PRtext,
        "print(df_grade)
print(totalGrade)
library(econDS); ans <- new.env(parent=.GlobalEnv) #對答案用",
"# 如果沒有econDS, 請先remotes::install_github(\"tpemartin/econDS\", force=T)"
      )

    gradeSectionContent -> re$studentRmds[[XstudentRmd]]$returnRmd$gradeText
  }
}
generate_returnRmdInBatch <- function(re, extraGradeComputationBatch){
  function(){
    purrr::walk(
      seq_along(re$studentRmds),
      ~{
        if(extraGradeComputationBatch==""){
          re$studentRmds[[.x]]$returnRmd_generate()
        } else {
          re$studentRmds[[.x]]$returnRmd_generate(extraGradeComputationBatch)
        }

      }
    )
  }
}
generate_returnInBatch <- function(re){
  function(){
    purrr::walk(
      seq_along(re$studentRmds),
      ~{
        re$studentRmds[[.x]]$returnRmd$return()
      }
    )
  }
}
generate_deleteInBatch <- function(re){
  function(){
    purrr::walk(
      seq_along(re$studentRmds),
      ~{
        re$studentRmds[[.x]]$returnRmd$delete()
      }
    )
  }
}
generate_returnMethods <- function(XstudentRmd, re, lines, filepath) {
  function() {
    xfun::write_utf8(
      lines,
      con = filepath
    )
    re$studentRmds[[XstudentRmd]]$returnRmd$delete <- function() {
      file.remove(
        filepath
      )
    }
  }
}

