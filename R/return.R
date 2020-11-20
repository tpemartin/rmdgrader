#' Generate synthesizer that sythersizes both correct answer and student answer
#'
#' @description the synthesizer takes in only one input that is the codeChunkTable produced by rmd2drake::get_listCodeChunksFromRmdlines
#' @param correctAnsFilename A character of filepath to the correct answer Rmd
#'
#' @return
#' @export
#'
#' @examples none
synthersizeWithCorrectAnsFunctional <- function(correctAnsFilename){
  require(dplyr)
  correctAnsFilename %>%
    xfun::read_utf8() %>%
    rmdgrader::reviseRmd_atAns()-> ansRmdlines_revised

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
#'
#' @return
#' @export
#'
#' @examples none
reviseRmd_atAns <- function(ansRmdlines)
{
  require(dplyr)
  # insert one empty line to assure there is line after the last code chunk.
  ansRmdlines <- c(ansRmdlines,"")
  chunkTable <- ansRmdlines %>% rmd2drake::get_chunksTable(exclude = "(afterMake=T|drake=F)")

  ansRmdlines_atAns <-
    ansRmdlines %>%
    augment_atAnsBracket(chunkTable) %>%
    setup_attachAtAns(chunkTable)
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

  output <- vector("list", numberOfDivisions)
  for (.x in 1:(numberOfDivisions - 1)) {
    seqX <- fullPositionMarks[1, .x]:fullPositionMarks[2, .x]
    originPart <- rmdlines[seqX]
    attachPart <-
      ifelse(.x %% 2 == 0,
             "} %at% ans",
             "{"
      )
    output[[.x]] <-
      c(originPart, attachPart)
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

