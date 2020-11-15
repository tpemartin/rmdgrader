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
  ansRmdlines <- xfun::read_utf8(params$correctAnsFilepath)
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

