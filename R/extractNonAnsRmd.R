# library(rmdgrader)
# library(rmd2drake)
# rprojroot::is_rstudio_project$make_fix_file() -> .root
# ansFilename <-
#   file.path(
#     .root(), "homeworks", "homework4-ans.Rmd"
#   )

#' Generate homework or exam Rmd for students from ans Rmd
#' @description ansXX label code chunks will be processed. Block programming will preserve the outer most block. The last line will always be the markoff answer object based on the object shown in the last line in ans Rmd
#'
#' @param ansFilename A character of full path to the ans Rmd
#'
#' @return A list of processed chunks. When unlist, it will be the student Rmd content.
#' @export
#'
#' @examples none
generate_studentRmdFromAnsRmd <- function(ansFilename, destfolderPath="."){
  require(dplyr); require(stringr); require(purrr)
  list_chunks <- get_listChunks(ansFilename = ansFilename)
  ansLabels <- str_subset(names(list_chunks), "ans")
  reduceList <- list(list_chunks) %>% append(as.list(ansLabels))

  reduce(reduceList, update_listChunks4students) -> list_updatedChunks

  saveFilename <- str_remove(basename(ansFilename), "-ans(?=\\.Rmd)")
  browser()
  destfile <- file.path(
    destfolderPath, basename(saveFilename)
  )
  unlist(list_updatedChunks) %>%
    xfun::write_utf8(
      con=destfile
    )
  invisible(list_updatedChunks)
}

# helpers -----------------------------------------------------------------

get_listChunks <- function(ansFilename){
  xfun::read_utf8(
    ansFilename
  ) -> rmdlines

  require(dplyr)
  rmd2drake::get_chunksTable(rmdlines) -> chunkTables
  chunkTables %>%
    filter(
      str_detect(object,"ans")
    ) -> chunkTables

  lineNumbers <- seq_along(rmdlines)
  endBreaks <- c((chunkTables$begin-1) ,chunkTables$end)
  lineCuts <-
    cut(lineNumbers, breaks=c(-Inf, endBreaks, Inf))

  phraseTarget <- paste0("(",paste0(chunkTables$end, collapse="|"),")")

  regexPattern <- paste0(phraseTarget,"\\]$")
  regexPattern
  whichBelongs2ansLabels <- str_which(levels(lineCuts), regexPattern)
  levels(lineCuts)[whichBelongs2ansLabels] <- chunkTables$object

  allLevels <- levels(lineCuts)
  map(
    seq_along(allLevels),
    ~{
      whichIsX <- which(lineCuts == allLevels[[.x]])
      rmdlines[whichIsX]
    }
  ) -> list_chunks
  names(list_chunks) <- allLevels
  list_chunks
}

update_listChunks4students <- function(list_chunks, ansLabel){
  list_chunks[[ansLabel]] %>%
    str_remove_all("\\s") %>%
    str_which("", negate=F) %>%
    {list_chunks[[ansLabel]][.] }-> allvalidLines

  if(str_detect(allvalidLines[[1]], "<-$") && str_detect(allvalidLines[[2]], "^\\{")){

    str_which(allvalidLines, "\\}$") %>% max() -> whichCloseBlock

    allvalidLines[
      c(1,2,whichCloseBlock, length(allvalidLines))
    ]
  } else {
    allvalidLines[[length(allvalidLines)]]
  }-> allvalidLines

  # if last line does not have #, and #
  allvalidLines[[length(allvalidLines)]] %>%
    {ifelse(str_detect(., "^#"), . , paste0("# ",.))} ->
    allvalidLines[[length(allvalidLines)]]

  list_chunks[[ansLabel]] <- allvalidLines
  list_chunks
}


# generate_studentRmdFromAnsRmd(ansFilename)
# file.edit("test.Rmd")
