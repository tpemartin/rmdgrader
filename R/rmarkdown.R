require(rmd2drake)
require(xfun)
require(dplyr)
require(purrr)
require(withr)
require(rlang)
require(callr)

#' Produce list of code chunks
#'
#' @param rmdlines A character vector of rmdlines
#'
#' @return A list with elements named by chunk labels and element values of rmdlines character vector segments
#' @export
#'
#' @examples none.
rmd_list_codeChunks <- function(rmdlines){
  rmd_chunkTable(rmdlines) -> chunkTable

  with(
    chunkTable,
    {
      setNames(map2(
        start, end,
        function(x, y) {rmdlines[(x+1):(y-1)]}
      ), label)
    }
  ) -> list_codeChunks
}

#' Get chunk table from rmdlines
#'
#' @description With character vectors read from an Rmd file, this function returns a data frame chunk table which tells you the start, end and chunk labels in the rmdlines
#' @param rmdlines a character vector, possibily read from xfun::read_utf8 from an Rmd file.
#'
#' @return a chunk table data frame
#' @export
#'
#' @examples
#' \dontrun{
#' filename <- "/Users/martinl/Github/course-dashboard-programming-for-data-science//grading_flow/HW8/homework8-ans.Rmd"
#' xfun::read_utf8(process$correctAnsFilename$filename) -> rmdlines
#' rmd_chunkTable(rmdlines)
#' }
rmd_chunkTable <- function(rmdlines){
  chunkTable <-{
    whichIsChunkStart <-  stringr::str_which(rmdlines,"^```\\{")
    whichCouldBeChunkEnd <- stringr::str_which(rmdlines, "^```$")
    require(dplyr)
    data.frame(
      end=whichCouldBeChunkEnd,
      cut= cut(
        whichCouldBeChunkEnd,
        breaks=c(-Inf,whichIsChunkStart,Inf))
    ) %>%
      group_by(cut) %>%
      summarise(
        end=min(end),
        cut=cut
      ) %>%
      ungroup() %>%
      mutate(
        start=as.integer(stringr::str_extract(as.character(
          cut
        ),"(?<=\\()[0-9]+"))
      ) -> chunkTable

     chunkTable$label <- unlist(stringr::str_extract(
       rmdlines[chunkTable$start],
       "(?<=\\s)\\b[^\\s,\\}\\=]+\\b(?!\\=)"))
     chunkTable$engine <- unlist(
       stringr::str_extract(
         rmdlines[chunkTable$start],
         "(?<=```\\{)\\w"
       )
     )

     chunkTable[c("part", "subseq")] <- {
       part0 <-
         stringr::str_extract(chunkTable$label,"(?<=(ans|data))[0-9]+(?=\\.)")
       partNumber <- dplyr::if_else(
         is.na(part0),
         stringr::str_extract(chunkTable$label,"(?<=(ans|data))[0-9]{1}"),
         part0
       )

       subSeq <- dplyr::if_else(
         is.na(part0),
         stringr::str_extract(chunkTable$label,"(?<=(ans|data)[0-9]{1})[0-9]+"),
         stringr::str_extract(chunkTable$label,"(?<=\\.)[0-9]+")
       )
       data.frame(
         part=partNumber,
         subseq=subSeq
       )
     }
     chunkTable$prefix <- {
       stringr::str_extract(chunkTable$label,"^(data|ans)")
     }

     chunkTable$suffix <- {
       stringr::str_extract(chunkTable$label,"[:alpha:]+$")
     }

     chunkTable
  }
  return(chunkTable)
}

get_codeChunks <- function(Rmdlines){
  require(dplyr)
  chunkTable <- Rmdlines %>% rmd2drake::get_chunksTable(exclude = "(afterMake=T|drake=F)")
  codeChunks <- Rmdlines %>% get_listCodeChunksFromRmdlinesWithChunkTable(chunkTable)
  codeChunks
}

# get_codeChunks <- function(Rmdlines){
#   require(dplyr)
#   Rmdlines %>%
#     rmd2drake::get_chunksTable(
#       exclude="(afterMake=T|drake=F)"
#     ) -> chunkTable
#   Rmdlines %>%
#     rmd2drake::get_listCodeChunksFromRmdlines(
#       requireLabel = T,
#       exclude="(afterMake=T|drake=F)") -> codeChunks
#   codeChunks
# }


# helpers -----------------------------------------------------------------

get_listCodeChunksFromRmdlinesWithChunkTable <- function(Rmdlines, chunkTable)
{
  chunkTable %>%
    filter(!is.na(object)) -> chunkTableWithLabels
  chunkTableWithLabels
  # browser()
  seq_along(chunkTableWithLabels$object) %>%
    purrr::map(
      ~{
        with(
          chunkTableWithLabels,
          {
            codes= Rmdlines[begin[[.x]]:end[[.x]]]
            codes
          }
        )
      }
    ) -> list_codeChunks

  names(list_codeChunks) <- chunkTableWithLabels$object
  list_codeChunks
}

obtain_ansObjectName <- function(x){
  x %>%
    {
      stringr::str_trim(., side="both") %>%
        purrr::keep(function(x) x!="") %>%
        {.[[length(.)]]} %>%
        stringr::str_remove("[#\\s]+") %>%
        stringr::str_split(";") %>%
        unlist()
    }
}

# obtain_ansObjectName <- function(x){
#   x %>%
#     {
#       stringr::str_trim(., side="both") %>%
#         purrr::keep(function(x) x!="") %>%
#         {.[[length(.)]]} %>%
#         stringr::str_remove("[#\\s]+")
#     }
# }

decomposeChunkLabels <- function(codeChunks){
  names_codechunks <- names(codeChunks)
  types_codechunks <-
    stringr::str_extract(names_codechunks, "^[:alpha:]+")
  digits_codechunks <-
    stringr::str_extract(names_codechunks, "[:digit:]+")
  parts_codechunks <- stringr::str_sub(digits_codechunks,1,1)
  postfix_codechunks <-
    stringr::str_extract(names_codechunks, "[:alpha:]+$")

  labels_codechunks <-
    data.frame(
      label=names_codechunks,
      type=types_codechunks,
      part=factor(parts_codechunks),
      digit=stringr::str_pad(
        digits_codechunks,
        width=2, side="right", pad="0") %>%
        as.integer(),
      postfix=postfix_codechunks
    )
  labels_codechunks
}
