require(rmd2drake)
require(xfun)
require(dplyr)
require(purrr)
require(withr)
require(rlang)
require(callr)

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
        stringr::str_remove("[#\\s]+")
    }
}

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
