get_codeChunkProcessed_from_filepath <- function(filename, codeChunksFromAnsFile=F){
  Rmdlines <- xfun::read_utf8(filename)
  require(dplyr)
  codeChunks = {
    Rmdlines %>%
      get_codeChunks()
  }
  if(codeChunksFromAnsFile){
    map(
      list(
        chunkExpressions=parse_codeChunks,
        chunkLabelsDecomposed=decomposeChunkLabels,
        ansObjectnames=get_ansObjectnames
      ),
      exec,
      !!!list(codeChunks=codeChunks)
    ) -> codeChunksProcessed
  } else {
    map(
      list(
        chunkExpressions=parse_codeChunks,
        chunkLabelsDecomposed=decomposeChunkLabels
      ),
      exec,
      !!!list(codeChunks=codeChunks)
    ) -> codeChunksProcessed
  }
  codeChunksProcessed
}
