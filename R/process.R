#' Initiator for Rmd processing service
#'
#' @return an processing instance environment
#' @export
#'
#' @examples
#' \dontrun{
#' process <- Process()
#' }
Process <- function(){
  pe <- new.env(parent=.GlobalEnv)
  pe$correctAnsFilename <- list(
    filename=file.path(
      .root(),params$ansfilename
    ),
    basename=basename(params$ansfilename)
  )
  pe$correctAnsFilename$tryGet_list_codeChunks =
    tryGet_list_codeChunksFunctional(
      pe,"correctAnsFilename",
      pe$correctAnsFilename$filename, filenameAsElementName=F, codeChunksFromAnsFile=T)


  pe$studentsRmds <- list()

  population_peWithStudentsRmds(pe)

  pe$.preProcessing <- function(.fn=NULL){
    if(!is.null(.fn)) .fn(pe)
  }

  pe$processingRmds <- processingRmds

  pe$inBatch_studentsRmds <- list(
    tryGet_list_codeChunks = function() {
      # whichLeftOut <- which(names(pe$studentsRmds) == ".batch_tryGet_list_codeChunks")
      purrr::walk(
        # pe$studentsRmds[-whichLeftOut],
        pe$studentsRmds,
        ~ {
          .x$tryGet_list_codeChunks()
        }
      )
    }
  )
  return(pe)
}

processingRmds <- function(silent=F){
  if(!silent) message('If there is preprocessing need, such as changing student submissions file content, \nuse pe$.preProcessing(.fn) with .fn, a function object that can take pe as input, \nbefore pe$processingRmds() or redo pe$processingRmds() after every .preProcessing. ')

  pe$allRmds <- c(
    pe$correctAnsFilename,
    pe$studentsRmds
  )

  .GlobalEnv$objectValues <- get_answerObjectValues(pe$allRmds, pe$correctAnsFilename)

  pe$destfile=file.path(.root(),params$gradingFolder, params$title,"data4step3.Rdata")

  pe$save <- function(){
    save(.GlobalEnv$objectValues, pe$allRmds, file=pe$destfile)
  }
}
population_peWithStudentsRmds <- function(pe){
  studentsRmds <-  list.files(
    file.path(.root(), params$submissionFolder, params$title)
    , full.names = T) %>%
    stringr::str_subset("\\.Rmd$")
  purrr::map(
    seq_along(studentsRmds),
    ~list(
      filename=studentsRmds[[.x]],
      basename=basename(studentsRmds[[.x]]),
      tryGet_list_codeChunks= tryGet_list_codeChunksFunctional(pe,"studentsRmds",studentsRmds[[.x]], codeChunksFromAnsFile=F)
    )
  ) -> list_studentRmds
  pe$studentsRmds = setNames(list_studentRmds, basename(studentsRmds))
}
tryGet_list_codeChunksFunctional <- function(pe, objectname, filename, codeChunksFromAnsFile=F, filenameAsElementName=T){
  function(){
    if(filenameAsElementName){
      pe[[objectname]][[basename(filename)]]$codeChunksProcessed <- tryGet_list_codeChunks(filename, codeChunksFromAnsFile)
    } else {
      pe[[objectname]]$codeChunksProcessed <- tryGet_list_codeChunks(filename, codeChunksFromAnsFile)
    }

  }
}
tryGet_list_codeChunks_bk <- function(filename, codeChunksFromAnsFile){
  # filenames <- process$studentsRmds
  # filename <- filenames[[1]]

  # xfun::read_utf8(filename) -> rmdlines
  #
  # try(
  #   get_listCodeChunksFromRmdlines(rmdlines, requireLabel = T),
  #   silent=T) ->
  #   list_codeChunks

  try(
    get_codeChunkProcessed_from_filepath(filename, codeChunksFromAnsFile=codeChunksFromAnsFile),
    silent=T) ->
    codeChunksProcessed


  if(is(codeChunksProcessed,"try-error")){
    return(list(NULL))
  } else {
    return(codeChunksProcessed)
  }
}
tryGet_list_codeChunks <- function(filename, codeChunksFromAnsFile){
  # filenames <- process$studentsRmds
  # filename <- filenames[[1]]

  # xfun::read_utf8(filename) -> rmdlines
  #
  # try(
  #   get_listCodeChunksFromRmdlines(rmdlines, requireLabel = T),
  #   silent=T) ->
  #   list_codeChunks

  rmdlines <- xfun::read_utf8(filename)
  if(codeChunksFromAnsFile)
  {
    return(list(
      chunkTable = rmd_chunkTable(rmdlines),
      list_codeChunks = rmd_list_codeChunks(rmdlines)
    ))
  } else {
    try(
      list(
      list_codeChunks = rmd_list_codeChunks(rmdlines)),
      silent = T) -> result
    if(is(result,"try-error")){
      return(list(NULL))
    } else {
      return(result)
    }
  }
}

