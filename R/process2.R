#' Initiator for Rmd processing service
#'
#' @return an processing instance environment
#' @export
#'
#' @examples
#' \dontrun{
#' process <- Process2()
#' }
Process2 <- function(ansRmd, path_studentRmds){
  .root <- rprojroot::is_rstudio_project$make_fix_file()

  pe <- new.env(parent=.GlobalEnv)
  if(file.exists(ansRmd)){
    filename= ansRmd
  } else {
    filename = file.path(
      .root(),ansRmd
    )
  }
  pe$correctAnsFilename <- list(
    filename=filename,
    basename=basename(ansRmd)
  )
  pe$correctAnsFilename$tryGet_list_codeChunks =
    tryGet_list_codeChunksFunctional(
      pe,"correctAnsFilename",
      pe$correctAnsFilename$filename, filenameAsElementName=F, codeChunksFromAnsFile=T)


  pe$studentsRmds <- list()

  studentRmds <- list.files(
    path_studentRmds, full.names= T
  ) |> stringr::str_subset("\\.Rmd$")
  population_peWithStudentsRmds(pe, studentRmds)

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

  # add file.edit method
  pathname <- dirname(pe$studentsRmds[[2]]$filename)
  studentRmds <- names(pe$studentsRmds)
  pe$file.edit <- mfile.editFunctional(studentRmds, pathname)

  # add move2problem method
  purrr::walk(
    seq_along(pe$studentsRmds),
    ~{pe$studentsRmds[[.x]]$move2problem <-
      generate_move2problemFunction(problemFolder, process, .x)}
  )

  pe$export <- process_export(pe)

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
generate_move2problemFunction <- function(problemFolder, pe, .it) {
  function() {

    pe$.nullify <- function(){
      # null the Rmd
      pe$studentsRmds[[.it]] <- NULL
      pe$file.edit[[.it]] <- NULL
    }

    if (!dir.exists(problemFolder)) dir.create(problemFolder)
    file.copy(
      from = pe$studentsRmds[[.it]]$filename,
      to = file.path(
        problemFolder, pe$studentsRmds[[.it]]$basename
      ), overwrite = T
    )
    file.remove(
      pe$studentsRmds[[.it]]$filename
    )
    message(
      glue::glue("file {pe$studentsRmds[[.it]]$basename} is moved to the following folder:\n\n{problemFolder}\n\nto reverse the operation, move the file back and initiate the Process instance again.\n\nDon't forget to pe$.nullify() before moving to Evaluation stage")
    )
  }
}
process_export <- function(process){
  function(path=""){
    process_exported <- list()
    process_exported$studentsRmds <- process$studentsRmds
    process_exported$correctAnsFilename <- process$correctAnsFilename
    saveRDS(process_exported, file=file.path(path, "processed.Rds"))
    message("process_exported saved at ",file.path(path, "processed.Rds"),"\n")
    invisible(process_exported)
  }
}
