#' Initiate an Evaluation instance after Process instance is done
#'
#' @param pe A Process instance that has been processed.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pe <- Process()
#' ei <- Evalutate(pe)
#' }
Evaluate2 <- function(pe){
  require(stringr)
  require(purrr)
  require(dplyr)
  # pe <- process
  pe <- as.environment(pe)
  ee <- new.env(parent=pe)

  ee$allRmds <- get_allRmds(pe)

  # generate environments for each running sequence
  # debug(generate_environment4eachRunningSequence)
  ee$running_sequence <- rmdgrader:::generate_environment4eachRunningSequence(pe, ee)

  # each environment is equipped with a sequential answer environment generator. To use, see the following examples:
  # ee$running_sequence$part12$generate_runningSeqEnvironments() # generate a new sequential answer environments for part12
  # ee$running_sequence$part13$generate_runningSeqEnvironments() # generate a new sequential answer environments for part13

  # activeEE <- ee$running_sequence$part12
  runningSeqEnvironments <- ee$running_sequence
  rmdgrader:::attach_run_correctAnsFunctions(
    runningSeqEnvironments = runningSeqEnvironments,
    pe
  )

  # # Once run_correctAns() attached, users can run correct answer codes under each running sequence as the following:
  # ee$running_sequence$part12$run_correctAns() #-> corrAnsEnvironments
  # ee$running_sequence$part12$corrAnsEnvironments$ans12.1$diagX
  #
  # ee$running_sequence$part13$run_correctAns()
  # ee$running_sequence$part13$corrAnsEnvironments$ans13.1s$codes
  # ee$running_sequence$part13$corrAnsEnvironments$ans13.2s$codes

  # add ansObjectnames
  ee$ansObjectnames <- extract_ansObjnames(pe)

  # browser()
  # generate correctAnsEnvironments
  purrr::walk(
    ee$running_sequence,
    ~{
      .x$run_correctAns()
    }
  )

  # create answerValues
  ee$answerValues <-
    setNames(vector("list", length(ee$allRmds)), ee$allRmds)

  # add resolve methods to every Rmds
  purrr::walk(
    ee$allRmds,
    ~generate_ansValuesResolvingFunction(pe, ee, basenameRmd = .x)
  )

  # add batch resolution method
  # browser()
  names(ee$running_sequence) -> allPartnames
  for(.y in seq_along(allPartnames))
  {
    Ypartname <- allPartnames[[.y]]
    ee$answerValues$batch$resolve[[Ypartname]] <-
      generate_answerValueBatchResolveFunction(pe, ee, Ypartname)
  }

  # attach
  attach_run_correctAnsFunctions(ee$running_sequence, pe)
  partnames <- names(ee$running_sequence)
  correctAnsBasename <- pe$correctAnsFilename$basename
  for(.part in seq_along(partnames)){
    ee$answerValues[[correctAnsBasename]]$resolve[[partnames[[.part]]]] <-
      generate_resolutionMethods4correctAnsBasename(.part, correctAnsBasename, ee)
  }

  # attach save method
  ee$export <- export_objectValues(ee)

  ee
}

# helpers -----------------------------------------------------------------


export_objectValues <- function(ee) {
  function(path="") {
    filename=file.path(path, "evaluated.Rdata")
    whichIsBatch <- stringr::str_which(names(ee$answerValues), "batch")
    purrr::map(
      ee$answerValues[-whichIsBatch],
      ~ purrr::flatten(.x$values)
    ) -> objectValues

    # browser()
    ee$objectValues <- objectValues

    whichIsAns <- stringr::str_which(names(objectValues),"ans")
    evaluated <- list()
    evaluated$correctValues <- objectValues[[whichIsAns]]
    evaluated$studentValues <- objectValues[-whichIsAns]
    # purrr::map(
    #   objectValues,
    #   ~{
    #    if(is.null(.x)) .x else
    #     purrr::flatten(.x)
    #   }
    # ) -> objectValues
    evaluated$allRmds <- ee$allRmds
    message(
      "evaluated saved at ", filename, "\n"
    )
    saveRDS(evaluated, file=filename)
    invisible(evaluated)
    # save(correctValues,studentValues , allRmds, file = filename)
  }
}

