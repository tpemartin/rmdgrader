#' Execute Rmd scripts and obtain answer object values
#'
#' @param studentsRmds A character of full filename path of Rmds
#' @param correctAnsFilename A character of full filename path of correct Answer Rmds
#'
#' @return A list of 2
#' @export
#'
#' @examples none
get_answerObjectValues <- function(studentsRmds, correctAnsFilename)
{
  studentValues <- vector("list", length(studentsRmds))
  names(studentValues) <- basename(studentsRmds)
  # get Rmd structure information from answer Rmd
  {
    correctAnsFilename %>%
      get_codeChunkProcessed_from_filepath(codeChunksFromAnsFile=T) -> correctCodeChunksProcessed

    # 大題標號
    parts <- levels(correctCodeChunksProcessed$chunkLabelsDecomposed$part)

    # get ansObjNames
    ansObjectNames <<- correctCodeChunksProcessed$ansObjectnames

  }

  badRmds <- vector("list", length(parts))
  names(badRmds) <- parts
  for(.y in seq_along(parts)){
    unprocessableRmds <- c()
    # if(.y==3) browser()
    # 產生某一大題回答值
    targetPart <- parts[[.y]]
    # targetLabels
    targetLabels <-
      {
        correctCodeChunksProcessed$chunkLabelsDecomposed %>%
          filter(type=="ans", part==targetPart) %>% pull(label)
      }
    # browser()
    # prepare dataEnvironment for the part
    # browser()
    prepare_dataEnvironment(correctAnsFilename, part=targetPart) ->> dataEnvironment

    {
      # 針對某一份Rmd[[.x]]
      # .x=32
      # whichIsTheTarget <- str_which(studentsRmds,"410874230")
      totalRmds = length(studentsRmds)
      for(.x in seq_along(studentsRmds))
      {
        # if(.y==2 && .x==whichIsTheTarget) browser()
        studentFilename <- studentsRmds[[.x]]
        isStudentRmd <- str_detect(basename(studentFilename), "ans", negate=T)
        isAnsFile <- ifelse(stringr::str_detect(studentFilename,"ans"), T, F)
        cat('.x = ',.x," /",totalRmds,"; ",basename(studentFilename),"\n")


        # Process student code chunks
        tryCatch(
          {
            studentFilename %>%
              get_codeChunkProcessed_from_filepath()
          },
          error = function(e) {
            warning(studentFilename, " has format problem")
            studentFilename
          }
        ) -> codeChunksProcessed

        # 檢查codeChunks是否為合理可被執行，不合理flag_wrong_content=T
        flag_wrong_content <-
          tryCatch({
            !all(targetLabels %in% names(codeChunksProcessed[["chunkExpressions"]]))},
            error=function(e){
              T
            })
        if(flag_wrong_content){
          warning("Rmd file content is incorrect")
          unprocessableRmds <- c(unprocessableRmds, studentsRmds[[.x]])
          next
        }

        # 執行合理codeChunks
        answerEnvironment <<- new.env(parent=dataEnvironment)
        # browser()
  #### Core step: 執行各chunk並存下答案物件值

          fillupDataEnv_with_ansEnvir(codeChunksProcessed, targetPart, answerEnvironment, isAnsFile=isAnsFile, isStudentRmd=isStudentRmd )


        # browser()
        studentValues[[.x]] <- append(
          studentValues[[.x]],
          answerEnvironment$ansValues)
      }
    }
    badRmds[[.y]] <- unprocessableRmds
  }
  list(
    studentValues=studentValues,
    badRmds=badRmds
  )
}

tryCatch_codeExpressions2 <- function(answerCodeExpressions, answerEnvironment){
  tryCatch({
    purrr::walk(
      answerCodeExpressions,
      eval, envir=answerEnvironment
    )
    T
  },
  error=function(e){
    "Error: codes cannot be processed"
    F
  }) -> flag_executable
  invisible(flag_executable)
}
tryCatch_codeExpressions <- function(answerCodeExpressions, answerEnvironment){
  tryCatch({
    answerEnvironment$answerCodeExpressions <- answerCodeExpressions
    rlang::with_env(
      answerEnvironment,
      {
        purrr::walk(
          answerCodeExpressions,
          eval
        )
      }
    )
    # purrr::walk(
    #   answerCodeExpressions,
    #   eval, envir=answerEnvironment
    # )
    T
  },
  error=function(e){
    "Error: codes cannot be processed"
    F
  }) -> flag_executable
  invisible(flag_executable)
}

parse_codeChunks <- function(codeChunks){
  purrr::map(
    codeChunks,
    parseCodeChunk2Expressions)
}

get_ansObjectnames <- function(codeChunks) {
  whichHasAnsObj <- stringr::str_which(names(codeChunks), "ans[:digit:]+(?![cs])")
  codeChunks[whichHasAnsObj] %>%
    purrr::map(obtain_ansObjectName) -> ansObjectnames
  whichHasAnsObj <- stringr::str_which(names(codeChunks), "ans[:digit:]+(?![cs])")
  codeChunks[whichHasAnsObj] %>%
    purrr::map(obtain_ansObjectName) -> ansObjectnames
}

processDataExprs2getDataEnvironment <- function(
  dataLabels, codeChunksProcessed, dataEnvironment
){
  # browser()
  dataExpressions <- {
    codeChunksProcessed$chunkExpressions[dataLabels]
  }
  # generate dataEnvironment
  {
    dataExpressions %>%
      walk(
        function(x){
          walk(
            x,
            eval, envir=dataEnvironment
          )
        }
      )
  }

}
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
Evaluate <- function(pe){
  require(stringr)
  # pe <- process
  pe <- as.environment(pe)
  ee <- new.env(parent = pe)

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
  ee$save <- save_objectValues(ee)

  ee
}

# helpers -----------------------------------------------------------------

parseCodeChunk2Expressions <- function(x){
  tryCatch({
    x %>%
      paste0(collapse="\n") %>%
      rlang::parse_exprs()
  },
  error=function(e){
    "codes can not be parsed to expression"
    rlang::expr('Error chunk')
  })
}
get_allRmds <- function(pe){
  c(
    pe$correctAnsFilename$basename,
    map_chr(
      seq_along(pe$studentsRmds),
      ~{pe$studentsRmds[[.x]]$basename}
    )
  )
}
get_running_sequence <- function(pe){
  pe$correctAnsFilename$codeChunksProcessed$chunkTable %>%
    {
      list(
        setup_library =
          stringr::str_subset(.$label, regex("(setup|library)", ignore_case = T)),
        parts = stringr::str_subset(.$label, regex("(setup|library)", ignore_case = T), negate = T)
      )
    } ->
    list_setup_parts

  ct <- pe$correctAnsFilename$codeChunksProcessed$chunkTable
  ct %>%
    mutate(
      part=if_else(is.na(part), label, part)
    ) %>%
    group_by(part) %>%
    tidyr::nest() -> ct

  ct %>%
    filter(
      stringr::str_detect(part, "setup|library")
    ) -> tbl_ct
  if(nrow(tbl_ct)!=0){
    {tbl_ct$data[[1]]$label} -> setup_labels
  } else {
    NULL -> setup_labels
  }


  ct %>%
    filter(
      stringr::str_detect(part, "setup|library", T)
    ) -> ct_parts

  purrr::map(
    1:nrow(ct_parts),
    ~{
      list(
        labels=c(setup_labels,
        ct_parts[.x,]$data[[1]]$label)
      )
    }
  ) %>%
    setNames(paste0("part",ct_parts$part))
}
generate_sequenceOfEnvironment_basedOnSeqLabels <- function(seq_labels){
  #
  ansEnv <- list()
  for(.x in seq_along(seq_labels))
  {
    parentAnsE <-
      if(.x == 1) .GlobalEnv else ansEnv[[.x-1]]

    ansEnv[[.x]] <- new.env(parent=parentAnsE)
  }
  setNames(ansEnv, seq_labels)
}
generate_sequenceOfEnvironment_basedOnSeqLabelsFunctional <-
  function(seq_labels){
    function(){
      #
      ansEnv <- vector("list", length(seq_labels))
      for(.x in seq_along(seq_labels))
      {
        parentAnsE <-
          if(.x == 1) .GlobalEnv else ansEnv[[.x-1]]

        ansEnv[[.x]] <- new.env(parent=parentAnsE)
      }
      setNames(ansEnv, seq_labels)
    }
  }
generate_environment4eachRunningSequence <- function(pe, ee){
  running_sequence <- get_running_sequence(pe)
  map(
    running_sequence,
    ~ {
      .x <- as.environment(.x)
      parent.env(.x) <- ee
      .x
    }
  ) -> runningSequenceEnvironments

  walk(
    runningSequenceEnvironments,
    ~{
      # runningSeqEnvironment
      .x$generate_runningSeqEnvironments <-
        function(){
          generate_sequenceOfEnvironment_basedOnSeqLabels(
            .x$labels
          )
        }
    }
  )
  runningSequenceEnvironments
}
attach_run_correctAnsFunctions <- function(runningSeqEnvironments, pe){
  corrAnsFrame <- pe$correctAnsFilename
  xlist_codeChunks <- corrAnsFrame$codeChunksProcessed$list_codeChunks

  walk(
    runningSeqEnvironments,
    ~{
      activeLabels <- .x$labels
      activeAnswerEnvs <-
        .x$generate_runningSeqEnvironments()

      .x$run_correctAns <- function(){
        purrr::walk(
          seq_along(activeLabels),
          ~ {
            activeCodeText <-
              xlist_codeChunks[[activeLabels[[.x]]]]
            activeEvalEnvir <-
              activeAnswerEnvs[[activeLabels[[.x]]]]
            if(stringr::str_detect(activeLabels[[.x]],"s$")){
              activeEvalEnvir$codes <- activeCodeText
            } else {
              eval(
                parse(text = activeCodeText),
                envir = activeEvalEnvir
              )
            }
          }
        )

        activeAnswerEnvs ->
          .x$corrAnsEnvironments
      }
    }
  )
}
extract_ansObjnames <- function(pe){
  ansLabels <- stringr::str_subset(
    names(pe$correctAnsFilename$codeChunksProcessed$list_codeChunks),"^ans")
  ansObjnames <- vector("character", length(ansLabels))
  ansObjnames <-
    purrr::map_chr(
      seq_along(ansLabels),
      ~ {
        XansLabel <- ansLabels[[.x]]

        if (stringr::str_detect(XansLabel, "s$")) {
          "codes"
        } else {
          Xcodes <- pe$correctAnsFilename$codeChunksProcessed$list_codeChunks[[XansLabel]]
          whichHasAnsObjname <- max(stringr::str_which(stringr::str_trim(Xcodes, side = "left"), "^#"))
          ansObjname <-
            stringr::str_extract(Xcodes[[whichHasAnsObjname]], "\\b[:graph:]+\\b")
          ansObjname
        }
      })
  setNames(ansObjnames, ansLabels)
}
detach_runningSequence <- function(){
  attached <- search()
  toDetach <- stringr::str_subset(attached, "running_sequence")
  exprToDetach <- parse(text=toDetach)
  purrr::walk(
    seq_along(exprToDetach),
    ~eval(rlang::expr(detach(!!exprToDetach[[.x]])))
  )
}
ansValueResolveFunctional <- function(.part, tempEnv, ee, pe, basenameRmd){
  function(inBatch=F) {
    # browser()
    tempEnv[[.part]]$generate_runningSeqEnvironments() ->
      tempEnv[[.part]]$resolved

    list_ansLabels_setup_dataLabels <-
      get_ansLabels_setupdataLabels(ee, .part = .part)
    ansLabels = list_ansLabels_setup_dataLabels$ansLabels
    setup_dataLabels = list_ansLabels_setup_dataLabels$setup_dataLabels

    # attach setup and data
    if(!inBatch && length(setup_dataLabels)!=0){
      purrr::walk(
        setup_dataLabels,
        ~ attach(
          ee$running_sequence[[.part]]$corrAnsEnvironments[[.x]]
        )
      )
    }

    ee$answerValues[[basenameRmd]]$values[[.part]] <-
        setNames(vector("list", length(ansLabels)), ansLabels)
    for (.it in seq_along(ansLabels))
    {
      # .it <- 1
      Xlabel <- ansLabels[[.it]]
      # get codes from one Rmd
      Xcodes <- pe$studentsRmds[[basenameRmd]]$codeChunksProcessed$list_codeChunks[[Xlabel]]

      XansObjname <- ee$ansObjectnames[[Xlabel]]
      if(stringr::str_detect(Xlabel, "s$")){
        XansObjname <- "codes"
        objValue <- Xcodes
      } else {
        objValue <-
          tryEvaluate_Xcodes(
            Xcodes,
            resolvingEnvironment = tempEnv[[.part]]$resolved[[Xlabel]])
      }

      ee$answerValues[[basenameRmd]]$values[[.part]][Xlabel] <-
        list(
          Xlabel=list(objValue)
        )
    }
    if(!inBatch) detach_runningSequence()

  }
}
generate_ansValuesResolvingFunction <- function(pe, ee, basenameRmd){
  # basenameRmd <- "HW8_410678019.Rmd"
  ee$.tempEnv <-
    generate_environment4eachRunningSequence(
      pe, new.env(parent=ee)
    )
  partNames <- names(ee$running_sequence)
  partNames
  setNames(
    purrr::map(
      seq_along(partNames),
      ~ansValueResolveFunctional(.x, ee$.tempEnv, ee, pe, basenameRmd)
    ),
    partNames
  ) -> ee$answerValues[[basenameRmd]]$resolve
}
generate_answerValueBatchResolveFunction <- function(pe, ee,Ypartname)
{
  # correctRunningSequenceLabels <- ee$running_sequence[[Ypartname]]$labels
  # setup_dataLabels <- stringr::str_subset(
  #   correctRunningSequenceLabels, "(data|setup)"
  # )
  list_ansLabels_setup_dataLabels <-
    get_ansLabels_setupdataLabels(ee, .part = Ypartname)
  setup_dataLabels = list_ansLabels_setup_dataLabels$setup_dataLabels

  function(){
    stringr::str_subset(
      names(ee$answerValues),
      "batch", T
    ) -> allStudentRmds
    # names(ee$running_sequence) -> allPartnames

    # attach library and setup, and data
    {
      purrr::walk(
        setup_dataLabels,
        ~ attach(
          ee$running_sequence[[Ypartname]]$corrAnsEnvironments[[.x]]
        )
      )
    }

    for(.it in seq_along(allStudentRmds))
    {
      XstudentRmds <- allStudentRmds[[.it]]
      ee$answerValues[[XstudentRmds]]$resolve[[Ypartname]](inBatch=T)
    }


    # detach library and setup, and data
    detach_runningSequence()
  }
}

save_objectValues <- function(ee) {
  function(filename) {
    whichIsBatch <- stringr::str_which(names(ee$answerValues), "batch")
    purrr::map(
      ee$answerValues[-whichIsBatch],
      ~ purrr::flatten(.x$values)
    ) -> objectValues

    # browser()
    ee$objectValues <- objectValues

    whichIsAns <- stringr::str_which(names(objectValues),"ans")

    correctValues <- objectValues[[whichIsAns]]
    studentValues <- objectValues[-whichIsAns]
    # purrr::map(
    #   objectValues,
    #   ~{
    #    if(is.null(.x)) .x else
    #     purrr::flatten(.x)
    #   }
    # ) -> objectValues
    allRmds <- ee$allRmds
    save(correctValues,studentValues , allRmds, file = filename)
  }
}

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

get_ansLabels_setupdataLabels <- function(ee, .part){
  correctRunningSequenceLabels <- ee$running_sequence[[.part]]$labels
  setup_dataLabels <- stringr::str_subset(
    correctRunningSequenceLabels, "(data|setup)"
  )
  setdiff(
    correctRunningSequenceLabels,
    setup_dataLabels
  ) -> ansLabels
  list(
    setup_dataLabels=setup_dataLabels,
    ansLabels=ansLabels
  )
}
generate_resolutionMethods4correctAnsBasename <- function(.part, correctAnsBasename, ee){
  partnames <- names(ee$running_sequence)
  Xpartname <- partnames[[.part]]
  list_ansLabels_setup_dataLabels <-
    get_ansLabels_setupdataLabels(ee, .part = Xpartname)
  ansLabels = list_ansLabels_setup_dataLabels$ansLabels
  setup_dataLabels = list_ansLabels_setup_dataLabels$setup_dataLabels
  function(...){
    ee$running_sequence[[Xpartname]]$run_correctAns()
    ee$answerValues[[correctAnsBasename]]$values[[.part]] <-
      setNames(vector("list", length(ansLabels)), ansLabels)
    for (.it in seq_along(ansLabels))
    {
      # .it <- 1
      Xlabel <- ansLabels[[.it]]
      if(stringr::str_detect(Xlabel, "s$")){
        XansObjname <- "codes"
      } else {
        XansObjname <- ee$ansObjectnames[[Xlabel]]
      }

      # produce objValue
      {
        chr2eval <-
        paste0(
          "ee$running_sequence[[.part]]$corrAnsEnvironments[[Xlabel]]$",
          XansObjname)
      try(
        eval(
          parse(text=chr2eval)
        ), silent=T
      ) -> objValue

      if(is(objValue, "try-error")) objValue <- NULL
      }

      objValue ->
        ee$answerValues[[correctAnsBasename]]$values[[.part]][[Xlabel]][[1]]
    }
  }

}

tryEvaluate_Xcodes <- function(
  Xcodes, resolvingEnvironment
){
  isXcodesNULL <- FALSE
  cannotParseXcodes <- FALSE
  parsedXcodesCannotBeEvaluated <- FALSE
  isXcodesNULL <- is.null(Xcodes)
  if(!isXcodesNULL){
    tryResult_parsing <- try(parse(text=Xcodes), silent=T)
    cannotParseXcodes <- is(tryResult_parsing, "try-error")
  }
  if(!isXcodesNULL && !cannotParseXcodes){
    Xexpression <- rlang::expr(
      eval(tryResult_parsing, envir = resolvingEnvironment)
    )
    tryResult_evaltheParsed <- try(rlang::eval_bare(Xexpression), silent = T)
    parsedXcodesCannotBeEvaluated <- is(tryResult_evaltheParsed, "try-error")
  }

  if(isXcodesNULL){
    objValue <- "Xcodes is null."
  } else
  if(cannotParseXcodes){
      objValue <- "Xcodes cannot be parsed."
  } else
  if(parsedXcodesCannotBeEvaluated){
    objValue <- "Parsed Xcodes can not be evaluated."
  } else {
    objValue <- tryResult_evaltheParsed
  }
  return(objValue)
}
