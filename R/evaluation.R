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
    ) %>%
    {.$data[[1]]$label} -> setup_labels

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
      activeAnswerEnvs <- .x$generate_runningSeqEnvironments()

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

    correctRunningSequenceLabels <- ee$running_sequence[[.part]]$labels
    setup_dataLabels <- stringr::str_subset(
      correctRunningSequenceLabels, "(data|setup)"
    )
    setdiff(
      correctRunningSequenceLabels,
      setup_dataLabels
    ) -> ansLabels
    # attach setup and data
    if(!inBatch){
      purrr::walk(
        setup_dataLabels,
        ~ attach(
          ee$running_sequence[[.part]]$corrAnsEnvironments[[.x]]
        )
      )
    }

    ee$answerValues[[basenameRmd]]$values <-
      setNames(vector("list", length(ansLabels)), ansLabels)
    for (.it in seq_along(ansLabels))
    {
      # .it <- 1
      Xlabel <- ansLabels[[.it]]
      # get codes from one Rmd
      Xcodes <- pe$studentsRmds[[basenameRmd]]$codeChunksProcessed$list_codeChunks[[Xlabel]]
      Xexpression <- rlang::expr(
        eval(parse(text = Xcodes), envir = tempEnv[[.part]]$resolved[[Xlabel]])
      )
      tryResult <- try(rlang::eval_bare(Xexpression), silent = T)

      XansObjname <- ee$ansObjectnames[[Xlabel]]
      if (is(tryResult, "try-error")) {
        tempEnv[[.part]]$resolved[[Xlabel]][[XansObjname]] <- tryResult
      }

      tempEnv[[.part]]$resolved[[Xlabel]][[XansObjname]] ->
        ee$answerValues[[basenameRmd]]$values[[Xlabel]]
    }
    if(!inBatch) detach_runningSequence()


    # tempEnv[[.part]] -> tempEnv[[.part]]$resolvedEnvironment
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
  correctRunningSequenceLabels <- ee$running_sequence[[Ypartname]]$labels
  setup_dataLabels <- stringr::str_subset(
    correctRunningSequenceLabels, "(data|setup)"
  )
  function(){
    stringr::str_subset(
      names(ee$answerValues),
      "batch|ans", T
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
