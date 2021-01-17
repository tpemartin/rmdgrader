setup_library <- function(codeChunksProcessed, dataEnvironment){
  dataEnvironment$setupText <- codeChunksProcessed$chunkExpressions$setup
  with_env(
    dataEnvironment,
    {
      setupText %>%
        map_chr(
          deparse
        ) %>%
        {stringr::str_which(.,"library")} -> whichHasLibrary

      setupText[whichHasLibrary] %>%
        walk(
          eval
        )
    }
  )
}

fillup_dataEnvironment <- function(envir, correctAnsFilepath, targetPart){
  correctAnsFilepath %>%
    get_codeChunkProcessed_from_filepath(codeChunksFromAnsFile=T) -> codeChunksProcessed
  # browser()
  setup_library(codeChunksProcessed,
                dataEnvironment = envir)

  codeChunksProcessed$chunkLabelsDecomposed %>%
    filter(part == targetPart) -> targetPartLabels

  if(any(targetPartLabels$type=="data")) {
    # dataEnvironment = new.env(parent=.GlobalEnv)

    dataLabels <- {
      targetPartLabels %>%
        filter(
          type=="data"
        ) %>%
        .$label
    }
    processDataExprs2getDataEnvironment(
        dataLabels,
        codeChunksProcessed,
        dataEnvironment = envir
      )
  }
  # attach ansObjectnames to envir
  purrr::map(
    codeChunksProcessed$ansObjectnames,
    ~stringr::str_remove_all(.x,"\\s")
  ) -> codeChunksProcessed$ansObjectnames

  envir$ansObjectnames <- codeChunksProcessed$ansObjectnames
  invisible(envir)
}


fillupDataEnv_with_ansEnvir <- function(codeChunksProcessed, targetPart, answerEnvironment, ...){
  argList <- list(...)

  # .x=2
  {
    {
      library(dplyr)
      library(rmdgrader)
      codeChunksProcessed$chunkLabelsDecomposed %>%
        filter(part ==targetPart) -> targetPartLabels

      targetPartLabels %>%
        filter(
          type=="ans"
        ) %>%
        arrange(digit) %>%
        pull(label) -> ansLabels
    }
    {
      # answerEnvironment <- new.env(parent = dataEnvironment)
      answerEnvironment[["ansValues"]] <- list()
      for(.x in seq_along(ansLabels)){
        targetAnsLabel <- ansLabels[[.x]]
        # if(targetAnsLabel=="ans61") browser()
        answerCodeExpressions <-
          codeChunksProcessed$chunkExpressions[[targetAnsLabel]]

        switch(
          targetAnsLabel %>% get_ansLabelType,
          "s"={ # 程式陳述正確與否的題目
            answerEnvironment$ansValues[[targetAnsLabel]] <- list(
              answerCodeExpressions
            )
          },
          "b"={ # 可借用正確答案之environment

            # Create borrwoEnvironment from correct answers
            if(length(argList)!=0 && argList$isAnsFile) createBorrowEnvironment(answerEnvironment, targetAnsLabel)
            cat('target label: ', targetAnsLabel, '\n')
            borrowEnvName <- as.name(paste0("borrowEnv_", targetAnsLabel))

            # clone borrowEnvironment for local evaluation
            borrowEnvExpr <- rlang::expr(borrowEnv <- rlang::env_clone(!!borrowEnvName))
            eval(borrowEnvExpr)

            # clone answerEnvironment to do original scenario evaluation to preserve student answer environment continuity
            answerEnvironment_clone <-
              rlang::env_clone(answerEnvironment)

            # execute in borrowEnvironment and update ansValues in answerEnvironment
            flag_executable_borrow <-
              tryCatch_eval_inAnsEnv(
                answerCodeExpressions,
                borrowEnv)
            recordAnsObjValues_withFlagExecutable(
              flag_executable_borrow,targetAnsLabel,
              saveEnv=answerEnvironment,
              evalEnv=borrowEnv)

            # execute in answerEnvironment, but no need to update ansValues in answerEnvironment
            flag_executable_ansClone <-
              tryCatch_eval_inAnsEnv(
                answerCodeExpressions,
                answerEnvironment)


            # if(flag_executable){
            #
            #   answerEnvironment$ansValues[[targetAnsLabel]] <- list(get_ansObjectValueFromAnswerEnvironment(borrowEnv, targetAnsLabel))
            #
            # } else {
            #   answerEnvironment$ansValues[[targetAnsLabel]] %>%
            #     append(list("Error"))
            # }
          },
          "NA"={
            cat('target label: ', targetAnsLabel, '\n')
            # if(targetAnsLabel=="ans31") browser()
            flag_executable <-
              tryCatch_eval_inAnsEnv(
                answerCodeExpressions,
                answerEnvironment)

            if(flag_executable){

              # if(targetAnsLabel=="ans131") browser()
              answerEnvironment$ansValues[[targetAnsLabel]] <- list(get_ansObjectValueFromAnswerEnvironment(answerEnvironment, targetAnsLabel,
                                                                                                            isStudentRmd=argList$isStudentRmd))

            } else {
              answerEnvironment$ansValues[[targetAnsLabel]] %>%
                append(list(NA))
            }
          })
      }
    }

  }
}
fillupDataEnv_with_ansEnvir2 <- function(codeChunksProcessed, targetPart){

  # .x=2
  {
    answerEnvironment <- new.env(parent=dataEnvironment)
    answerEnvironment$codeChunksProcessed <- codeChunksProcessed
    answerEnvironment$targetPart <- targetPart

    answerEnvironment$get_ansLabelType <- get_ansLabelType
    answerEnvironment$tryCatch_codeExpressions <- tryCatch_codeExpressions
    browser()
    with_env(
      answerEnvironment,
      {
        {
          library(dplyr)
          library(rmdgrader)
          codeChunksProcessed$chunkLabelsDecomposed %>%
            filter(part ==targetPart) -> targetPartLabels

          targetPartLabels %>%
            filter(
              type=="ans"
            ) %>%
            arrange(digit) %>%
            pull(label) -> ansLabels
        }
        # answerEnvironment <- new.env(parent = dataEnvironment)
        answerEnvironment[["ansValues"]] <- list()
        for(.x in seq_along(ansLabels)){
          targetAnsLabel <- ansLabels[[.x]]
          answerCodeExpressions <-
            codeChunksProcessed$chunkExpressions[[targetAnsLabel]]

          switch(
            targetAnsLabel %>% get_ansLabelType,
            "s"={ # 程式陳述正確與否的題目
              answerEnvironment$ansValues[[targetAnsLabel]] <- list(
                answerCodeExpressions
              )
            },
            "NA"={
              # 讓環境裡ansObjectName帶有答案物件名
              answerEnvironment$ansObjectName <-
                ansObjectNames[[targetAnsLabel]]

              flag_executable <-
                tryCatch_codeExpressions(answerCodeExpressions, answerEnvironment)

              if(flag_executable){
                if(targetAnsLabel=='ans31') browser()
                {
                  targetAnsObjNames <- ansObjectNames[[targetAnsLabel]]
                  ansValues = {
                    ansValues <-
                      mget(targetAnsObjNames,
                           envir=answerEnvironment,
                           ifnotfound=NA)
                    ansValues <- ansValues[which(!is.na(ansValues))]
                    ansValues
                  }
                  answerEnvironment$ansValues[[targetAnsLabel]] <- ansValues
                }
                # {
                #   whichHasTargetAnsObjName <- which(
                #     map_lgl(
                #       answerEnvironment$ansObjectName,
                #       exists,
                #       envir=answerEnvironment
                #     )
                #   )
                #   if(length(whichHasTargetAnsObjName)==0){
                #     answerEnvironment$ansValues[[targetAnsLabel]] <-
                #       list(NULL)
                #   } else {
                #     targetAnsObjName <- answerEnvironment$ansObjectName[[whichHasTargetAnsObjName]]
                #     answerEnvironment$ansValues[[targetAnsLabel]] <-
                #       list(
                #         answerEnvironment[[targetAnsObjName]]
                #       )
                #   }
                # }
              } else {
                answerEnvironment$ansValues[[targetAnsLabel]] %>%
                  append(list(NA))
              }
            })
        }
      }
    )
  }
}

prepare_dataEnvironment <- function(correctAnsFilename, part){

  dataEnvironment <<- new.env(parent=.GlobalEnv)
    fillup_dataEnvironment(
      dataEnvironment,
      correctAnsFilepath = correctAnsFilename,
      targetPart = part
    )
  # browser()
}

# helpers -----------------------------------------------------------------


get_ansLabelType <- function(x) {
  x %>%
    stringr::str_extract("(?<=ans[:digit:]{1,2})[:alpha:]+") -> ansType
  ansType
}
