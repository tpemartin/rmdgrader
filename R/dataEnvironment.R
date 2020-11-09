setup_library <- function(codeChunksProcessed, dataEnvironment){
  with_env(
    dataEnvironment,
    {
      codeChunksProcessed$chunkExpressions$setup %>%
        map_chr(
          deparse
        ) %>%
        {stringr::str_which(.,"library")} -> whichHasLibrary

      codeChunksProcessed$chunkExpressions$setup[whichHasLibrary] %>%
        walk(
          eval
        )
    }
  )
}

fillup_dataEnvironment <- function(envir, correctAnsFilepath, part){
  correctAnsFilepath %>%
    get_codeChunkProcessed_from_filepath() -> codeChunksProcessed

  setup_library(codeChunksProcessed,
                dataEnvironment = envir)

  codeChunksProcessed$chunkLabelsDecomposed %>%
    filter(part == part) -> targetPartLabels

  if(any(targetPartLabels$type=="data")) {
    # dataEnvironment = new.env(parent=.GlobalEnv)

    dataLabels <- {
      targetPartLabels %>%
        filter(
          type=="data"
        ) %>%
        .$label
    }

    dataLabels %>%
      processDataExprs2getDataEnvironment(
        codeChunksProcessed,
        dataEnvironment = envir
      )

  }
  invisible(envir)
}

fillupDataEnv_with_ansEnvir <- function(codeChunksProcessed){

  # .x=2
  {
    with_env(
      dataEnvironment,
      {
        {
          codeChunksProcessed$chunkLabelsDecomposed %>%
            filter(part ==.x) -> targetPartLabels

          targetPartLabels %>%
            filter(
              is.na(postfix), type=="ans"
            ) %>%
            arrange(digit) %>%
            pull(label) -> ansLabels
        }
        answerEnvironment <- new.env(parent = dataEnvironment)
        answerEnvironment[["ansValues"]] <- list()
        for(.x in seq_along(ansLabels)){
          answerCodeExpressions <-
            codeChunksProcessed$chunkExpressions[[ansLabels[[.x]]]]
          answerEnvironment$ansObjectName <-
            codeChunksProcessed$ansObjectnames[[ansLabels[[.x]]]]

          flag_executable <-
            tryCatch_codeExpressions(answerCodeExpressions, answerEnvironment)

          if(flag_executable){
            answerEnvironment$ansValues[[ansLabels[[.x]]]] <- list(
              answerEnvironment[[answerEnvironment$ansObjectName]]
            )
          } else {
            answerEnvironment$ansValues[[ansLabels[[.x]]]] %>%
              append(list("Error"))
          }
        }


      }
    )
  }
}
prepare_dataEnvironment <- function(correctAnsFilename, part){

  dataEnvironment = new.env(parent=.GlobalEnv)
  dataEnvironment %>%
    fillup_dataEnvironment(
      correctAnsFilepath = correctAnsFilename,
      part=part
    )

}
