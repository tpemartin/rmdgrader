xfun::read_utf8("/Users/martinl/Github/course-dashboard-programming-for-data-science/homeworks/homework1-ans.Rmd") -> Rmdlines

codeChunks = {
  require(dplyr)
  Rmdlines %>%
    rmd2drake::get_chunksTable() -> chunkTable
  Rmdlines %>%
    rmd2drake::get_listCodeChunksFromRmdlines(requireLabel = T) -> codeChunks
  codeChunks
}

ansObjectnames = {
  whichHasAnsObj <- stringr::str_which(names(codeChunks),"ans[:digit:]+(?![cs])")
  codeChunks[whichHasAnsObj] %>%
    purrr::map(obtain_ansObjectName) -> ansObjectnames
  whichHasAnsObj <- stringr::str_which(names(codeChunks),"ans[:digit:]+(?![cs])")
  codeChunks[whichHasAnsObj] %>%
    purrr::map(obtain_ansObjectName) -> ansObjectnames
}


expressionChunks =
  purrr::map(
    codeChunks,
    parseCodeChunk2Expressions)

{
  names(codeChunks) %>% stringr::str_extract(
    "(?<=ans)[:digit:]+"
  ) %>%
    stringr::str_pad(width=2,side="right" ,pad = "0") -> twoDigits
  twoDigits %>%
    stringr::str_split_fixed("",2) -> colnameMat
  colnames(colnameMat) <- c("part","question")
  seq_part <- unique(colnameMat[,"part"]) %>%
    keep(~{.x!=""})



  }

r(func=function(){
  library(lubridate); library(jsonlite); library(readr); library(rlang);

  {
    dataEnvironment <- new.env()
    expressionChunks$data2 %>%
      walk(eval, envir=dataEnvironment)

    with_env(
      dataEnvironment,
      {
        answerEnvironment <- new.env()
        expressionChunks$ans21 %>%
          purrr::walk(
            eval, envir=answerEnvironment
          )
        ansValues <- as.list(answerEnvironment)
        save(
          ansValues, file="ansValues.Rdata"
        )
      }
    )
  }
})




local_environment(
  dataEnvironment,
  {
    answerEnvironment <- new.env()
    expressionChunks$ans21 %>%
      walk(
        eval, envir=answerEnvironment
      )
    answerEnvironment$ansValues <- list()
    answerEnvironment$ansValues[["ans21"]] <-
      answerEnvironment[["sumChNumbers"]]
    print(dataEnvironment)
    print(answerEnvironment)
    print(answerEnvironment[["sumChNumbers"]])
  }
)

local_environment(
  dataEnvironment,
  {
    answerValues <- list()
    expressionChunks$ans21 %>%
      walk(
        eval, envir=dataEnvironment
      )
    answerValues$ans21 <- dataEnvironment[["sumChNumbers"]]
    assign("answerValues", answerValues, envir=.GlobalEnv)
  }
)

