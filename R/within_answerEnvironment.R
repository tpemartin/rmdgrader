get_ansObjectValueFromAnswerEnvironment <- function(envir, targetAnsLabel, isStudentRmd=T){
  # look for the answer object in answerEnvironment
  targetAnswerObjectNames = dataEnvironment$ansObjectnames[[targetAnsLabel]]
  map(
    targetAnswerObjectNames,
    mget_safe,
    envir=envir
  ) -> targetAnswerObjectValues
  targetAnswerObjectValueSingleton <- list(NULL)
  if(isStudentRmd && any(!is.na(targetAnswerObjectValues) &
                         stringr::str_detect(targetAnswerObjectValues,"Parsing Error",negate=T))){
    targetAnswerObjectValueSingleton <- {
      whichIsNotNAParsingError <-
        which(!is.na(targetAnswerObjectValues) & str_detect(targetAnswerObjectValues, "Parsing Error", negate=T))
      targetAnswerObjectValues[[whichIsNotNAParsingError[[1]]]]

    }
  } else {
    if(length(targetAnswerObjectValues)==1){
      targetAnswerObjectValueSingleton <- targetAnswerObjectValues[[1]]
    } else {
      targetAnswerObjectValueSingleton <- targetAnswerObjectValues
    }

  }
  return(targetAnswerObjectValueSingleton)
}
#' a safe version of mget, this allow input argument to be either a character of object name or a character of obtaining value, such as "a[['number']][[1]]"
#'
#' @param text A character either represents an object name or how to obtain a value
#' @param envir An environment where to look for value
#'
#' @return
#' @export
#'
#' @examples none
mget_safe <- function(text, envir){
  tryCatch({
    eval(parse(text=text), envir = envir)
  },
  error=function(e){
    paste0("Parsing Error of ", text)
  })-> objValue
  objValue
}

