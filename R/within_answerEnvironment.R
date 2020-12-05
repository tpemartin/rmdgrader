get_ansObjectValueFromAnswerEnvironment <- function(envir, targetAnsLabel){
  # look for the answer object in answerEnvironment
  targetAnswerObjectNames = dataEnvironment$ansObjectnames[[targetAnsLabel]]
  map(
    targetAnswerObjectNames,
    mget_safe,
    envir=envir
  ) -> targetAnswerObjectValues
  targetAnswerObjectValueSingleton <- list(NULL)
  if(any(!is.na(targetAnswerObjectValues))){
    targetAnswerObjectValueSingleton <- {
      whichIsNotNA <- which(!is.na(targetAnswerObjectValues))
      targetAnswerObjectValues[[whichIsNotNA[[1]]]]
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
mget_safe <- function(text, envir=rlang::caller_env()){
  tryCatch({
    eval(parse(text=text), envir = envir)
  },
  error=function(e){
    NA
  })-> objValue
  objValue
}

