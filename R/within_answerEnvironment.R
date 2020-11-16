get_ansObjectValueFromAnswerEnvironment <- function(envir, targetAnsLabel){
  # look for the answer object in answerEnvironment
  targetAnswerObjectNames = dataEnvironment$ansObjectnames[[targetAnsLabel]]
  targetAnswerObjectValues =
    mget(
      targetAnswerObjectNames, envir=envir,
      ifnotfound = NA
    )
  targetAnswerObjectValueSingleton <- list(NULL)
  if(any(!is.na(targetAnswerObjectValues))){
    targetAnswerObjectValueSingleton <- {
      whichIsNotNA <- which(!is.na(targetAnswerObjectValues))
      targetAnswerObjectValues[[whichIsNotNA[[1]]]]
    }
  }
  return(targetAnswerObjectValueSingleton)
}
