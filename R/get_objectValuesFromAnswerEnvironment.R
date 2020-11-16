get_objectValuesFromAnswerEnvironment <- function(answerEnvironment){
  possibleAnsObjNames <- answerEnvironment$ansObjectName
  objValues <- mget(possibleAnsObjNames, envir=answerEnvironment, ifnotfound = NA)
  if(all(is.na(objValues))){
    list(NULL)
  } else {
    which(!is.na(objValues)) -> whichIsNotNA
    answerEnvironment[[
      possibleAnsObjNames[[whichIsNotNA[[1]]]]
    ]]
  }
}
