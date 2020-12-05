get_objectValuesFromAnswerEnvironment <- function(answerEnvironment){
  possibleAnsObjNames <- answerEnvironment$ansObjectName

  objValues <- vector("list", legnth(possibleAnsObjNames)) # to accommodate multiple ansObjectNames
  for(.x in possibleAnsObjNames){
    mget_safe(.x, envir=answerEnvironment) -> objValues[[.x]]
  }

  # objValues <- mget(possibleAnsObjNames, envir=answerEnvironment, ifnotfound = NA)

  if(all(is.na(objValues))){
    list(NULL)
  } else {
    which(!is.na(objValues)) -> whichIsNotNA
    # answerEnvironment[[
    #   possibleAnsObjNames[[whichIsNotNA[[1]]]]
    # ]]
    objValues[[whichIsNotNA[[1]]]]
  }
}

# helpers -----------------------------------------------------------------


