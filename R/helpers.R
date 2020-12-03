#' With executable flag, record the evaluated ans object values in answerEnvironment
#'
#' @param flag_executable A logical
#' @param targetAnsLabel A character
#' @param saveEnv An environment to save ans object value
#' @param evalEnv An environment where to evaluate expressions
#'
#' @return none, but saveEnv is updated
#' @export
#'
#' @examples none
recordAnsObjValues_withFlagExecutable <- function(flag_executable,
                                    targetAnsLabel,
                                    saveEnv,
                                    evalEnv=NULL){
  if(is.null(evalEnv)) evalEnv <- saveEnv

  if(flag_executable){

    saveEnv$ansValues[[targetAnsLabel]] <- list(get_ansObjectValueFromAnswerEnvironment(evalEnv, targetAnsLabel))

  } else {
    saveEnv$ansValues[[targetAnsLabel]] %>%
      append(list("Error"))
  }
}
