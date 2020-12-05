#' evaluate language (any code within \{\}) safely
#'
#' @param language A language. Any codes as long as inside \{\}
#' @param errorReturn default=NA. What would be returned when the language can not be evaluated.
#' @param envir default=caller environment. Where to evaluate the language
#'
#' @return
#' @export
#'
#' @examples eval_safe({a <- 3; b<-a+4})
eval_safe <- function(language, errorReturn=NA, envir=rlang::caller_env()){
  expr2eval <-  rlang::enexpr(language)
  errorReturnExpr <- rlang::enexpr(errorReturn)
  # browser()
  rlang::expr(
    tryCatch(
      !!expr2eval,
      error=function(e){
        !!errorReturnExpr
      }
    )
  ) -> tryCatchExpr

  eval(
    tryCatchExpr, envir=envir
  )

}




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
