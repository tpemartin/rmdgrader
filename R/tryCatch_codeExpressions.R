tryCatch_eval_inAnsEnv <- function(answerCodeExpressions, answerEnvironment){
  flag_executable <- F
  tryCatch(
    R.utils::withTimeout({
    answerCodeExpressions %>% eval_inAnsEnv(answerEnvironment)
    T
  }, timeout=15),
  error=function(e){
    "Error: codes cannot be processed"
    F
  }) -> flag_executable
  invisible(flag_executable)
}
eval_inAnsEnv <- function(stepExpr, envir){
  # browser()
  purrr::walk(
    stepExpr,
    eval,
    envir=envir
  )
  # do.call(eval, stepExpr, envir=envir)
}

