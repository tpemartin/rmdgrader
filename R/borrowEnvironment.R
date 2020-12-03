
createBorrowEnvironment <- function(ansEnvironment, targetAnsLabel){
  borrowEnvName <- as.name(paste0("borrowEnv_", targetAnsLabel))
  createBorrowEnvExpr <- rlang::expr((!!borrowEnvName) <<- rlang::env_clone(answerEnvironment, parent=dataEnvironment))
  eval(createBorrowEnvExpr)
}

