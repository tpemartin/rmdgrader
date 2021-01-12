#' Transform functional that modifies x function to keep all screen messages and turn x function into x(...)
#'
#' @param ... a list-pair input arguments of original x function
#'
#' @return A list of return (returned value from x(...) call) and messages on screen
#' @export
#'
#' @examples none.
transformFunctional_messageKeep <- function (...)
{
  list_args <- list(...)
  function(x) {
    xsym <- rlang::ensym(x)
    .GlobalEnv$messageEnv  <- new.env()
    .GlobalEnv$messageEnv$messages=list()
    if (!is.function(x))
      return(NULL)
    xnew <- modify_function2keepScreenMessages(x, messageEnv)
    call_expr <- rlang::expr(xnew(!!!list_args))
    xvalue <- try(eval(call_expr), silent = T)
    if (is(x, "try-error"))
      return(NULL)
    result <-
      list(
        return=xvalue,
        messages=.GlobalEnv$messageEnv$messages)
    return(result)
  }
}

# helpers -----------------------------------------------------------------


messageKeep <- function(x){
  message(x)
  append(.GlobalEnv$messageEnv$messages, x) -> .GlobalEnv$messageEnv$messages
}

get_bodyText <- function(x){
  body(x) -> bodyx
  deparse(bodyx) -> bodyTxt
  bodyTxt
}
get_functionHeadText <- function(x){
  xFormals <- formals(x)
  arguments <- paste0(names(xFormals), collapse = ",")
  funTxt <- glue::glue("function(<<arguments>>)", .open = "<<", .close=">>")
}
modify_function2keepScreenMessages <- function(x, messageEnv){
  bodyTxt <- get_bodyText(x)
  newBodyTxt <- {
    require(magrittr)
    bodyTxt %>%
      # stringr::str_replace_all( # return(currentUser) -> returnKeep(currentUser)
      #   "return\\(","returnKeep\\(") %>%
      stringr::str_replace_all(
        "(message|cat|warning|print)\\(","messageKeep\\("
      )
  }
  funHead <- get_functionHeadText(x)
  eval(
    parse(text=
            c(funHead,
              newBodyTxt))
  )
}
