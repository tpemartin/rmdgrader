#' Grade student Rmds based on all.equal messages
#'
#' @param ae An all.equal_env object after assigning grades for all messages groups
#'
#' @return A list of grades and comments
#' @export
#'
#' @examples none
grade_by_all.equalMessages <- function(ae)
{
  assertthat::assert_that(exists("studentValues", envir = .GlobalEnv),
                          exists("correctValues", envir = .GlobalEnv))
  ae$result$table_messageGroups -> tb
  tb$group <- 1:nrow(tb)
  tb[,c("group", "grade", "comment", "Rmds")] %>%
    as_tibble() -> tb
  tb$grade %>% unlist() -> tb$grade
  tb$comment <-
    map_chr(tb$comment,
            ~{ifelse(length(.x)==0, "", .x)})

  tidyr::unnest(tb, cols = c(Rmds)) -> tb_unnested

  grades <- list(
    grade=tb_unnested$grade,
    comment=tb_unnested$comment
  )
  names(grades$grade) <- tb_unnested$Rmds
  names(grades$comment) <- tb_unnested$Rmds
  return(grades)
}

#' Service initiator for all.equal comparisons
#'
#' @return
#' @export
#'
#' @examples none
allequalService <- function(targetLabel = targetLabel, .transform=NULL, switchTargetCurrent=F){
  ae <- new.env(parent=.GlobalEnv)
  class(ae) <- c(class(ae), "all.equal_env")

  populate_aeWith_basicInfo(ae, targetLabel, .transform, switchTargetCurrent)

  # ae$targetLabel <- targetLabel
  # ae$transform <- .transform
  # ae$result <- list(
  #   messages =
  #     get_allequalSummary(targetLabel = targetLabel, .transform=.transform, switchTargetCurrent=F))
  # # browser()
  ae$.yield_messageGroup <- function(){
    ae$result$messages_category <- unique(ae$result$messages)
    ae$result$messageGroups <- vector("list", length(ae$result$messages_category))
    purrr::walk(
      seq_along(ae$result$messages_category),
      ~{
        ae$result$messageGroups[[.x]]$messages <- ae$result$messages_category[[.x]]
        ae$result$messageGroups[[.x]]$Rmds <- character(0)
        ae$result$messageGroups[[.x]]$grade <- as.numeric(NA)
        ae$result$messageGroups[[.x]]$comment <- character(0)
      }
    )

    Rmdnames <- names(ae$result$messages)
    for(.it in seq_along(Rmdnames))
    {
      # if(Rmdnames[[.it]] == "HW6_410973083.Rmd") browser()
      whichGroupBelong <- which(map_lgl(
        ae$result$messages_category,
        ~identical(ae$result$messages[[.it]], .x)
      ))
      ae$result$messageGroups[[whichGroupBelong]]$Rmds <-
        c(ae$result$messageGroups[[whichGroupBelong]]$Rmds, Rmdnames[[.it]])
    }

    ae$.yield_messageGroupTable <- function(){
      list2DF(purrr::transpose(ae$result$messageGroups)) ->
        ae$result$table_messageGroups

        ae$result$table_messageGroups$Rmd_count <-
        purrr::map_int(
          ae$result$table_messageGroups$Rmds,
          length
        )
    }


  }

  ae$.update <- function(){
    ae$result$types_of_messages <-
      get_vectorOfAllPossibleMsgs(ae$result$messages)
    ae$result$messages_category <- unique(ae$result$messages)
    ae$.yield_messageGroup()
    ae$filterMessage=
      filterMessage_Functional(ae$result$messages)
  }

  ae$.update()

  ae$.categorize_targetIs=categorize_targetIs

  ae$.turnMsg2Null_byPattern=function(pattern){
    ae$nulifyResult <- list()
    turnMsg2Null_byPattern(ae$result$messages, pattern) -> ae$nulifyResult$messages
    ae$nulifyResult$update_result_messages=function(){
      ae$nulify_messages -> ae$result$messages
      ae$.update()
    }
  }

  ae$generate_xy4messageGroups <- function(mgetxy){
    ae$xy <- vector("list", length(ae$result$messageGroups))
    for (.it in seq_along(ae$result$messageGroups)) {
      whichIsGroupIt <- which(names(mgetxy) %in% ae$result$messageGroups[[.it]]$Rmds)
      ae$xy[[.it]] <- execute_mgetxy(mgetxy[whichIsGroupIt], targetLabel, .transform=.transform)
    }
    # browser()
    ae$check_messageGroups <- {
      list_.x <- vector("list", length(ae$result$messageGroups))
      for(.x in seq_along(ae$result$messageGroups)){
        rlang::expr(generate_.x_functions(ae, !!.x)) -> expr_generate.X
        eval(expr_generate.X) -> list_.x[[.x]]
      }
      list_.x
    }
    names(ae$check_messageGroups) <- paste0("G", seq_along(ae$result$messageGroups))
    # ae$xy <- execute_mgetxy(mgetxy, targetLabel, .transform=.transform)
  }

  ae
}


# helpers -----------------------------------------------------------------

#' Execute mgetxy for some ansXXX with transform function possibility
#'
#' @param mgetxy An mgetxy object
#' @param targetLabel A ansXXX label
#' @param .transform  default=NULL, a transform function
#'
#' @return a list of xy
#' @export
#'
#' @examples none
execute_mgetxy <- function(mgetxy, targetLabel, .transform=NULL)
{
  list_output <- vector("list", length(mgetxy))
  for(.it in seq_along(mgetxy))
  {
    mgetxy[[.it]](targetLabel)
    x <- .GlobalEnv$x
    y <- .GlobalEnv$y
    x <- if(is.null(.transform)) x else .transform(x)
    # browser()
    y <- if(is.null(.transform)) y else .transform(y)
    list_output[[.it]] <- list(x=x,y=y)
  }
  names(list_output) <- names(mgetxy)
  list_output
}
#' When all.equal msg shows pattern, turn msg to NULL (i.e. error)
#'
#' @param summary31 list of all.equal messages
#' @param pattern Regex pattern
#'
#' @return
#' @export
#'
#' @examples none
turnMsg2Null_byPattern <- function(summary31, pattern){
  purrr::map(
    summary31,
    function(x) {
      if(any(stringr::str_detect(x,pattern))) return(NULL)
      x
    }
  ) -> summary31b
  summary31b
}
#' Turn list of all.equal msg to unique msg vectors
#'
#' @param summary31b list of all.equal msgs
#'
#' @return
#' @export
#'
#' @examples none
get_vectorOfAllPossibleMsgs <- function(summary31b){
  list_allMsg <- purrr::keep(summary31b, function(x) !is.logical(x) && !is.null(x))
  allMsg <- sort(unique(unlist(list_allMsg)))
  allMsg
}
#' Divide list of all.equal msg into list of group with different names of "Target is..."
#'
#' @param summary31b A list of all.equal msgs
#'
#' @return
#' @export
#'
#' @examples none
categorize_targetIs <- function(summary31b){
  require(dplyr)
  refineListAllEqual_byMsgKeyphraseFunctional(summary31b) -> refine_byPattern
  refine_byPattern("^target is") %>%
    purrr::map(stringr::str_subset, "target is") %>%
    unlist -> all_targetIs
  levels_targetIs <- unique(all_targetIs)
  purrr::map(
    levels_targetIs,
    ~{
      names(subset(all_targetIs, all_targetIs == .x))
    }
  ) -> list_targetIs
  names(list_targetIs) <- levels_targetIs
  list_targetIs
}


#' Subset list of all.equal messages down to those fit msgKeyphrase pattern
#'
#' @param list_allequalMsgs A list of all.equal msg to be refined
#'
#' @return A refined list
#' @export
#'
#' @examples none
filterMessage_Functional <- function(list_allequalMsgs){
  function(msgKeyphrase){
    list_allMsg <- purrr::keep(list_allequalMsgs, function(x) !is.logical(x))
    allMsg <- unique(unlist(list_allMsg))
    map_lgl(
      seq_along(list_allMsg),
      ~any(stringr::str_detect(list_allMsg[[.x]], msgKeyphrase))
    ) -> pick_refined
    list_allMsg[pick_refined]
  }
}

#' Generate list of all.equal results where Target is correct answer and Current is student answer
#'
#' @param targetLabel A character of like ans11, etc
#' @param whichCorrectAnsvalue A number default at 1, indicating which values in correct answer values is used for comparison
#' @param .transform A function, default at NULL. If not null, it is the function that transforms x and y before grading and comparison
#'
#' @return A list of all.equal outcomes. If transformed error, it will be try-error class object.
#' @export
#'
#' @examples none
get_allequalSummary <- function (targetLabel, whichCorrectAnsvalue = 1, .transform=NULL, switchTargetCurrent=F)
{
  assertthat::assert_that(exists("studentValues", envir = .GlobalEnv),
                          exists("correctValues", envir = .GlobalEnv))
  msg_allEqual <- vector("list", length(names(.GlobalEnv$studentValues)))
  y<-.GlobalEnv$correctValues[[targetLabel]][[whichCorrectAnsvalue]]
  if(!is.null(.transform)) y <- .transform(y)
  for (.x in seq_along(.GlobalEnv$studentValues)) {
    x<- .GlobalEnv$studentValues[[.x]][[targetLabel]][[1]]
    if(!is.null(.transform)){
      x <- try(.transform(x), silent = T)
    }
    msg_allEqual[[.x]] <-
      if(switchTargetCurrent){
        all.equal(x, y)
      } else {
        all.equal(y,x) # default
      }

  }

  names(msg_allEqual) <- names(studentValues)
  msg_allEqual
}
#' Generate transform function for function grading
#'
#' @param ... A name-value pairs for function-to-grade's inputs
#'
#' @return A transform function of one functional class input
#' @export
#'
#' @examples none
transformFunctional <- function(...){
  # browser()
  list_args <- list(...)
  function(x){
    xsym <- rlang::ensym(x)
    if(!is.function(x)) return(NULL)
    rlang::expr((!!xsym)(!!!list_args)) -> call_expr
    try(eval(call_expr), silent=T) -> xvalue
    if(is(x,"try-error")) return(NULL)
    return(xvalue)
  }
}


populate_aeWith_basicInfo <- function(ae, targetLabel, .transform, switchTargetCurrent)
{
  ae$targetLabel <- targetLabel
  ae$transform <- .transform
  ae$result <- list(
    messages = get_allequalSummary(
      targetLabel=targetLabel,
      .transform= .transform,
      switchTargetCurrent = switchTargetCurrent
    )
  )

}
