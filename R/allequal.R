#' Grade student Rmds based on all.equal messages
#'
#' Service initiator for all.equal comparisons
#'
#' @return
#' @export
#'
#' @examples none
allequalService <- function(targetLabel = targetLabel, .transform=NULL, switchTargetCurrent=F, useSHA=F){
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
        rlang::expr(generate_.x_functions(ae, !!.x, useSHA)) -> expr_generate.X
        eval(expr_generate.X) -> list_.x[[.x]]
      }
      list_.x
    }
    # browser()

    if(useSHA){
      names(ae$check_messageGroups) <-
        purrr::map_chr(
          seq_along(ae$check_messageGroups),
          ~{
            paste0(unlist(ae$result$messageGroups[[.x]]$messages), collapse = "") -> sourceX
            paste0("G", digest::sha1(sourceX))
          }
        )
    } else {
      names(ae$check_messageGroups) <- paste0("G", seq_along(ae$result$messageGroups))
    }

    names(ae$xy) <- names(ae$check_messageGroups)
    # ae$xy <- execute_mgetxy(mgetxy, targetLabel, .transform=.transform)
  }

  ae$extract_grades <- get_gradesFromAeFunctional(ae)

  # attach_file.edits(ae, path)
  ae
}
#' Extract grades and comments from an all.equal_env grading result.
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
    if(length(.GlobalEnv$studentValues[[.x]][[targetLabel]])==1L){
      x<- .GlobalEnv$studentValues[[.x]][[targetLabel]][[1]]
    } else
    {
      x<- .GlobalEnv$studentValues[[.x]][[targetLabel]]
    }

    if(!is.null(.transform)){
      x <- try(.transform(x), silent = T)
    }
    msg_allEqual[[.x]] <-
      if(switchTargetCurrent){
        tryCatch_allEqual(x, y)
        # all.equal(x, y)
      } else {
        tryCatch_allEqual(y, x)
        # all.equal(y,x) # default
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
#' When facing list of ae's, use this to compute grades as sum/mean across all ae's extracted grades
#'
#' @param list_ae A list of ae's.
#'
#' @return A data frame with sum and mean of grades across all versions of ae's
#' @export
#'
#' @examples none
compute_gradesFromMultipleAes <- function(list_ae){
  assertthat::assert_that(
    is.list(list_ae) &&
      all(purrr::map_lgl(list_ae, is, "all.equal_env")),
    msg="Input must be a list of multiple all.equal_env objects")

  map(
    list_ae,
    ~.x$extract_grades()
  ) -> list_grades

  map(
    list_grades,
    names
  ) -> list_names
  reduce(list_names, union) -> allRmdNames

  map_dfc(
    list_grades,
    ~.x[allRmdNames]
  ) -> df_grades

  df_grades %>%
    mutate(
      sum=sum(c_across()),
      mean=mean(c_across()),
      Rmd=allRmdNames
    ) -> df_grades2
  return(df_grades2)

}
get_gradesFromAeFunctional <- function(ae){
  function(naIsZero=T){
    ae$.yield_messageGroupTable()
    list_grade <- grade_by_all.equalMessages(ae)
    grade <- list_grade$grade
    if(naIsZero){
      grade[
        is.na(grade)
      ] <- 0
    }
    grade
  }

}
useSubcat2ReviseGrade <- function(ae) {
  function(grades){
    # grades <- ae$extract_grades()
    cat_errors <- ae$subcat$cat_errors
    purrr::map(
      cat_errors,
      ~ {
        grade <- rep(.x$grade, length(.x$el_names))
        names(grade) <- .x$el_names
        grade
      }
    ) -> revisedGrade
    unlist(revisedGrade) -> revisedGrade
    rmdnames <- names(revisedGrade)
    grades[rmdnames] <- revisedGrade
    grades
  }
}
#' Categorise rmd based on manually defined list of msg and rmd elements
#'
#' @param ae An ae environment to attach subcat element
#' @param list_msg_rmd_pairs A list of msg and rmd elements
#'
#' @return none. but attach subcat in ae
#' @export
#'
#' @examples none
subcategorise_byMsgRmdPairs <- function(ae, list_msg_rmd_pairs){
  assertthat::assert_that(
    all(c("msg", "rmd") %in% names(list_msg_rmd_pairs)),
    msg="list_msg_rmd_pairs must be a list with two elements named msg and rmd"
  )
  resultsTxt <- setNames(
    list_msg_rmd_pairs$msg,
    list_msg_rmd_pairs$rmd)
  ae$subcat <- list(
    cat_errors= categorise_elementNames_ByElementValues(resultsTxt),
    accommodate_grades = useSubcat2ReviseGrade(ae)
  )
}

tryCatch_allEqual <- function(y, x){
  tryCatch(
    all.equal(y,x),
    error=function(e){
      e
    }) -> result
  return(as.character(result))
}
attach_file.edits <- function(ae, path){
  studentValues |> names() -> studentRmds
  #
  # .x=1
  ae$file.edits <- purrr::map(
    seq_along(studentRmds),
    ~function(){
      file.edit(
        file.path(path,
          studentRmds[[.x]]))
    }
  ) |> setNames(studentRmds)
}
