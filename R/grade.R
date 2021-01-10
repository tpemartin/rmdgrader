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
    try(do.call(as.character(xsym), list_args), silent=T) -> xvalue
    if(is(x,"try-error")) return(NULL)
    return(xvalue)
  }
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
  ae$targetLabel <- targetLabel
  ae$transform <- .transform
  ae$result <- list(
    messages =
      get_allequalSummary(targetLabel = targetLabel, .transform=.transform, switchTargetCurrent=F))
  # browser()
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
    browser()
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
#' @return A list of all.equal outcomes
#' @export
#'
#' @examples none
get_allequalSummary <- function (targetLabel, whichCorrectAnsvalue = 1, .transform=NULL, switchTargetCurrent=F)
{
  assertthat::assert_that(exists("studentValues", envir = .GlobalEnv),
                          exists("correctValues", envir = .GlobalEnv))
  msg_allEqual <- vector("list", length(names(studentValues)))
  y<-correctValues[[targetLabel]][[whichCorrectAnsvalue]]
  if(!is.null(.transform)) y <- .transform(y)
  for (.x in seq_along(studentValues)) {
    x<- studentValues[[.x]][[targetLabel]][[1]]
    if(!is.null(.transform)){
      x <- try(.transform(x), silent = T)
    }
    msg_allEqual[[.x]] <-
      if(switchTargetCurrent){
        all.equal(x, y)
      } else {
        all.equal(y,x)
      }

  }

  names(msg_allEqual) <- names(studentValues)
  msg_allEqual
}

#' Decompose an object for systematic grading
#'
#' @param y an object
#'
#' @return a list of class(object), length(object), el_withNames, el_noName
#' @export
#'
#' @examples none
decomposeObject <- function(y){
  nY <- length(y) # determine grades for each element
  namesY <- names(y)
  whichHasNoName <-
    if(is.null(namesY)){
      1:nY
    } else {
      which(namesY=="")
    }
  whichHasName <-
    if(is.null(namesY)){
      NULL
    } else {
      which(namesY!="")
    }

  ywithNames <- y[whichHasName]
  ynoName <- y[whichHasNoName]

  list(
    class=class(y),
    length=nY,
    el_withNames = ywithNames,
    el_noName = ynoName
  )
}

#' Grade answer with given grading method
#'
#' @description there must be studentValues and correctValues two list of answer object values proccessed from student Rmds and teacher Rmd
#' @param targetLabel A character, such as "ans11".
#' @param gradingMethod A function with three arguments: student answer value, correct answer value, errorMsg
#' @param whichCorrectAnsvalue An number, default=1, represents which correct answer to use
#'
#' @return A list of grades
#' @export
#'
#' @examples none

grade <- function(targetLabel, gradingMethod, whichCorrectAnsvalue=1){
  assertthat::assert_that(
    exists("studentValues", envir=.GlobalEnv),
    exists("correctValues", envir = .GlobalEnv)
  )

  correctValues[[targetLabel]][[whichCorrectAnsvalue]] ->> y
  studentNames <- names(studentValues)
  seq_along(studentValues) %>%
    purrr::map(
      ~gradingMethod(studentValues[[.x]][[targetLabel]][[1]],y)
    ) -> grades
  names(grades) <- names(studentValues)
  grades
}

#' Execute transformation function safely in which when error happens it will return original value
#'
#' @param x An object to be transformed
#' @param fun A symbol of transform function
#' @param ... passed to the transform function
#'
#' @return either fun(x); or, when error in fun(x), return(x)
#' @export
#'
#' @examples none
exec_safe <- function(x, fun, ...)
{
  fun <- rlang::ensym(fun)
  rlang::expr({
    tryCatch({
      x <- (!!fun)(x, ...)
      x
    },
    error=function(e){
      x
    })
  }) -> toEval
  eval(toEval)
}


#' For-loop grade answer with given grading method
#'
#' @description there must be studentValues and correctValues two list of answer object values proccessed from student Rmds and teacher Rmd
#' @param targetLabel A character, such as "ans11".
#' @param gradingMethod A function with three arguments: student answer value, correct answer value, errorMsg
#' @param whichCorrectAnsvalue An number, default=1, represents which correct answer to use
#'
#' @return A list of grades
#' @export
#'
#' @examples none
grade_for <- function(targetLabel, gradingMethod, whichCorrectAnsvalue=1){
  assertthat::assert_that(
    exists("studentValues", envir=.GlobalEnv),
    exists("correctValues", envir = .GlobalEnv)
  )

  correctValues[[targetLabel]][[whichCorrectAnsvalue]] ->> y
  studentNames <- names(studentValues)
  grades <- vector("list", length(studentValues))
  for(.x in seq_along(studentValues)){
    cat('.x =',.x, ' filename =', names(studentValues)[[.x]], '\n')
    gradingMethod(studentValues[[.x]][[targetLabel]][[1]],y) ->
      grades[[.x]]
  }
  names(grades) <- names(studentValues)
  grades
}

#' Generate function to quickly pull out specific answers from a student and the correcnt answers
#'
#' @description Given student index in studentValues, generate a getxy function of answer label which quickly gives x(this student's answer) and y(correct answer) in the global environment
#' @param .x A character or number. When character, it should be from names(studentValues); When number, it should be the location regarding element in studentValues.
#'
#' @return A function
#' @export
#'
#' @examples none.
getxyFunctional <- function(.x){
  .x = ifelse(is.character(.x), basename(.x), .x)
  .x = stringr::str_remove_all(.x, "\\s")
  stringr::str_which(
    names(studentValues), .x) -> whichIsTheTarget
  function(targetLabel, whichCorrectAnsvalue=1, .transform=NULL){
    assertthat::assert_that(
      exists("studentValues", envir=.GlobalEnv),
      exists("correctValues", envir = .GlobalEnv)
    )
    studentValues[[whichIsTheTarget]][[targetLabel]][[1]] -> x
    correctValues[[targetLabel]][[whichCorrectAnsvalue]] -> y
    if(!is.null(.transform)){
      .transform(x) -> x
      .transform(y) -> y
    }
    x ->> x
    y ->> y
  }
}
#' The extension to ifelse to accommodate error situation
#'
#' @param cond A logical
#' @param v1 value to return when T
#' @param v2 value to return when F
#' @param errorMsg message to print and return when error
#'
#' @return
#' @export
#'
#' @examples none
ifelsethen <- function(cond, v1, v2, errorMsg) {
  tryCatch(
    {
      ifelse(
        cond,
        v1,
        v2
      )
    },
    error = function(e) {
      errorMsg
    }
  ) -> result
  return(result)
}
get_errorStatus <- function(list_expectations){
  map_dbl(list_expectations, check_expectation)
}
.l <- check_expectation <- function(.expr){
  tryCatch({
    eval(.expr)
    return(F)
  },
  error=function(e){
    return(T)
  })
}

#' Generate a list of getxy from a group of Rmds selected
#'
#' @param eliteGroup A character vector each represent a chosen Rmd to form getxy as how we use in getxy input argument. This is a vectorized version.
#'
#' @return
#' @export
#'
#' @examples none
generate_mgetxy = function(eliteGroup){
  map(
    eliteGroup,
    getxyFunctional
  ) -> list_getxy
  names(list_getxy) <- eliteGroup
  list_getxy
}

#' Grade function specific to elite group
#'
#' @param .mgetxy A list of getxy functions
#'
#' @return
#' @export
#'
#' @examples none
grademFunctional <- function(.mgetxy){

  function(ansLabel, gradingMethod){
    map_dbl(
      .mgetxy,
      ~{
        .x(ansLabel)
        gradingMethod(x,y)
      }
    )
  }
}


# helpers -----------------------------------------------------------------


generate_.x_functions <- function(ae, .x) {
  function(){
    rlang::expr({
      # Group 1's all.equal messages
      message("all.equal Messages: \n")
      print(head(ae$result$messageGroups[[!!.x]]$messages, 10))
      # Group 1's x y
      # .temp <- ae$xy[[!!.x]]
      # View(.temp)

      Rmd1 <- sample(ae$result$messageGroups[[!!.x]]$Rmds, 1)
      message("抽出的檔案\n")
      print(Rmd1) # 抽出的檔案
      message("原始程式碼\n")
      mgetxy[[Rmd1]](ae$targetLabel)
      print(x) # 原始程式碼
      message("transform後，沒transform則與上面相同\n")
      print(head(ae$xy[[!!.x]][[Rmd1]]$x)) # transform後，沒transform則與上面相同
      ae$check_messageGroups[[paste0("G",!!.x,"grade")]] <-
        list(
          comment=function(comment){
            comment -> ae$result$messageGroups[[!!.x]]$comment
          },
          grade=function(grade){
            grade -> ae$result$messageGroups[[!!.x]]$grade
          }
        )

    }) -> expr_.x
    eval(expr_.x)
  }

}

