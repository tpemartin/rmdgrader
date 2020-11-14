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
  function(targetLabel, whichCorrectAnsvalue=1){
    assertthat::assert_that(
      exists("studentValues", envir=.GlobalEnv),
      exists("correctValues", envir = .GlobalEnv)
    )
    studentValues[[.x]][[targetLabel]][[1]] ->> x
    correctValues[[targetLabel]][[whichCorrectAnsvalue]] ->> y
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
