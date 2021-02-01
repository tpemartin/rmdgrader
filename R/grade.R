#' A mixture of m grading and all.equal grading
#'
#' @param ae A all.equal_env object, which has .yield_messageGroupTable method already applied.
#' @param gradingMethod A function of m grading
#' @param whichCorrectAnsvalue An integer indicating which object value to be used as answer
#'
#' @return a list of grades with Rmd names as element names
#' @export
#'
#' @examples none
gradeMix <- function(ae, gradingMethod, whichCorrectAnsvalue=1){
  assertthat::assert_that(
    exists("studentValues", envir=.GlobalEnv),
    exists("correctValues", envir = .GlobalEnv)
  )
  ae$result$table_messageGroups %>%
    filter(
      map_lgl(grade, function(x) !is.na(x))
    ) %>% select(grade, Rmds) %>%
    tidyr::unnest(cols=Rmds) -> gradesAssignedFromAE

  unlist(gradesAssignedFromAE$grade) -> gradesAE
  unlist(gradesAssignedFromAE$Rmds) -> RmdsAE

  targetLabel <- ae$targetLabel
  correctValues[[targetLabel]][[whichCorrectAnsvalue]] ->> y
  whichAreUngraded <- which(!(names(studentValues) %in% RmdsAE))
  studentValuesUngraded <- studentValues[whichAreUngraded]
  studentNames <- names(studentValuesUngraded)
  seq_along(studentValuesUngraded) %>%
    purrr::map(
      ~gradingMethod(studentValuesUngraded[[.x]][[targetLabel]][[1]],y)
    ) -> gradesNonAE
  append(
    gradesAE, gradesNonAE
  ) -> grades
  names(grades) <- c(RmdsAE, studentNames)
  grades
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
  if(length(result)==0) result <- errorMsg
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


generate_.x_functions <- function(ae, .x, useSHA) {
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
      # browser()
      if(useSHA){
        XgroupName <-
          stringr::str_sub(names(ae$check_messageGroups)[[!!.x]], 1, 7)
        XgroupcommentName <-
          paste0(XgroupName,"_GradeComment")
      } else {
        XgroupcommentName <- paste0("G",!!.x,"grade")
      }

      # ae$check_messageGroups[[]] <-
      ae$check_messageGroups[[XgroupcommentName]] <-
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

#' Generate records_gradeComment for step return from tb_grades
#'
#' @param tb_grades A data frame.
#'
#' @return
#' @export
#'
#' @examples none
generate_recordGradeCommentsFromTb_gradesWithoutComments <- function(tb_grades) {
  record_gradesComments <- vector("list", nrow(tb_grades))
  namesTb_grades <- names(tb_grades)
  ansLabels <- stringr::str_subset(
    namesTb_grades, "ans"
  )
  for (.y in 1:nrow(tb_grades)) {
    record_gradesComments[[.y]] <- {
      Ygrades <- tb_grades[.y, ansLabels]
      setNames(purrr::map(
        seq_along(ansLabels),
        ~ {list(
          list(
            time = timestamp(quiet = T),
            grade = Ygrades[[ansLabels[[.x]]]],
            comment = ""
          )
        )

        }
      ), ansLabels)
    }
  }
  names(record_gradesComments) <- tb_grades$name
  record_gradesComments
}
