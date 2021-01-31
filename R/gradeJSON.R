#' Review instance initiator
#'
#' @param tb_grades A dataframe of grades from each ansXXX. If need to be recale, recale them before input here.
#' @param title A character of title, normally from params$title
#'
#' @return A review instance
#' @export
#'
#' @examples None
Review <- function(tb_grades, title){
  ge <- new.env()
  ge$source_JSON <- {
    records_gradeComment <-
      generate_recordGradeCommentsFromTb_gradesWithoutComments(tb_grades)

    names_studentRmds <- names(records_gradeComment)

    purrr::map(
      names_studentRmds,
      ~ {
        Xstudent <- .x # names_studentRmds[[.x]]
        Xrecord_gradeComment <- records_gradeComment[[Xstudent]]

        setNames(
          list(
            list(
              grade = as.numeric(NA),
              PR = as.numeric(NA),
              raw = Xrecord_gradeComment,
              tb_grade = subset(tb_grades, tb_grades$name == Xstudent)
            )
          ), title
        )
      }
    ) -> source_gradeJSON
    setNames(source_gradeJSON, names_studentRmds)
  }

  ge$compute_GradePR <- compute_GradePRFuns(ge, names_studentRmds)

  ge$compute_GradePR()

  purrr::walk(
    seq_along(names_studentRmds),
    ~{
      Xstudent <- names_studentRmds[[.x]]
      XallAnsLabels <- stringr::str_subset(
        names(ge$source_JSON[[Xstudent]][[title]]$raw), "ans"
      )
      for(.YansLabel in XallAnsLabels){
        ge$source_JSON[[Xstudent]][[title]]$raw[[.YansLabel]]$gradeUpdate =
          generate_gradeUpdateFuns(ge, Xstudent, title, .YansLabel)
        ge$source_JSON[[Xstudent]][[title]]$raw[[.YansLabel]]$commentUpdate =
          generate_commentUpdateFuns(ge, Xstudent, title, .YansLabel)
      }
    }
  )



  return(ge)
}

# helpers -----------------------------------------------------------------
generate_gradeUpdateFuns <- function(ge, Xstudent, title, .YansLabel){
  YansLabel <- .YansLabel
  function(newGrade){
    ge$source_JSON[[Xstudent]][[title]]$raw[[YansLabel]][[1]]$grade <- newGrade
    ge$source_JSON[[Xstudent]][[title]]$tb_grade[[YansLabel]] <- newGrade
  }
}
generate_commentUpdateFuns <- function(ge, Xstudent, title, YansLabel){
  function(newComment){
    ge$source_JSON[[Xstudent]][[title]]$raw[[YansLabel]][[1]]$comment <- newComment
  }
}
compute_GradePRFuns <- function(ge, names_studentRmds){
  function(){
    for(Xstudent in names_studentRmds){
      ge$source_JSON[[Xstudent]][[title]]$tb_grade %>%
        select(contains("ans")) %>%
        mutate(
          total=sum(c_across())
        ) -> ge$source_JSON[[Xstudent]][[title]]$tb_grade

      ge$source_JSON[[Xstudent]][[title]]$tb_grade$total ->
        ge$source_JSON[[Xstudent]][[title]]$grade

      purrr::map_dbl(
        ge$source_JSON,
        ~{
          Xgrade <- .x[[title]]$grade
          ifelse(is.na(Xgrade), 0, Xgrade)}
      ) -> allGrades
      PR <- round(pmin((100-percent_rank(allGrades)*100)+1, 100),0)
      names(PR) <- names(ge$source_JSON)
      purrr::walk(
        seq_along(ge$source_JSON),
        ~{
          ge$source_JSON[[.x]][[title]]$PR <- PR[[.x]]
        }
      )
    }
  }
}

