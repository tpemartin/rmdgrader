#' Genrate Synthesize instance to put student grades in their Rmds
#'
#' @param df_grades a data frame from the RDS file read generated by AllequalService2 instance's export_grade method.
#' @param mapping a list of ans label and Rmd section title (name-value) pair. For example, list(ans11="## 1.1", ans12="## 1.2", ans13="## 1.3", ans14="## 1.4", ans15="## 1.5")
#' @param path a path where the current student Rmds sit in.
#'
#' @return
#' @export
#'
#' @examples none
Synthesize <- function(df_grades, mapping, path) {
  syn <- new.env()
  studentRmds <- df_grades$rmd

  syn$all <- rmdgrader:::synthesize_returnRmd2_functional(df_grades, mapping, path)
  list_syn_individual <- purrr::map(
    seq_along(studentRmds),
    ~{
      rmdgrader:::synthesize_returnRmd2_functional(df_grades[.x, ], mapping, path)
    }
  )
  names(list_syn_individual) <- studentRmds
  syn$individual <- list_syn_individual

  return(syn)
}
synthesize_returnRmd2_functional <- function(df_grades, mapping, path){
  function(){
    synthesize_returnRmd2(df_grades, mapping, path)
  }
}
synthesize_returnRmd2 <- function(df_grades, mapping, path="")
{
  names(mapping) -> ansLabels
  for(.x in seq_along(df_grades$rmd)){
    # .x=16
    rmdX <- df_grades$rmd[[.x]]
    # read in Rmd
    {
      filename = file.path(path, rmdX)
      cat(filename, "\n")
      rmdlines <- xfun::read_utf8(filename)
    }
    # clean YAML
    {
      stringr::str_which(rmdlines, "^---$") -> whichAreYMAL
      if(length(whichAreYMAL)<2){
        cat("  YAML has problem.")
        next
      }
      rmdlines_YAML <- whichAreYMAL[[1]]:whichAreYMAL[[2]]
      {
        whichRmdlines_YAMLIsGrade <-
          stringr::str_which(rmdlines[rmdlines_YAML], "grade:")
        if (length(whichRmdlines_YAMLIsGrade) != 0)
        {
          whichIsGradeYAML <-
            rmdlines_YAML[whichRmdlines_YAMLIsGrade]
          which2remove <-
            whichIsGradeYAML:(whichAreYMAL[[2]] - 1)
          rmdlines <- rmdlines[-which2remove]
        }
      }
    }
    # locate mapping lines
    {
      gradeYMALTemplate <- "  {ansLabelY}: {gradeY}"
      gradeYAMLX <- character(0)
      allGradeYAMLX <- character(0)
      for(.y in seq_along(mapping)){
        ansLabelY <- ansLabels[[.y]]
        gradeY <- df_grades[[ansLabels[[.y]]]][[.x]]
        # browser()
        gradeYAMLX <-
          paste(gradeYAMLX,
            glue::glue(gradeYMALTemplate, .trim=FALSE),
            sep="\n")
        # for each mapped line add score
        stringr::str_which(rmdlines, stringr::fixed(mapping[[.y]])) -> whichIsTarget
        if(length(whichIsTarget)!=1){
          mapping_status <- "failed"
          cat(glue::glue("  {ansLabelY}: {mapping_status}"))
          next
        }
        # remove grade
        rmdlines[[whichIsTarget]] |>
          stringr::str_remove("\\s\\([0-9]+.*\\)$") ->
          rmdlines[[whichIsTarget]]
        glue::glue("{rmdlines[[whichIsTarget]]} ({gradeY})") -> newLine
        rmdlines[[whichIsTarget]] <- newLine
      }
    }
    # editing YAML
    {
      stringr::str_which(rmdlines, "^---$") -> whichAreYMAL
      totalGradeYAML <- "grade:\n  total: {total}"
      total <- df_grades$sum[[.x]]
      allGradeYAMLX <-
        paste(
          glue::glue(totalGradeYAML, .trim=FALSE),
          gradeYAMLX, sep="")
      # cat(allGradeYAMLX)
      # add total grades to header
      allGradeYAMLX_padding <-
        paste(allGradeYAMLX, "---", sep="\n")
      rmdlines[[whichAreYMAL[[2]]]] <- allGradeYAMLX_padding
    }
    xfun::write_utf8(rmdlines, con=filename)
    # file.edit(filename)
  }
}
