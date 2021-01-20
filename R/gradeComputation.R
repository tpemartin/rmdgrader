#' get max grade out of grade vectors
#'
#' @param ... grade vectors with rmd as element names
#'
#' @return a grade vector with rmd as element names
#' @export
#'
#' @examples
#' \dontrun{
#' getMaxGrade(grade111, grade112, grade113)
#' }
getMaxGrade <- function(...){
  tb_grade <- merge_gradeVectors(...)
  do.call("pmax", tb_grade[,-1]) -> tb_grade$max
  setNames(tb_grade$max, tb_grade$rmd)
}

#' Merge several named numerical \\(grade\\) vectors into a tibble
#'
#' @param ... arguments of grade vectors
#'
#' @return A tibble with Rmd column as the merging key.
#' @export
#'
#' @examples
#' \dontrun{
#' merge_gradeVectors(grade111, grade112, grade113)
#' }
merge_gradeVectors <- function(...){
  argSyms <- rlang::ensyms(...)
  argQuos <- rlang::enquos(...)
  purrr::map(
    seq_along(argSyms),
    ~tibble::tibble(
      rmd=names(!!argQuos[[.x]]),
      !!argSyms[[.x]]:=!!argQuos[[.x]])
  ) -> list_tibbles
  purrr::reduce(list_tibbles, merge)
}
#' Convert list of grades into a data frame with total computed
#'
#' @param ... A name=value pair, such as ans11=grade11, ans12=grade12,...
#'
#' @return
#' @export
#'
#' @examples none
convert_gradeList2dataframe <- function(..., turnInBonus=3, maxPoint=10, fullMark=NULL) {
  groupvar <- list(...)
  # browser()
  names(groupvar) -> ansLabels

  seq_along(ansLabels) %>%
    map(
      ~ {
        df_temp <-
          data.frame(
            name = names(groupvar[[.x]])
          )
        df_temp[[ansLabels[[.x]]]] <- safe_unlist(groupvar[[.x]])
        df_temp
      }
    ) -> list_df
  reduce(list_df, full_join, by = "name") -> df_grades
  assertthat::assert_that(is.numeric(fullMark))
  fullMark = ifelse(is.null(fullMark), length(ansLabels), fullMark)
  df_grades %>%
    rowwise() %>%
    mutate(
      total = sum(c_across(contains("ans")), na.rm = T),
      final = min(maxPoint, turnInBonus + (maxPoint-turnInBonus)/fullMark*total)) %>%
    ungroup() %>%
    mutate(
      PR = pmin(round(100*(1-percent_rank(final)),0)+1,100)
    ) %>%
    select(name, total, final, PR, everything()) -> df_grades

  df_grades
}
