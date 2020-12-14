#' Convert list of grades into a data frame with total computed
#'
#' @param ... A name=value pair, such as ans11=grade11, ans12=grade12,...
#'
#' @return
#' @export
#'
#' @examples none
convert_gradeList2dataframe <- function(..., turnInBonus=3, maxPoint=10) {
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

  df_grades %>%
    rowwise() %>%
    mutate(
      total = sum(c_across(contains("ans")), na.rm = T),
      final = min(maxPoint, turnInBonus + (maxPoint-turnInBonus)/length(ansLabels)*total)) %>%
    ungroup() %>%
    mutate(
      PR = pmin(round(100*(1-percent_rank(final)),0)+1,100)
    ) %>%
    select(name, total, final, PR, everything()) -> df_grades

  df_grades
}
