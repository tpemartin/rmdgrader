#' Convert list of grades into a data frame with total computed
#'
#' @param ... A name=value pair, such as ans11=grade11, ans12=grade12,...
#'
#' @return
#' @export
#'
#' @examples none
convert_gradeList2dataframe <- function(...) {
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
      final = 3 + 7/length(ansLabels)*total
    ) %>%
    select(name, total, final, everything()) -> df_grades

  df_grades
}
