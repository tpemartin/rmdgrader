#' Instance initiator to convenience grouping task
#'
#' @param vector2group A vector of characters to be grouped.
#'
#' @return An environment instance for grouping
#' @export
#'
#' @examples none
Group <- function(vector2group){
  # vector2group <- FuntionLists
  # take care xxx() xxx
  unlist(vector2group) %>% unique() -> listfuns00
  stringr::str_remove_all(unlist(listfuns00),"\\s+") ->
    listfuns0
  as.factor(listfuns0) -> listfuns
  levels(listfuns) -> oldLevels
  stringr::str_extract(
    oldLevels, "^[^\\(]+"
  ) -> newLevels
  levels(listfuns) <- newLevels
  data.frame(
    origin=listfuns0,
    level=as.character(listfuns)
  ) -> tb_mapping

  whichIsNA <-
    which(is.na(tb_mapping$level))
  tb_mapping$level[whichIsNA] <- "_wrongExpression"
  tidyr::nest(
    tb_mapping,
    data=c(origin)
  ) -> list0
  # list0$level[[1]]
  purrr::map(
    seq_along(list0$level),
    ~{
      list0$data[[.x]]$origin
    }) -> list1
  names(list1) <- list0$level
  fe <- new.env()
#
#   whichIsNA <- which(is.na(names(list1)))
#   names(list1)[[whichIsNA]] <- "_wrongExpression"
  fe$original <- listfuns00
  # fe$groups <- {
  #   setNames(as.list(fe$original),
  #            stringr::str_remove_all(unlist(fe$original),"\\s+"))
  # }
  fe$groups <- list1
  fe$sortFirst <-
    setNames(purrr::map(
      1:6,
      sortFirstNFun,
      fe), paste0("n",1:6))
  fe$sortFirst$n6()
  fe$regroup <- regroupFun(fe)
  fe$recover <- recoverFun
  return(fe)
}




# helpers -----------------------------------------------------------------

sortFirstNFun <- function(n, fe){
  function(){
    names(fe$groups) -> groupNames
    groupNames[order(stringr::str_sub(groupNames, end=n))] -> ordered_groupNames
    fe$groups[ordered_groupNames] -> fe$groups
  }
}
regroupFun <- function(fe){
  function(beginEL, endEL, newName=NULL){
  # newName=NULL
  # beginEL<- fe$groups$all
  # endEL <- fe$groups$`all()`

  which(
    purrr::map_lgl(
      fe$groups,
      ~dplyr::setequal(.x,beginEL))) -> whichIsStart
  which(
    purrr::map_lgl(
       fe$groups,
      ~dplyr::setequal(.x,endEL)))  -> whichIsEnd

  if(is.null(newName)) newName = names(fe$groups[whichIsStart])

  unlist(fe$groups[whichIsStart:whichIsEnd]) -> newGroup
  fe$.restore <- fe$groups
  fe$groups[whichIsStart:whichIsEnd] <- NULL
  fe$groups[[newName]] <- newGroup
  fe$sortFirst$n6()
}}
recoverFun <- function(fe){
  function(){
    fe$groups <- fe$.restore
  }
}
