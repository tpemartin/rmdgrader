
parseCodeChunk2Expressions <- function(x){
  x %>%
    paste0(collapse="\n") %>%
    rlang::parse_exprs()
}
