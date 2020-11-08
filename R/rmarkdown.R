require(rmd2drake)
require(xfun)
require(dplyr)
require(purrr)
require(withr)
require(rlang)
require(callr)

obtain_ansObjectName <- function(x){
  x %>%
    {
      stringr::str_trim(., side="both") %>%
        purrr::keep(function(x) x!="") %>%
        {.[[length(.)]]} %>%
        stringr::str_remove("[#\\s]+")
    }
}

