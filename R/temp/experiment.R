correctAnsFilename <- "/Users/martinl/Github/course-dashboard-programming-for-data-science/homeworks/homework1-ans.Rmd"
root <- "/Users/martinl/Github/course-dashboard-programming-for-data-science/studentsSubmission/hw1"

require(dplyr)
studentsRmds <-  list.files(root, full.names = T) %>%
  stringr::str_subset("\\.Rmd$")
allRmds <- c(
  correctAnsFilename,
  studentsRmds
)

# revise ans4 to ans4s
walk(
  studentsRmds,
  ~{
    xfun::read_utf8(.x) %>%
      stringr::str_replace("ans4s\\}$", "ans4s\\}") %>%
      xfun::write_utf8(.x)
  })









