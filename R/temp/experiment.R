correctAnsFilename <- "/Users/martinl/Github/course-dashboard-programming-for-data-science/homeworks/homework1-ans.Rmd"
root <- "/Users/martinl/Github/course-dashboard-programming-for-data-science/studentsSubmission/hw1"
studentsRmds <-  list.files(root, full.names = T) %>%
  stringr::str_subset("\\.Rmd$")
unprocessableFolder <- file.path(root,"unprocessable")
unprocessableRmds <- c()

if(!dir.exists(unprocessableFolder)) dir.create(unprocessableFolder)

.x=2
{
# for(.x in seq_along(studentsRmds)){
  studentFilename <- studentsRmds[[.x]]

  correctAnsFilename %>%
    get_codeChunkProcessed_from_filepath() -> codeChunksProcessed

  parts <- levels(codeChunksProcessed$chunkLabelsDecomposed$part)
  .x = parts[[2]]

  # prepare dataEnvironment
  prepare_dataEnvironment(correctAnsFilename, part=.x) ->> dataEnvironment

  # get ansObjNames


  # evaluate student code chunk in dataEnvironment
  tryCatch(
    {
      studentFilename %>%
        get_codeChunkProcessed_from_filepath()
    },
    error = function(e) {
      warning(studentFilename, " has format problem")
      studentFilename
    }
  ) -> codeChunksProcessed

  if(is.character(codeChunksProcessed)){
    unprocessableRmds <- c(unprocessableRmds, codeChunksProcessed)
    next
  }

  codeChunksProcessed %>%
    fillupDataEnv_with_ansEnvir()
}




