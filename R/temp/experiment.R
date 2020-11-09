correctAnsFilename <- "/Users/martinl/Github/course-dashboard-programming-for-data-science/homeworks/homework1-ans.Rmd"
root <- "/Users/martinl/Github/course-dashboard-programming-for-data-science/studentsSubmission/hw1"

require(dplyr)
studentsRmds <-  list.files(root, full.names = T) %>%
  stringr::str_subset("\\.Rmd$")

# revise ans4 to ans4s
walk(
  studentsRmds,
  ~{
    xfun::read_utf8(.x) %>%
      stringr::str_replace("ans4s\\}$", "ans4s\\}") %>%
      xfun::write_utf8(.x)
  })


{
  studentValues <- vector("list", length(studentsRmds))
  names(studentValues) <- basename(studentsRmds)
  # get Rmd structure information from answer Rmd
  {
    correctAnsFilename %>%
      get_codeChunkProcessed_from_filepath() -> correctCodeChunksProcessed

    # 大題標號
    parts <- levels(correctCodeChunksProcessed$chunkLabelsDecomposed$part)

    # get ansObjNames
    ansObjectNames <<- correctCodeChunksProcessed$ansObjectnames

  }

  # prepare folder for unprocessable Rmds
  unprocessableFolder <- file.path(root,"unprocessable")
  if(!dir.exists(unprocessableFolder)) dir.create(unprocessableFolder)
  unprocessableRmds <- c()


  # 產生某一大題回答值
  targetPart <- parts[[3]]
  # targetLabels
  targetLabels <-
    {
      intersect(
        names(ansObjectNames),
        {
          correctCodeChunksProcessed$chunkLabelsDecomposed %>%
            filter(part==targetPart) %>% pull(label)
        }
      )
    }

  # prepare dataEnvironment for the part
  prepare_dataEnvironment(correctAnsFilename, part=targetPart) ->> dataEnvironment

  {
    # 針對某一份Rmd[[.x]]
    # .x=32
    for(.x in seq_along(studentsRmds))
    {
      studentFilename <- studentsRmds[[.x]]
      cat('.x = ',.x, "; ",basename(studentFilename),"\n")


      # Process student code chunks
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


      flag_wrong_content <-
        tryCatch({
          !all(targetLabels %in% names(codeChunksProcessed[["chunkExpressions"]]))},
          error=function(e){
            T
          })
      if(flag_wrong_content){
        warning("Rmd file content is incorrect")
        unprocessableRmds <- c(unprocessableRmds, studentsRmds[[.x]])
        next
      }
      codeChunksProcessed %>%
        fillupDataEnv_with_ansEnvir(dataEnvironment, targetPart)
      studentValues[[.x]] <- append(
        studentValues[[.x]],
        dataEnvironment$answerEnvironment$ansValues)
    }
  }

}






