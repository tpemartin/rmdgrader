correctAnsFilename <- "/Users/martinl/Github/course-dashboard-programming-for-data-science/homeworks/homework1-ans.Rmd"
root <- "/Users/martinl/Github/course-dashboard-programming-for-data-science/studentsSubmission/hw1"

studentfilenam <-"/Users/martinl/Github/course-dashboard-programming-for-data-science/studentsSubmission/hw1/hw1_410874201.Rmd"
file.edit(studentfilenam)
require(dplyr)
studentfilenam %>%
  get_answerObjectValues(correctAnsFilename) -> results

correctAnsFilename %>%
  get_answerObjectValues(correctAnsFilename) ->xx

studentsRmds <- correctAnsFilename
{
  studentValues <- vector("list", length(studentsRmds))
  names(studentValues) <- basename(studentsRmds)
  # get Rmd structure information from answer Rmd
  {
    correctAnsFilename %>%
      get_codeChunkProcessed_from_filepath(codeChunksFromAnsFile=T) -> correctCodeChunksProcessed

    # 大題標號
    parts <- levels(correctCodeChunksProcessed$chunkLabelsDecomposed$part)

    # get ansObjNames
    ansObjectNames <<- correctCodeChunksProcessed$ansObjectnames

  }

  badRmds <- vector("list", length(parts))
  names(badRmds) <- parts
  for(.y in seq_along(parts)){
    unprocessableRmds <- c()

    # 產生某一大題回答值
    targetPart <- parts[[.y]]
    # targetLabels
    targetLabels <-
      {
        correctCodeChunksProcessed$chunkLabelsDecomposed %>%
          filter(type=="ans", part==targetPart) %>% pull(label)
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

        # 檢查codeChunks是否為合理可被執行，不合理flag_wrong_content=T
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

        # 執行合理codeChunks
        codeChunksProcessed %>%
          fillupDataEnv_with_ansEnvir(dataEnvironment, targetPart)
        studentValues[[.x]] <- append(
          studentValues[[.x]],
          dataEnvironment$answerEnvironment$ansValues)
      }
    }
    badRmds[[.y]] <- unprocessableRmds
  }
  list(
    studentValues=studentValues,
    badRmds=badRmds
  )
  }
