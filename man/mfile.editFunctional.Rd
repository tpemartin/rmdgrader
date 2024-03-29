% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groupTools.R
\name{mfile.editFunctional}
\alias{mfile.editFunctional}
\title{Generate a list of file.edit for each student Rmd}
\usage{
mfile.editFunctional(studentRmds, submissionPath)
}
\arguments{
\item{studentRmds}{A character vector of student Rmds}

\item{submissionPath}{A character value specifying the path of the above files}
}
\value{
A list of file.edit functions
}
\description{
Generate a list of file.edit for each student Rmd
}
\examples{
none
\dontrun{
  studentRmds <- allRmds[-1]
  submissionPath <-
    file.path(
      .root(),
      params$submissionFolder,
      params$title
    )
  mfile.edit <- mfile.editFunctional(studentRmds, submissionPath)
}
}
