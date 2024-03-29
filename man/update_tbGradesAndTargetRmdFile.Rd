% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dispute.R
\name{update_tbGradesAndTargetRmdFile}
\alias{update_tbGradesAndTargetRmdFile}
\title{Update tb_grades and target file Rmd under the hood}
\usage{
update_tbGradesAndTargetRmdFile(
  tb_grades,
  targetFile,
  disputeFileInfo,
  maxPoint = 10
)
}
\arguments{
\item{tb_grades}{A data frame from step3 grading}

\item{targetFile}{A character of full path to a target dispute Rmd}

\item{disputeFileInfo}{A list returned from generate_disputeFilesFromIssueComments()}
}
\value{
A data frame tb_grades new
}
\description{
Update tb_grades and target file Rmd under the hood
}
\examples{
none.
}
