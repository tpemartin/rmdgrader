% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grade.R
\name{grade}
\alias{grade}
\title{Grade answer with given grading method}
\usage{
grade(targetLabel, gradingMethod, whichCorrectAnsvalue = 1)
}
\arguments{
\item{targetLabel}{A character, such as "ans11".}

\item{gradingMethod}{A function with three arguments: student answer value, correct answer value, errorMsg}

\item{whichCorrectAnsvalue}{An number, default=1, represents which correct answer to use}
}
\value{
A list of grades
}
\description{
there must be studentValues and correctValues two list of answer object values proccessed from student Rmds and teacher Rmd
}
\examples{
none
}
