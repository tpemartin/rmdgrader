% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grade.R
\name{exec_safe}
\alias{exec_safe}
\title{Execute transformation function safely in which when error happens it will return original value}
\usage{
exec_safe(x, fun, ...)
}
\arguments{
\item{x}{An object to be transformed}

\item{fun}{A symbol of transform function}

\item{...}{passed to the transform function}
}
\value{
either fun(x); or, when error in fun(x), return(x)
}
\description{
Execute transformation function safely in which when error happens it will return original value
}
\examples{
none
}
