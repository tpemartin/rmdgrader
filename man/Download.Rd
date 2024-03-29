% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{Download}
\alias{Download}
\title{Generate a download instance}
\usage{
Download(title, path)
}
\arguments{
\item{title}{the title of the course work.}

\item{path}{the path where files downloaded to.}
}
\value{

}
\description{
the instance is equipped with get_courseWork method which requires a google drive folder URL, class roster with 姓名, 學號 and Google classroom login email (the name of this column is also required). Once applied get_courseWork method, user can Download$courseWork$download(overwrite=F, validSchoolIds=all school Ids) to download those work from validSchoolIds
}
\examples{
none
}
