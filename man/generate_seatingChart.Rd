% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seatingChart.R
\name{generate_seatingChart}
\alias{generate_seatingChart}
\title{Generate seating chart}
\usage{
generate_seatingChart(seatChartUrl, sheet_seatChart, student_names, sheet)
}
\arguments{
\item{seatChartUrl}{A link to the seating chart google sheets}

\item{sheet_seatChart}{The sheet name that has seating chart with non-seats marked X}

\item{student_names}{A character}

\item{sheet}{The name of assigned seat chart}
}
\value{

}
\description{
Generate seating chart
}
\examples{
seatingChartUrl <- "https://docs.google.com/spreadsheets/d/14jQT6tvbc7Xv_ID1eoOIqUA3q_4HujjMYthcelebdlw/edit#gid=0"
students <- {
  "https://docs.google.com/spreadsheets/d/1fs61_qTY4IYqtzWgg7VNbJ_igwJaVgRPYVjvodre5JI/edit#gid=1259661119" |>
    googlesheets4::read_sheet(
      sheet="完整資訊"
    )
}
rmdgrader::generate_seatingChart(
  seatChartUrl = seatingChartUrl,
  sheet_seatChart = "Sheet1",
  student_names = students$姓名,
  sheet = "new assigned-seating chart")
}
