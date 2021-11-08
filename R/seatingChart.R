#' Generate seating chart
#'
#' @param seatChartUrl A link to the seating chart google sheets
#' @param sheet_seatChart The sheet name that has seating chart with non-seats marked X
#' @param student_names A character
#' @param sheet The name of assigned seat chart
#'
#' @return
#' @export
#'
#' @examples
#' seatingChartUrl <- "https://docs.google.com/spreadsheets/d/14jQT6tvbc7Xv_ID1eoOIqUA3q_4HujjMYthcelebdlw/edit#gid=0"
#' students <- {
#'   "https://docs.google.com/spreadsheets/d/1fs61_qTY4IYqtzWgg7VNbJ_igwJaVgRPYVjvodre5JI/edit#gid=1259661119" |>
#'     googlesheets4::read_sheet(
#'       sheet="完整資訊"
#'     )
#' }
#' rmdgrader::generate_seatingChart(
#'   seatChartUrl = seatingChartUrl,
#'   sheet_seatChart = "Sheet1",
#'   student_names = students$姓名,
#'   sheet = "new assigned-seating chart")
generate_seatingChart <- function(
  seatChartUrl, sheet_seatChart, student_names, sheet){

  seatingTemplate <- {
    seatChartUrl |>
      googlesheets4::read_sheet(sheet=sheet_seatChart, col_names = F) -> seatingTemplate

    colnames(seatingTemplate) <-
      LETTERS[1:ncol(seatingTemplate)]
    seatingTemplate
  }
  goodSeatChart <- {
    expand.grid(
      rownames(seatingTemplate) |> as.integer(),
      colnames(seatingTemplate),
      stringsAsFactors = F) -> all_the_seats

    whichIsGood <- which(seatingTemplate != "X")

    all_the_seats[whichIsGood, ]
  }

  assigned_seats <- {
    sample(
      seq_along(goodSeatChart$Var1),
      length(student_names),
      replace=F) -> assigned_indices
    goodSeatChart$student_names = ""
    goodSeatChart$student_names[assigned_indices] = student_names

    goodSeatChart |>
      filter(student_names !="") |>
      rename(".row"="Var1", ".col"="Var2")
  }
  seatChart_with_names <- {

    seatChart_with_names <- seatingTemplate
    for(.x in seq_along(assigned_seats$student_names))
    {
      assigned_seat_x <- assigned_seats[.x, ]
      seatChart_with_names[[assigned_seat_x$.col]][[assigned_seat_x$.row]] <- assigned_seat_x$student_names
    }

    seatChart_with_names |>
      unlist() |>
      stringr::str_which("%") -> whichHasNoName
    seatChart_with_names |>
      as.matrix() -> seatChart_with_names_matrix
    seatChart_with_names_matrix[whichHasNoName] <- ""
    seatChart_with_names_matrix |> as.data.frame()
  }

  # Upload to google sheets
  {
    ss = googlesheets4::as_sheets_id(seatChartUrl)
    googlesheets4::write_sheet(
      data = seatChart_with_names,
      sheet = sheet,
      ss=ss
    )
  }
}
