var $seatmap, $autocomplete_input, $row, $col, seatmapping;

function displaySeat(){
    student_name = $autocomplete_input.val();
    student_seat = seatmapping[student_name];
    row_col = student_seat.split(".");
    $row.text(row_col[0]);
    $col.text(row_col[1]);
}

$(document).ready(function(){
    $seatmap = $("#seatmap");
    $autocomplete_input = $("#autocomplete-input");
    $row = $("#seat-row");
    $col = $("#seat-col");
    seatmapping = JSON.parse($seatmap.text());
    $autocomplete_input.on("change", displaySeat);

  });
