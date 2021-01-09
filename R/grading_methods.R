point_characterTemplate <- function(){
  list(
    class=list(point=0.3), # class 對
    elementValues=list(
      point=0.5,
      weight=list(
        onlyMatchAsSetWithNonDuplicates=1,
        onlyMatchAsSet=1,
        onlyPartialMatch=0.8
      )),
    names=list(point=0) # 元素名稱 對
  )
}
grade_characterFunctional <- function(point_character){

  function(x,y){
    ifelsethen(isTRUE(all.equal(x,y)), return(1), 0, 0)
    # class
    ifelsethen(is(x, "character"), point_character$class$point , 0, 0) -> pt_class
    # names
    eval_safe(
      {
        ifelsethen(
          point_character$names!=0 && # 有要改元素名稱
            all(names(x) %in% names(y)), point_character$names$point, 0, 0)
      }
    ) -> pt_names

    # elementValues
    eval_safe(
      {
        ifelsethen(
          isTRUE(all.equal(x, y)), # exact equal
          point_character$elementValues$point,
          # match as set without duplicates
          {
            ifelsethen(
              length(x)==length(y) && all(x %in% y),
              point_character$elementValues$point*
                point_character$elementValues$weight$onlyMatchAsSetWithNonDuplicates,
              # match as set allow duplicates
              {
                ifelsethen(
                  length(setdiff(y,x))==0,
                  point_character$elementValues$point *
                    point_character$elementValues$weight$onlyMatchAsSet,
                  # partial match
                  {
                    ifelsethen(
                      any(as.character(x) %in% y),
                      sum(as.character(x) %in% y) / length(y) * point_character$elementValues$point *
                        point_character$elementValues$weight$onlyPartialMatch,
                      0
                    )
                  },
                  0
                )
              }, #
              0
            )
          },
          0
        )
      }
    ) -> pt_elementValues
    pt_class+pt_names+pt_elementValues
  }

}

point_dataframeTemplate <- function(){
  list(
    class=list(
      point=0.3
    ),
    names=list(
      point=0.2
    ),
    elementValues=list(
      point=0.5
    )
  )
}
grade_dataframeBasicfunctional <- function(point_dataframe){
  function(x,y){
    ifelsethen(is(x,"data.frame"), point_dataframe$class$point,0,0) -> pt_class
    eval_safe({
      ifelsethen(all(names(x) %in% names(y)),
                 point_dataframe$names$point,
                 {ifelsethen(any(names(x) %in% names(y)),
                             point_dataframe$names$point*sum(names(x) %in% names(y))/length(names(y)),
                             0,
                             0)},
                 0)
    }) -> pt_names
    return(pt_class+pt_names)
  }
}
point_listTemplate <- function(){
  list(
    class=list(
      point=0.3
    ),
    names=list(
      point=0.2
    ),
    elementValues=list(
      point=0.5
    )
  )
}
grade_list4classNamesfunctional <- function(point_list){
  function(x,y){
    ifelsethen(is(x,"list"), point_list$class$point,0,0) -> pt_class
    eval_safe({
      ifelsethen(
        length(setdiff(names(y), names(x)))==0 &&
          length(setdiff(names(y), names(y)))==0,
        point_list$names$point,
        {ifelsethen(any(names(x) %in% names(y)),
                    point_list$names$point*sum(names(x) %in% names(y))/length(names(y)),
                    0,
                    0)},
        0)
    }) -> pt_names
    return(pt_class+pt_names)
  }
}

