#' Generate a list of point distribution template for using in grade_factorFunctional()
#'
#' @return A list of point distribution the point element values should summed up to one. As to weight, it is the weight applied to the point in each situation.
#' @export
#'
#' @examples none
point_factorTemplate <- function(){
  point_class =
    list(
      point = 0.2,
      weight=list(
        unordered = 1,
        ordered = 0.5
      )
    )
  point_levels =
    list(
      point = 0.4,
      weight =
        list(
          correctLevelsAsSetEqual=0
        )
    )
  point_content = list(
    point=0.4,
    weight=list(
      onlySetEqual=0.5
    )
  )

  list(
    class=point_class,
    levels=point_levels,
    content=point_content
  )
}
#' Generate a grading function for factor vector
#'
#' @param point_factor A list follows the template return from point_factorTemplate()
#'
#' @return A function whose implementation will yield a list of point and details
#' @export
#'
#' @examples none.
grade_factorFunctional <- function(point_factor){
  point_class <- point_factor$class
  point_levels <- point_factor$levels
  point_content <- point_factor$content
  grade_factor <- function(x,y){
    require(future)
    # correct class
    cr0 %<-% ifelsethen(
      is.factor(x),
      1,0,0)

    # correct level sequence
    cr1.1 %<-%
      ifelsethen(
        identical(levels(x),levels(y)),
        1,0,0)
    # correct level as set
    cr1.2 %<-%
      ifelsethen(
        dplyr::setequal(levels(x),levels(y)),
        1,0,0)
    # correct element values: set equal
    cr2 %<-%
      ifelsethen(dplyr::setequal(as.character(x), as.character(y)),
                 1,0,0)
    # correct element values: identical
    cr3 %<-% ifelsethen(
      all(as.character(x)==as.character(y)),
      1,0,0)
    cr4 %<-% ifelsethen(
      is.ordered(x),
      1,0,0
    )
    # assess class
    {
      pointClass = ifelsethen(
        cr0,
        ifelsethen(
          cr4,
          point_class$weight$ordered,
          point_class$weight$unordered,
          0
        ),
        0,0
      ) * point_class$point
    }
    # assess levels
    {
      pointLevels = {
        ifelsethen(
          cr0,
          ifelsethen(
            cr1.1,
            1,
            ifelsethen(
              cr1.2,
              point_levels$weight$correctLevelsAsSetEqual*cr1.2,
              0,0
            ),0
          ),
          0,0) * point_levels$point
      }}
    # assess content
    {
      pointContent =
        ifelsethen(
          cr3,
          1,
          ifelsethen(
            cr2,
            point_content$weight$onlySetEqual,
            0,0
          ),0
        ) * point_content$point
    }

    list(
      point=pointClass+pointLevels+pointContent,
      details=list(
        classCorrect=cr0,
        levelsCorrectSequence=cr1.1,
        levelsCorrectAsSet=cr1.2,
        contentCorrectIdentical=cr3,
        contentCorrectAsSet=cr2,
        isOrdered=cr4
      )
    )
  }
}
