#' Rectangle diamiter
#'
#' @description Function which calculates a diamiter of a rectangle
#' based on its apexes.
#'
#' @param x1 Value of the first point on the X axis.
#' @param x2 Value of the second point on the X axis.
#' @param x3 Value of the third point on the X axis.
#' @param x4 Value of the fourth point on the X axis.
#' @param y1 Value of the first point on the Y axis.
#' @param y2 Value of the second point on the Y axis.
#' @param y3 Value of the third point on the Y axis.
#' @param y4 Value of the fourth point on the Y axis.
#' @return Numeric vector
#' @export
#'
#' @examples
#' fo_dia_rec(0,0,4,0,4,5,0,5)
#' fo_dia_rec(-5,-5,5,-5,5,5,-5,5)
fo_dia_rec <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length_line(x1, y1, x2, y2)
  b <- length_line(x2, y2, x3, y3)
  if (fo_can_rec(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a rectangle")
  } else {
    sqrt(a ^ 2 * b ^ 2)
  }
}

