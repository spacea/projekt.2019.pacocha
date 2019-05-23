#' Square area
#'
#' @description Function which calculates an area of a square
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
#'
#' @return Numeric vector
#' @export
#'
#' @examples
#' fo_area.sqr(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(1),-1/2*sqrt(1),1/2*sqrt(1))
#' fo_area.sqr(0,0,5,0,5,5,0,5)
fo_area.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  if (fo_can.sqr(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a square")
  } else {
    a ^ 2
  }
}
