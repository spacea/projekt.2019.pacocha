#' Quadrangle parimiter
#'
#' @description Function which calculates a quadrangle parimiter
#' based on its apexes positions.
#'
#' @param x1 Value of the first point on the X axis.
#' @param x2 Value of the second point on the X axis.
#' @param x3 Value of the third point on the X axis.
#' @param x4 Value of the fourth point on the X axis.
#' @param y1 Value of the first point on the Y axis.
#' @param y2 Value of the second point on the Y axis.
#' @param y3 Value of the third point on the Y axis.
#' @param y3 Value of the fourth point on the Y axis.
#'
#' @return Numeric vector
#' @export
#'
#' @examples
#' fo_par.quad(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(2),-1/2*sqrt(1),1/2*sqrt(1))
#' fo_par.quad(-2,2,3,3,7,7,0,20)
fo_par.quad <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x3, y3, x4, y4)
  d <- length.line(x4, y4, x1, y1)
  a + b + c + d
}