#' Hexagon perimiter
#'
#' @description Function which calculates perimiter of a hexagon.
#'
#' @param x1 Value of the first point on the X axis.
#' @param x2 Value of the second point on the X axis.
#' @param x3 Value of the third point on the X axis.
#' @param x4 Value of the fourth point on the X axis.
#' @param y1 Value of the first point on the Y axis.
#' @param y2 Value of the second point on the Y axis.
#' @param y3 Value of the third point on the Y axis.
#' @param y4 Value of the fourth point on the Y axis.
#' @param y5 Value of the fifth point on the Y axis.
#' @param y6 Value of the sixth point on the Y axis.
#' @param x5 Value of the fifth point on the X axis.
#' @param x6 Value of the sixth point on the X axis.
#'
#' @return Numeric value
#' @export
#'
#' @examples
#' fo_per.hex(1,0,3, 45)
#'
fo_per.hex <- function(xs, ys, r, alpha = 0){
  if(is.numeric(xs) == FALSE){
    stop("First argument is non-numeric")
  } else if(is.numeric(ys) == FALSE){
    stop("Second argument is non-numeric")
  } else if(is.numeric(r) == FALSE){
    stop("Third argument is non-numeric")
  } else if(is.numeric(alpha) == FALSE){
    stop("Fourth argument is non-numeric")
  } else {
  6 * r
  }
}

