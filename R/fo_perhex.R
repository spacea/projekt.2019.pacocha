#' Hexagon perimeter
#'
#' @description Function which calculates a perimeter of a hexagon.
#'
#' @param xs Center point value on the X axis.
#' @param ys Center point value on the Y axis.
#' @param r Lenght from the center to the apex.
#' @param alpha Angle of rotation.
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

