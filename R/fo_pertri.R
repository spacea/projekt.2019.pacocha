#' Triangle perimiter
#'
#' @description Function which calculates perimiter of a triangle.
#'
#' @param x1 Value of the first point on the X axis.
#' @param x2 Value of the second point on the X axis.
#' @param x3 Value of the third point on the X axis.
#' @param y1 Value of the first point on the Y axis.
#' @param y2 Value of the second point on the Y axis.
#' @param y3 Value of the third point on the Y axis.
#'
#' @return Numeric value
#' @export
#'
#' @examples
#' fo_per.tri(0,0,0,1,15,15)
fo_per.tri <- function(x1, y1, x2, y2, x3, y3){
  if(is.numeric(x1) == FALSE){
    stop("First argument is non-numeric")
  } else if(is.numeric(y1) == FALSE){
    stop("Second argument is non-numeric")
  } else if(is.numeric(x2) == FALSE){
    stop("Third argument is non-numeric")
  } else if(is.numeric(y2) == FALSE){
    stop("Fourth argument is non-numeric")
  } else if(is.numeric(x3) == FALSE){
    stop("Fifth argument is non-numeric")
  } else if(is.numeric(y3) == FALSE){
    stop("Sixth argument is non-numeric")
  } else {
    a <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
    b <- sqrt(((x3 - x2)^2) + ((y3 - y2)^2))
    c <- sqrt(((x1 - x3)^2) + ((y1 - y3)^2))
    a + b + c
  }
}

