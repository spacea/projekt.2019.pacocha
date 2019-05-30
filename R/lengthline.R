#' Length line
#'
#' @description Function which calculates length of a line
#' based on the position of its end points.
#'
#' @param x1 Value of the first point on the X axis.
#' @param x2 Value of the second point on the X axis.
#' @param y1 Value of the first point on the Y axis.
#' @param y2 Value of the second point on the Y axis.
#'
#' @return Numeric value
#'
#' @examples
#' length_line(0,-5,0,5)
#'
length_line <- function(x1, y1, x2, y2){
  if(is.numeric(x1) == FALSE){
    stop("First argument is non-numeric")
  } else if(is.numeric(y1) == FALSE){
    stop("Second argument is non-numeric")
  } else if(is.numeric(x2) == FALSE){
    stop("Third argument is non-numeric")
  } else if(is.numeric(y2) == FALSE){
    stop("Fourth argument is non-numeric")
  } else {
    sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  }
}

