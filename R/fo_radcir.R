#' Circle radius
#'
#' @description Function which calculates a radius of a
#' circle.
#'
#' @param x1 Center point on the X axis.
#' @param y1 Center point on the Y axis.
#' @param x2 Point on the circumference on the X axis.
#' @param y2 Point on the circumference on the Y axis.
#'
#' @return Numerical vector
#' @export
#'
#' @examples
#' fo_rad.cir(1,1,2,2)
fo_rad.cir <- function(x1,y1,x2,y2){
  if(is.numeric(x1) == FALSE){
    stop("First argument is non-numeric")
  } else if(is.numeric(y1) == FALSE){
    stop("Second argument is non-numeric")
  } else if(is.numeric(x2) == FALSE){
    stop("Third argument is non-numeric")
  } else if(is.numeric(y2) == FALSE){
    stop("Fourth argument is non-numeric")
  } else {
    r <-   sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
    cat(r)
  }
}

