#' Circle area
#'
#' @description Function which calculates an area of a
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
#' fo_area.cir(1,1,2,2)
#' fo_area.cir(345,324,577,234)

fo_area.cir <- function(x1, y1, x2 ,y2) {
  x1 <- 2
  x2 <- 2
  y1 <- 2
  y2 <- 2
  r <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  if(is.numeric(r) == FALSE){
    stop("Argument is non-numeric")
  }else {print(pi*r^2)
  }
}
fo_area.cir(0,0,5,5)
