#' Square creation
#'
#' @description Function which tests whether the figure is a square
#' based on its apexes.
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
#' @return Logical value
#' @export
#'
#' @examples
#' fo_can.sqr(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(1),-1/2*sqrt(1),1/2*sqrt(1))
#' fo_can.sqr(0,0,5,0,5,5,0,5)
fo_can.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x3, y3, x4, y4)
  d <- length.line(x4, y4, x1, y1)

  if(angle.cos(x1, y1, x2, y2, x4, y4) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a square")
  }
  if(angle.cos(x2, y2, x3, y3, x1, y1) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a square")
  }
  if(angle.cos(x3, y3, x4, y4, x2, y2) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a square")
  }
  if(angle.cos(x4, y4, x1, y1, x3, y3) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a square")
  }
  if(a == b){
    TRUE
  } else {
    stop("The geometric figure is not a square")
  }
  if(a == c){
    TRUE
  } else {
    stop("The geometric figure is not a square")
  }
  if(a == d){
    TRUE
  } else {
    stop("The geometric figure is not a square")
  }
  if(b == c){
    TRUE
  } else {
    stop("The geometric figure is not a square")
  }
  if(b == d){
    TRUE
  } else {
    stop("The geometric figure is not a square")
  }
  if(c == d){
    TRUE
  } else {
    stop("The geometric figure is not a square")
  }

}
