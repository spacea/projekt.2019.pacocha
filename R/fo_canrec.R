#' Rectangle creation
#'
#' @description Function which tests whether the figure is a rectangle
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
#' fo_can.rec(0,0,4,0,4,5,0,5)
#' fo_can.rec(-5,-5,5,-5,5,5,-5,5)
fo_can.rec <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x3, y3, x4, y4)
  d <- length.line(x4, y4, x1, y1)
  if(angle.cos(x1, y1, x2, y2, x4, y4) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
  if(angle.cos(x2, y2, x3, y3, x1, y1) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
  if(angle.cos(x3, y3, x4, y4, x2, y2) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
  if(angle.cos(x4, y4, x1, y1, x3, y3) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
  if (a == c){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
  if (b == d){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
}
