#' Rectangle plot
#'
#' @description Function which plots a rectangle
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
#' @return Plot
#' @export
#'
#' @examples
#' fo_plot.rec(0,0,4,0,4,5,0,5)
#' fo_plot.rec(-5,-5,5,-5,5,5,-5,5)
fo_can.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  if(fo_can.rec(x1, y1, x2, y2, x3, y3, x4, y4) == TRUE){
    x <- c(x1, x2, x3, x4)
    y <- c(y1, y2, y3, y4)
    plot(x, y)
    lines(c(x[1], x[2]), c(y[1], y[2]))
    lines(c(x[2], x[3]), c(y[2], y[3]))
    lines(c(x[3], x[4]), c(y[3], y[4]))
    lines(c(x[4], x[1]), c(y[4], y[1]))
  } else {
    stop("The geometric figure is not a rectangle")
  }
}
