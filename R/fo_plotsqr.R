#' Square plot
#'
#' @description Function which plots a square
#' based on its apexes positions.
#'
#' @param x1 Value of the first point on the X axis.
#' @param x2 Value of the second point on the X axis.
#' @param x3 Value of the third point on the X axis.
#' @param x4 Value of the fourth point on the X axis.
#' @param y1 Value of the first point on the Y axis.
#' @param y2 Value of the second point on the Y axis.
#' @param y3 Value of the third point on the Y axis.
#' @param y4 Value of the fourth point on the Y axis.
#'
#' @return Plot
#' @export
#'
#' @examples
#' fo_plot_sqr(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(1),-1/2*sqrt(1),1/2*sqrt(1))
#' fo_plot_sqr(0,0,5,0,5,5,0,5)
#' fo_plot_sqr(0,0,5,0,5,5,0,5)
fo_plot_sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  if(fo_can_sqr(x1, y1, x2, y2, x3, y3, x4, y4) == TRUE){
    x <- c(x1, x2, x3, x4)
    y <- c(y1, y2, y3, y4)
    graphics::plot(x, y)
    graphics::lines(c(x[1], x[2]), c(y[1], y[2]))
    graphics::lines(c(x[2], x[3]), c(y[2], y[3]))
    graphics::lines(c(x[3], x[4]), c(y[3], y[4]))
    graphics::lines(c(x[4], x[1]), c(y[4], y[1]))
  } else {
    stop("The geometric figure is not a square")
  }
}
