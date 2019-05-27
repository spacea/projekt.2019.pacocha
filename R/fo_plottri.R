#' Triangle plot
#'
#' @description Function which plots a triangle based on its apexes.
#'
#' @param x1 Value of the first point on the X axis.
#' @param x2 Value of the second point on the X axis.
#' @param x3 Value of the third point on the X axis.
#' @param y1 Value of the first point on the Y axis.
#' @param y2 Value of the second point on the Y axis.
#' @param y3 Value of the third point on the Y axis.
#'
#' @return Plot
#' @export
#'
#' @examples
#' fo_plot_tri(0, 0, 1, 0, 0, 1)

fo_plot_tri <- function(x1, y1, x2, y2, x3, y3){

  x <- c(x1, x2, x3)
  y <- c(y1, y2, y3)
  graphics::plot(x, y)
  graphics::lines(c(x[1], x[2]), c(y[1], y[2]))
  graphics::lines(c(x[2], x[3]), c(y[2], y[3]))
  graphics::lines(c(x[1], x[3]), c(y[1], y[3]))
}

