#' Hexagon creation
#'
#' @description Function which tests whether the figure is a hexagon
#' based on its apexes.
#'
#' @param x1 Value of the first point on the X axis.
#' @param x2 Value of the second point on the X axis.
#' @param x3 Value of the third point on the X axis.
#' @param x4 Value of the fourth point on the X axis.
#' @param y1 Value of the first point on the Y axis.
#' @param y2 Value of the second point on the Y axis.
#' @param y3 Value of the third point on the Y axis.
#' @param y4 Value of the fourth point on the Y axis.
#' @param y5 Value of the fifth point on the Y axis.
#' @param y6 Value of the sixth point on the Y axis.
#' @param x5 Value of the fifth point on the X axis.
#' @param x6 Value of the sixth point on the X axis.
#'
#' @return Logical value
#' @export
#'
#' @examples
#' fo_can.hex(0,0,1,0)
#'
fo_can.hex <- function(x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6){
  if(!(angle.cos(x1, y1, x2, y2, x6, y6) = -0.5)){
    FALSE
  } else if(!(angle.cos(x2, y2, x3, y3, x1, y1) = -0.5)){
    FALSE
  } else if(!(angle.cos(x3, y3, x4, y4, x2, y2) = -0.5)){
    FALSE
  } else if(!(angle.cos(x4, y4, x5, y5, x3, y3) = -0.5)){
    FALSE
  } else if(!(angle.cos(x5, y5, x6, y6, x4, y4) = -0.5)){
    FALSE
  } else if(!(angle.cos(x6, y6, x1, y1, x5, y5) = -0.5)){
    FALSE
  } else {
    TRUE
  }
}






