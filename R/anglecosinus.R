#' Angle cosinus
#'
#' @description Function which measures a cosinus of an angle.
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
#' angle_cos(0, 0, 1, 0, 1, 1)
angle_cos <- function(x1, y1, x2, y2, x3, y3){
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
    ux1x2 <- c((x2 - x1), (y2 - y1))
    ux2x3 <- c((x3 - x2), (y3 - y2))
    ux1x3 <- c((x3 - x1), (y3 - y1))

    ((ux1x2[1] * ux1x3[1]) + (ux1x2[2] * ux1x3[2])) / (sqrt((ux1x2[1])^2 + (ux1x2[2]^2)) * sqrt((ux1x3[1])^2 + (ux1x3[2]^2)))
  }
}

