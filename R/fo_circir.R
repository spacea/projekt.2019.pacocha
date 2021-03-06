#' Circle circumference
#'
#' @description Function which calculates a circumference of a
#' circle.
#'
#' @param x1 Center point on the X axis.
#' @param y1 Center point on the Y axis.
#' @param x2 Point on the circumference on the X axis.
#' @param y2 Point on the circumference on the Y axis.
#'
#' @return Numeric vector
#' @export
#'
#' @examples
#' fo_cir_cir(1,1,2,2)
#' fo_cir_cir(123,254,234,764)
fo_cir_cir <- function(x1,y1,x2,y2) { #cirumference
  r <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  if(is.numeric(r) == FALSE){
    stop("Argument is non-numeric")
  }else {print(2*pi*r)
  }
}
