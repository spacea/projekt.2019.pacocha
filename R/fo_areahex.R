#' Hexagon area
#'
#' @description Function which calculates an area of a hexagon
#' based on its apexes.
#'
#' @param x1 Value of the center point on the X axis.
#' @param x2 Value of the center point on the X axis.
#' @param x3 Length of a diamiter.
#'
#' @return Numeric value
#' @export
#'
#' @examples
#' fo_area.hex(1,0,3, 45)
#'
fo_area.hex <- function(xs, ys, r, alpha = 0){
  if(is.numeric(xs) == FALSE){
    stop("First argument is non-numeric")
  } else if(is.numeric(ys) == FALSE){
    stop("Second argument is non-numeric")
  } else if(is.numeric(r) == FALSE){
    stop("Third argument is non-numeric")
  } else if(is.numeric(alpha) == FALSE){
    stop("Fourth argument is non-numeric")
  } else if(alpha == 0){
    sin.a <- 0
    cos.a <- 1
  } else if(alpha == 30) {
    sin.a <- 1/2
    cos.a <- (sqrt(3))/2
  } else if(alpha == 45) {
    sin.a <- (sqrt(2))/2
    cos.a <- (sqrt(2))/2
  } else if(alpha == 60) {
    sin.a <- (sqrt(3))/2
    cos.a <- 1/2
  } else if(alpha == 90) {
    sin.a <- 1
    cos.a <- 0
  } else if(alpha == 120) {
    sin.a <- (sqrt(3))/2
    cos.a <- -1/2
  } else if(alpha == 135) {
    sin.a <- (sqrt(2))/2
    cos.a <- -(sqrt(2))/2
  } else if(alpha == 150) {
    sin.a <- 1/2
    cos.a <- -(sqrt(3))/2
  } else if(alpha == 180) {
    sin.a <- 0
    cos.a <- -1
  } else if(alpha == 210) {
    sin.a <- -1/2
    cos.a <- -(sqrt(3))/2
  } else if(alpha == 225) {
    sin.a <- -(sqrt(2))/2
    cos.a <- -(sqrt(2))/2
  } else if(alpha == 240) {
    sin.a <- -(sqrt(3))/2
    cos.a <- -1/2
  } else if(alpha ==  270) {
    sin.a <- -1
    cos.a <- 0
  } else if(alpha == 300) {
    sin.a <- -(sqrt(3))/2
    cos.a <- 1/2
  } else if(alpha ==  315) {
    sin.a <- -(sqrt(2))/2
    cos.a <- (sqrt(2))/2
  } else if(alpha == 330) {
    sin.a <- -1/2
    cos.a <- (sqrt(3))/2
  } else if(alpha == 360) {
    sin.a <- 0
    cos.a <- 1
  } else {
    stop("Unfortunately, this angle is not included, please try choosing a less complex one instead.")
  }

  x1 <- xs + r * cos.a
  y1 <- ys + r * sin.a

  x2 <- x1 + r * (cos.a * 1/2 - sin.a * ((sqrt(3))/2))
  y2 <- y1 + r * (sin.a * 1/2 + cos.a * ((sqrt(3))/2))

  x3 <- x2 + r * (cos.a * (-1/2) - sin.a * ((sqrt(3))/2))
  y3 <- y2 + r * (sin.a * (-1/2) + cos.a * ((sqrt(3))/2))

  x4 <- x3 + r * (cos.a * (-1) - sin.a * 0)
  y4 <- y3 + r * (sin.a * (-1) + cos.a * 0)

  x5 <- x4 + r * (cos.a * (-1/2) - sin.a * ((-sqrt(3))/2))
  y5 <- y4 + r * (sin.a * (-1/2) + cos.a * ((-sqrt(3))/2))

  ((3 * r^2 * sqrt(3)) / 2)
}
