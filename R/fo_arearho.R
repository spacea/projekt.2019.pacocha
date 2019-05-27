#' Rhombus area
#'
#' @description Function which calculates an area of rhombus.
#'
#' @param xs Center point value on the X axis.
#' @param ys Center point value on the Y axis.
#' @param r1 Length from the center to the apex.
#' @param r2 Length from the center to the apex.
#' @param alpha Angle of rotation.
#'
#' @return Numeric value
#' @export
#'
#' @examples
#' fo_area_rho(0,0,3,5,30)
fo_area_rho <- function(xs, ys, r1, r2, alpha){
  if(is.numeric(xs) == FALSE){
    stop("First argument is non-numeric")
  } else if(is.numeric(ys) == FALSE){
    stop("Second argument is non-numeric")
  } else if(is.numeric(r1) == FALSE){
    stop("Third argument is non-numeric")
  } else if(is.numeric(r2) == FALSE){
    stop("Fourth argument is non-numeric")
  } else if(is.numeric(alpha) == FALSE){
    stop("Fifth argument is non-numeric")
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

  x1 <- xs + r1 * cos.a
  y1 <- ys + r1 * sin.a

  x2 <- xs + r2 * (cos.a * 0 - sin.a * 1)
  y2 <- ys + r2 * (sin.a * 0 + cos.a * 1)

  x3 <- xs + r1 * (cos.a * -1 - sin.a * 0)
  y3 <- ys + r1 * (sin.a * -1 + cos.a * 0)

  x4 <- xs + r2 * (cos.a * 0 - sin.a * -1)
  y4 <- ys + r2 * (sin.a * 0 + cos.a * -1)

  e <- sqrt(((x3 - x1)^2) + ((y3 - y1)^2))
  f <- sqrt(((x4 - x2)^2) + ((y4 - y2)^2))
  (e*f) / 2
}
