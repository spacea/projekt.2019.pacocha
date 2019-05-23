#' Parallelogram area
#'
#' @description Function which calculates an area of a
#' parallelogram.
#'
#' @param xs Left down apex on the X axis.
#' @param ys Left down apex on the Y axis.
#' @param r1 Lenght of the basis.
#' @param r2 Length of the side.
#' @param alpha Angle of rotation.
#' @param beta Angle between the basis and the left side of
#' the parallelogram.
#'
#' @return Numeric vector
#' @export
#'
#' @examples
#' fo_area.par(0,0,3,3,45,45)
fo_area.par <- function(xs, ys, r1, r2, alpha, beta){

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
  } else if(is.numeric(beta) == FALSE){
    stop("Sixth argument is non-numeric")
  }

  if(beta == 0){
    sin.b <- 0
    cos.b <- 1
  } else if(beta == 30) {
    sin.b <- 1/2
    cos.b <- (sqrt(3))/2
  } else if(beta == 45) {
    sin.b <- (sqrt(2))/2
    cos.b <- (sqrt(2))/2
  } else if(beta == 60) {
    sin.b <- (sqrt(3))/2
    cos.b <- 1/2
  } else if(beta == 90) {
    sin.b <- 1
    cos.b <- 0
  } else if(beta == 120) {
    sin.b <- (sqrt(3))/2
    cos.b <- -1/2
  } else if(beta == 135) {
    sin.b <- (sqrt(2))/2
    cos.b <- -(sqrt(2))/2
  } else if(beta == 150) {
    sin.b <- 1/2
    cos.b <- -(sqrt(3))/2
  } else if(beta == 180) {
    sin.b <- 0
    cos.b <- -1
  } else if(beta == 210) {
    sin.b <- -1/2
    cos.b <- -(sqrt(3))/2
  } else if(beta == 225) {
    sin.b <- -(sqrt(2))/2
    cos.b <- -(sqrt(2))/2
  } else if(beta == 240) {
    sin.b <- -(sqrt(3))/2
    cos.b <- -1/2
  } else if(beta ==  270) {
    sin.b <- -1
    cos.b <- 0
  } else if(beta == 300) {
    sin.b <- -(sqrt(3))/2
    cos.b <- 1/2
  } else if(beta ==  315) {
    sin.b <- -(sqrt(2))/2
    cos.b <- (sqrt(2))/2
  } else if(beta == 330) {
    sin.b <- -1/2
    cos.b <- (sqrt(3))/2
  } else if(beta == 360) {
    sin.b <- 0
    cos.b <- 1
  } else {
    stop("Unfortunately, this angle is not included, please try choosing a less complex one instead.")
  }

  (r1 * r2 *sin.b)
}
