#' Parallelogram plot
#'
#' @description Function which plots a parallellogram.
#'
#' @param xs Left down apex on the X axis.
#' @param ys Left down apex on the Y axis.
#' @param r1 Lenght of the basis.
#' @param r2 Length of the side.
#' @param alpha Angle of rotation.
#' @param beta Angle between the basis and the left side of
#' the parallelogram.
#'
#' @return Plot
#' @export
#'
#' @examples
#' fo_plot.par(0,0,3,3,0,30)
fo_plot.par <- function(xs, ys, r1, r2, alpha, beta){
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

  x1 <- xs + r1 * cos.a
  y1 <- ys + r1 * sin.a

  x2 <- x1 + r2 * (cos.a * cos.b - sin.a * sin.b)
  y2 <- y1 + r2 * (sin.a * cos.b + cos.a * sin.b)

  x3 <- xs + r2 * (cos.a * cos.b - sin.a * sin.b)
  y3 <- ys + r2 * (sin.a * cos.b + cos.a * sin.b)

  x <- c(xs, x1, x2, x3)
  y <- c(ys, y1, y2, y3)

  plot(x, y)

  lines(c(x[1], x[2]), c(y[1], y[2]))
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[1], x[4]), c(y[1], y[4]))
  lines(c(x[3], x[4]), c(y[3], y[4]))

}
