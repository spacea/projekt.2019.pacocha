centerplot.hex <- function(xs, ys, r, alpha = 0){
  
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
  
  x2 <- xs + r * (cos.a * 1/2 - sin.a * ((sqrt(3))/2))
  y2 <- ys + r * (sin.a * 1/2 + cos.a * ((sqrt(3))/2))
  
  x3 <- xs + r * (cos.a * (-1/2) - sin.a * ((sqrt(3))/2))
  y3 <- ys + r * (sin.a * (-1/2) + cos.a * ((sqrt(3))/2))
  
  x4 <- xs + r * (cos.a * (-1) - sin.a * 0)
  y4 <- ys + r * (sin.a * (-1) + cos.a * 0)
  
  x5 <- xs + r * (cos.a * (-1/2) - sin.a * ((-sqrt(3))/2))
  y5 <- ys + r * (sin.a * (-1/2) + cos.a * ((-sqrt(3))/2))
  
  x6 <- xs + r * (cos.a * (1/2) - sin.a * ((-sqrt(3))/2))
  y6 <- ys + r * (sin.a * (1/2) + cos.a * ((-sqrt(3))/2))
  
  x <- c(xs, x1, x2, x3, x4, x5, x6)
  y <- c(ys, y1, y2, y3, y4, y5, y6)
  
  plot(x, y)
  
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[3], x[4]), c(y[3], y[4]))
  lines(c(x[4], x[5]), c(y[4], y[5]))
  lines(c(x[5], x[6]), c(y[5], y[6]))
  lines(c(x[6], x[7]), c(y[6], y[7]))
  lines(c(x[7], x[2]), c(y[7], y[2]))
}

centerplot.hex(0,0,3,45)
