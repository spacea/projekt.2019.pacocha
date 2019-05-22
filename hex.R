#regular hexagon

canit.hex <- function(x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6){
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

canit.hex(0,0,1,0)


#kąt jest liczony od prawej strony osi OX do lewej (przeciwnie z ruchem wskazówek zegara)
# sin <- (sin.a * cos.b + cos.a * sin.b)
# cos <- (cos.a * cos.b - sin.a * sin.b)



complex.hex <- function(xs, ys, r, alpha = 0){
  
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
  
  x <- c(xs, x1, x2, x3, x4, x5)
  y <- c(ys, y1, y2, y3, y4, y5)
  
  plot(x, y)
  
  lines(c(x[1], x[2]), c(y[1], y[2]))
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[3], x[4]), c(y[3], y[4]))
  lines(c(x[4], x[5]), c(y[4], y[5]))
  lines(c(x[5], x[6]), c(y[5], y[6]))
  lines(c(x[6], x[1]), c(y[6], y[1]))
}

complex.hex(1,0,3, 99)


