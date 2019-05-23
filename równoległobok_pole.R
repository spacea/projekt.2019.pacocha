#kąt jest liczony od prawej strony osi OX do lewej (przeciwnie z ruchem wskazówek zegara)
# sin <- (sin.a * cos.b + cos.a * sin.b)
# cos <- (cos.a * cos.b - sin.a * sin.b)

area.parallelogram <- function(xs, ys, r1, r2, alpha, beta){
  
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

area.parallelogram(0,0,3,3,45,45)