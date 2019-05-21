#' Rysowanie trójkąta
#'
#' @description Funkcja rysująca trójkąt wykorzystując do tego współrzędne wierzchołków.
#'
#' @param plot.triangle
#' x1, y1 <- współrzędne x i y pierwszego wierzchołka
#' x2, y2 <- współrzędne x i y drugiego wierzchołka
#' x3, y3 <- współrzędne x i y trzeciego wierzchołka
#'
#' @return rysunek trójkąta o określonych przez uzytkownika parametrach
#' @export
#'
#' @examples
#' plot.triangle(0, 0, 1, 0, 0, 1)

plot.triangle <- function(x1, y1, x2, y2, x3, y3){
  
  x <- c(x1, x2, x3)
  y <- c(y1, y2, y3)
  plot(x, y)
  lines(c(x[1], x[2]), c(y[1], y[2]))
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[1], x[3]), c(y[1], y[3]))
}


length.line <- function(x1, y1, x2, y2){
  if(is.numeric(x1) == FALSE){
    stop("First argument is non-numeric")
  } else if(is.numeric(y1) == FALSE){
    stop("Second argument is non-numeric")
  } else if(is.numeric(x2) == FALSE){
    stop("Third argument is non-numeric")
  } else if(is.numeric(y2) == FALSE){
    stop("Fourth argument is non-numeric")
  } else {
    sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  }
}

length.line(0,-10,0,10)


height.triangle <- function(x1, y1, x2, y2, x3, y3){
  2 * area.triangle(x1, y1, x2, y2, x3, y3) / length.line(x1, y1, x2, y2)
}


height.triangle(0, 0, 10, 0, 0, 10)


canit.triangle <- function(x1, y1, x2, y2, x3, y3){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x1, y1, x3, y3)
  sorted <- sort(c(a, b, c))
  if((sorted[1]) + (sorted[2]) > (sorted[3])){
    TRUE
  } else {
    FALSE
  }
}

canit.triangle(0,0,0,1,15,15)



perimeter.triangle <- function(x1, y1, x2, y2, x3, y3){
  
  x <- c(x1, x2, x3)
  y <- c(y1, y2, y3)
  plot(x, y)
  lines(c(x[1], x[2]), c(y[1], y[2]))
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[1], x[3]), c(y[1], y[3]))
  
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x1, y1, x3, y3)
  a + b + c
}

perimeter.triangle(0,0,0,1,15,15)



area.triangle <- function(x1, y1, x2, y2, x3, y3){
  
  x <- c(x1, x2, x3)
  y <- c(y1, y2, y3)
  plot(x, y)
  lines(c(x[1], x[2]), c(y[1], y[2]))
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[1], x[3]), c(y[1], y[3]))
  
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
    (1/2) * abs((x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1))
  }
}

area.triangle(0,2,0,10,10,5)