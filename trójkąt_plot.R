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

plot.triangle(1,1,2,22,13,7)