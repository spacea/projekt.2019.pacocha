
# Długość linii

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

# Kwadrat

# Czy figura może być kwadratem? Warunek na podstawie odległości od wierzchołków.

hwdp_can.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x3, y3, x4, y4)
  d <- length.line(x4, y4, x1, y1)
  if(a == b){
    TRUE
  } else {
    FALSE
  }
  if(a == c){
    TRUE
  } else {
    FALSE
  }
  if(a == d){
    TRUE
  } else {
    FALSE
  }
  if(b == c){
    TRUE
  } else {
    FALSE
  }
  if(b == d){
    TRUE
  } else {
    FALSE
  }
  if(c == d){
    TRUE
  } else {
    FALSE
  }
  
}


# Obwód kwadratu na podstawie iloczynu jednego z jego boków.

hwdp_par.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  if (hwdp_can.sqr(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a square") 
  } else {
    a * 4
  }
}  

# Pole kwadratu na podstawie kwadratu jednego z jego boku. 

hwdp_area.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  if (hwdp_can.sqr(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a square") 
  } else {
    a ^ 2
  }
} 

# Długość przekątnej kwadratu obliczana na podstawie długości jednego
# z jego boków. 

hwdp_dia.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  if (hwdp_can.sqr(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a square") 
  } else {
    a * sqrt(2)
  }
} 

# Rysowanie kwadratu na podstawie współrzędnych jego wierzchołków.

hwdp_plot.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  x <- c(x1, x2, x3, x4)
  y <- c(y1, y2, y3, y4)
  plot(x, y)
  lines(c(x[1], x[2]), c(y[1], y[2]))
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[3], x[4]), c(y[3], y[4]))
  lines(c(x[4], x[1]), c(y[4], y[1]))
}

hwdp_plot.sqr(0,0,5,0,5,5,0,5) 

# Czworobok

# Obliczanie obwodu czworoboku na podstawie odległości wierzchołków.

hwdp_par.pol <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x3, y3, x4, y4)
  d <- length.line(x4, y4, x1, y1)
    a + b + c + d
} 

# Rysowanie czworoboku na podstawie współrzędnych jego wierzchołków.

hwdp_plot.pol <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  x <- c(x1, x2, x3, x4)
  y <- c(y1, y2, y3, y4)
  plot(x, y)
  lines(c(x[1], x[2]), c(y[1], y[2]))
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[3], x[4]), c(y[3], y[4]))
  lines(c(x[4], x[1]), c(y[4], y[1]))
}


# Prostokąt 

# Czy figura jest prostokątem?

hwdp_can.rec <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x3, y3, x4, y4)
  d <- length.line(x4, y4, x1, y1)
  if (a == c){
    TRUE
  } else {
    FALSE
  }
  if (b == d){
    TRUE
  } else {
    FALSE
  }
}

hwdp_area.rec(0,0,4,0,4,5,0,5)

# Obwód prostokąta na podstawie sumy odległości wierzchołków. 

hwdp_par.rec <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  if (hwdp_can.rec(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a rectangle") 
  } else {
    2 * a + 2 * b
  }
} 

# Pole prostokąta na podstawie iloczynu jego boków.

hwdp_area.rec <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  if (hwdp_can.rec(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a rectangle") 
  } else {
    a * b
  }
} 

hwdp_dia.rec <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  if (hwdp_can.rec(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a rectangle") 
  } else {
    sqrt(a ^ 2 * b ^ 2)
  }
} 


hwdp_plot.rec <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  x <- c(x1, x2, x3, x4)
  y <- c(y1, y2, y3, y4)
  plot(x, y)
  lines(c(x[1], x[2]), c(y[1], y[2]))
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[3], x[4]), c(y[3], y[4]))
  lines(c(x[4], x[1]), c(y[4], y[1]))
}
