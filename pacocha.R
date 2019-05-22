
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

# Kąty

angle.cos <- function(x1, y1, x2, y2, x3, y3){
  
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
    ux1x2 <- c((x2 - x1), (y2 - y1))
    ux2x3 <- c((x3 - x2), (y3 - y2))
    ux1x3 <- c((x3 - x1), (y3 - y1))
    
    ((ux1x2[1] * ux1x3[1]) + (ux1x2[2] * ux1x3[2])) / (sqrt((ux1x2[1])^2 + (ux1x2[2]^2)) * sqrt((ux1x3[1])^2 + (ux1x3[2]^2)))
  }
}

# Kwadrat

# Czy figura może być kwadratem? Warunek na podstawie odległości od wierzchołków.

hwdp_can.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x3, y3, x4, y4)
  d <- length.line(x4, y4, x1, y1)
  
  if(angle.cos(x1, y1, x2, y2, x4, y4) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a square") 
  }
  if(angle.cos(x2, y2, x3, y3, x1, y1) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a square") 
  }
  if(angle.cos(x3, y3, x4, y4, x2, y2) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a square") 
  }
  if(angle.cos(x4, y4, x1, y1, x3, y3) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a square") 
  }
  if(a == b){
    TRUE
  } else {
    stop("The geometric figure is not a square") 
  }
  if(a == c){
    TRUE
  } else {
    stop("The geometric figure is not a square") 
  }
  if(a == d){
    TRUE
  } else {
    stop("The geometric figure is not a square") 
  }
  if(b == c){
    TRUE
  } else {
    stop("The geometric figure is not a square") 
  }
  if(b == d){
    TRUE
  } else {
    stop("The geometric figure is not a square") 
  }
  if(c == d){
    TRUE
  } else {
    stop("The geometric figure is not a square") 
  }
  
}

hwdp_can.sqr(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(1),-1/2*sqrt(1),1/2*sqrt(1))
hwdp_can.sqr(0,0,5,0,5,5,0,5)

# Obwód kwadratu na podstawie iloczynu jednego z jego boków.

hwdp_par.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  if (hwdp_can.sqr(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a square") 
  } else {
    a * 4
  }
}  

hwdp_par.sqr(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(1),-1/2*sqrt(1),1/2*sqrt(1))
hwdp_par.sqr(0,0,5,0,5,5,0,5)

# Pole kwadratu na podstawie kwadratu jednego z jego boku. 

hwdp_area.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  if (hwdp_can.sqr(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a square") 
  } else {
    a ^ 2
  }
} 

hwdp_area.sqr(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(1),-1/2*sqrt(1),1/2*sqrt(1))
hwdp_area.sqr(0,0,5,0,5,5,0,5)

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

hwdp_dia.sqr(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(1),-1/2*sqrt(1),1/2*sqrt(1))
hwdp_dia.sqr(0,0,5,0,5,5,0,5)

# Rysowanie kwadratu na podstawie współrzędnych jego wierzchołków.

hwdp_plot.sqr <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  if(hwdp_can.rec(x1, y1, x2, y2, x3, y3, x4, y4) == TRUE){
  x <- c(x1, x2, x3, x4)
  y <- c(y1, y2, y3, y4)
  plot(x, y)
  lines(c(x[1], x[2]), c(y[1], y[2]))
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[3], x[4]), c(y[3], y[4]))
  lines(c(x[4], x[1]), c(y[4], y[1]))
  } else {
    stop("The geometric figure is not a square")
  }
}

hwdp_plot.sqr(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(1),-1/2*sqrt(1),1/2*sqrt(1)) 
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

hwdp_par.pol(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(2),-1/2*sqrt(1),1/2*sqrt(1))
hwdp_par.pol(-2,2,3,3,7,7,0,20)

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

hwdp_plot.pol(0,0,1/2*sqrt(1),1/2*sqrt(1),0,sqrt(2),-1/2*sqrt(1),1/2*sqrt(1))
hwdp_plot.pol(-2,2,3,3,7,7,0,20)

# Prostokąt.

# Czy figura jest prostokątem?

hwdp_can.rec <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x3, y3, x4, y4)
  d <- length.line(x4, y4, x1, y1)
  if(angle.cos(x1, y1, x2, y2, x4, y4) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
  if(angle.cos(x2, y2, x3, y3, x1, y1) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
  if(angle.cos(x3, y3, x4, y4, x2, y2) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
  if(angle.cos(x4, y4, x1, y1, x3, y3) == 0){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
  if (a == c){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
  if (b == d){
    TRUE
  } else {
    stop("The geometric figure is not a rectangle")
  }
}

hwdp_can.rec(0,0,4,0,4,5,0,5)
hwdp_can.rec(-5,-5,5,-5,5,5,-5,5)

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

hwdp_par.rec(0,0,4,0,4,5,0,5)
hwdp_par.rec(-5,-5,5,-5,5,5,-5,5)

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

hwdp_area.rec(0,0,4,0,4,5,0,5)
hwdp_area.rec(-5,-5,5,-5,5,5,-5,5)

hwdp_dia.rec <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  if (hwdp_can.rec(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a rectangle") 
  } else {
    sqrt(a ^ 2 * b ^ 2)
  }
} 

hwdp_dia.rec(0,0,4,0,4,5,0,5)
hwdp_dia.rec(-5,-5,5,-5,5,5,-5,5)

hwdp_plot.rec <- function(x1, y1, x2, y2, x3, y3, x4, y4){
 if(hwdp_can.rec(x1, y1, x2, y2, x3, y3, x4, y4) == TRUE){
  x <- c(x1, x2, x3, x4)
  y <- c(y1, y2, y3, y4)
  plot(x, y)
  lines(c(x[1], x[2]), c(y[1], y[2]))
  lines(c(x[2], x[3]), c(y[2], y[3]))
  lines(c(x[3], x[4]), c(y[3], y[4]))
  lines(c(x[4], x[1]), c(y[4], y[1]))
 } else {
   stop("The geometric figure is not a rectangle")
 }
}

hwdp_plot.rec(0,0,4,0,4,5,0,5)
hwdp_plot.rec(-5,-5,5,-5,5,5,-5,5)


# ROMB

hwdp_can.rho <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  b <- length.line(x2, y2, x3, y3)
  c <- length.line(x3, y3, x4, y4)
  d <- length.line(x4, y4, x1, y1)
  
  if(angle.cos(x1, y1, x2, y2, x4, y4) == 1 / 2){
    TRUE
  } else {
    stop("The geometric figure is not a rhombus")
  }
  if(angle.cos(x2, y2, x3, y3, x1, y1) == -1 / 2){
    TRUE
  } else {
    stop("The geometric figure is not a rhombus")
  }
  if(angle.cos(x3, y3, x4, y4, x2, y2) == 1 / 2){
    TRUE
  } else {
    stop("The geometric figure is not a rhombus")
  }
  if(angle.cos(x4, y4, x1, y1, x3, y3) == -1 / 2){
    TRUE
  } else {
    stop("The geometric figure is not a rhombus")
  }
  if(a == b){
    TRUE
  } else {
    stop("The geometric figure is not a rhombus")
  }
  if(a == c){
    TRUE
  } else {
    stop("The geometric figure is not a rhombus")
  }
  if(a == d){
    TRUE
  } else {
    stop("The geometric figure is not a rhombus")
  }
  if(b == c){
    TRUE
  } else {
    stop("The geometric figure is not a rhombus")
  }
  if(b == d){
    TRUE
  } else {
    stop("The geometric figure is not a rhombus")
  }
  if(c == d){
    TRUE
  } else {
    stop("The geometric figure is not a rhombus")
  }
  
}

hwdp_can.rho(6,1,1,3,-4,1,1,-1)
hwdp_can.rho(4,0,0,1,-4,0,0,-1)
hwdp_plot.pol(5,1,1,4,-3,1,1,-2)

# Obwód rombu

hwdp_par.rho <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  if (hwdp_can.rho(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a rhombus") 
  } else {
    a * 4
  }
}

hwdp_par.rho(6,1,1,3,-4,1,1,-1)
hwdp_par.rho(4,0,0,1,-4,0,0,-1)

# Pole rombu

hwdp_area.rho <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  a <- length.line(x1, y1, x2, y2)
  if (hwdp_can.sqr(x1, y1, x2, y2, x3, y3, x4, y4) == FALSE){
    stop("The geometric figure is not a rhombus") 
  } else {
   abs(a ^ 2 * angle.cos(x2, y2, x3, y3, x1, y1))
  }
}

hwdp_area.rho(6,1,1,3,-4,1,1,-1)
hwdp_area.rho(4,0,0,1,-4,0,0,-1)

# Plot rombu

hwdp_plot.rho <- function(x1, y1, x2, y2, x3, y3, x4, y4){
  if(hwdp_can.rho(x1, y1, x2, y2, x3, y3, x4, y4) == TRUE){
    x <- c(x1, x2, x3, x4)
    y <- c(y1, y2, y3, y4)
    plot(x, y)
    lines(c(x[1], x[2]), c(y[1], y[2]))
    lines(c(x[2], x[3]), c(y[2], y[3]))
    lines(c(x[3], x[4]), c(y[3], y[4]))
    lines(c(x[4], x[1]), c(y[4], y[1]))
  } else {
    stop("The geometric figure is not a rhombus")
  }
}

hwdp_plot.rho(6,1,1,3,-4,1,1,-1)
hwdp_plot.rho(4,0,0,1,-4,0,0,-1)

