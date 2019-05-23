circle.area <- function(x1,y1,x2,y2) { #area
  r <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  if(is.numeric(r) == FALSE){
    stop("Argument is non-numeric")
  }else {print(pi*r^2)
  }
}
circle.area(1,1,2,2)
circle.area(345,324,577,234)

