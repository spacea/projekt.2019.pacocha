circle.circum <- function(x1,y1,x2,y2) { #cirumference
  r <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  if(is.numeric(r) == FALSE){
    stop("Argument is non-numeric")
  }else {print(2*pi*r)
  }
}

circle.circum(1,1,2,2)
circle.circum(123,254,234,764)
