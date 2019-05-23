#boki rownoleglobnoku
sides.parallelogram <- function(x1,y1,x2,y2,x3,y3,x4,y4){
  if(is.numeric(x1) == FALSE){
    stop("First argument is non-numeric.")
  } else if(is.numeric(y1) == FALSE){
    stop("Second argument is non-numeric.")
  } else if(is.numeric(x2) == FALSE){
    stop("Third argument is non-numeric.")
  } else if(is.numeric(y2) == FALSE){
    stop("Fourth argument is non-numeric.")
  } else if(is.numeric(x3) == FALSE){
    stop("Fifth argument is non-numeric.")
  } else if(is.numeric(y3) == FALSE){
    stop("Sixth argument is non-numeric.")
  } else if(is.numeric(x4) == FALSE){
    stop("Seventh argument is non-numeric.")
  } else if(is.numeric(y4) == FALSE){
    stop("Eighth argument is non-numeric.")
  } else {
    a1 <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
    a2 <- sqrt(((x4 - x3)^2) + ((y4 - y3)^2))
    b1 <- sqrt(((x3 - x1)^2) + ((y3 - y1)^2))
    b2 <- sqrt(((x4 - x2)^2) + ((y4 - y2)^2))
    print(a1)
    print(a2)
    print(b1)
    print(b2)
  }
}
sides.parallelogram(2857,3345,763,6645,6457,4767,2857,3345)
sides.parallelogram(287,345,783,645,675,467,878,583)
sides.parallelogram(287,345,783,"aufygaougfh",675,467,878,583)