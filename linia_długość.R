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