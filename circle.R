
x1 <- 1
y1 <- 2

x2 <- 4
y2 <- 5

hwdp_rad.circ <- function(x1,y1,x2,y2){
    if(is.numeric(x1) == FALSE){
      stop("First argument is non-numeric")
    } else if(is.numeric(y1) == FALSE){
      stop("Second argument is non-numeric")
    } else if(is.numeric(x2) == FALSE){
      stop("Third argument is non-numeric")
    } else if(is.numeric(y2) == FALSE){
      stop("Fourth argument is non-numeric")
    } else {
r <-   sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
    }
}

hwdp_area.circ <- function(r) { #area
  if(is.numeric(r) == FALSE){
    stop("Argument is non-numeric")
 }else {print(pi*r^2)
        }
}
  
hwdp_circum.circ <- function(r) { #cirumference
  if(is.numeric(r) == FALSE){
    stop("Argument is non-numeric")
  }else {print(2*pi*r)
        }
}

