#czy to jest rownoleglobok
is.parallelogram <- function(x1,y1,x2,y2,x3,y3,x4,y4) {
  a1 <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  a2 <- sqrt(((x4 - x3)^2) + ((y4 - y3)^2))
  b1 <- sqrt(((x3 - x1)^2) + ((y3 - y1)^2))
  b2 <- sqrt(((x4 - x2)^2) + ((y4 - y2)^2))
  if(a1==a2 && b1==b2) {
    print("This is a parallelogram.")
    is.para <- TRUE
  }else if(a1==a2 && b1==b2 && a1==b1 && a1==b2 && a2==b1 && a2==b2){
    print("Your parallelogram is also a square.")
    is.para <- TRUE
  }else if (a1==a2 || b1==b2){
    print("This is not a parallelogram, it only has 2 parallel sides.")
    is.para <- FALSE
  }else {
    print("This is not a parallelogram at all.")
    is.para <- FALSE
  }
}
is.parallelogram(1,1,2,2,3,3,4,4)
is.parallelogram(1,1,2,2,3,3,4,6)