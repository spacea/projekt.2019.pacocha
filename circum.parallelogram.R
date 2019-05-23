#obwod
circum.parallelogram <- function(x1,y1,x2,y2,x3,y3,x4,y4) {
  a1 <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  a2 <- sqrt(((x4 - x3)^2) + ((y4 - y3)^2))
  b1 <- sqrt(((x3 - x1)^2) + ((y3 - y1)^2))
  b2 <- sqrt(((x4 - x2)^2) + ((y4 - y2)^2))
  is.para <- a1==a2 && b1==b2
  if(is.para == TRUE){
    circum.para <- a1+a2+b1+b2
    cat(circum.para)
  }else {
    print("This is not a parallelogram.")
  } 
  
}

circum.parallelogram(1,1,2,2,3,3,4,4)
circum.parallelogram(1,1,2,2,3,3,4,6)