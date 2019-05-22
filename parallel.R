#współczynnik kierunkowy prostej

a <- function(x1, y1, x2, y2){
(y1 - y2) / (x1 - x2)
}

#warunek równoległości

aP1P2 <- a(1, 1, 2, 2)
aP3P4 <- a(3, 3, 4, 4)

if(aP1P2 == aP3P4){
  TRUE
} else {
  FALSE
}

#to samo ale zbite w jedną funkcję

lines.parallel <- function(x1, y1, x2, y2, x3, y3, x4, y4){
 a <- function(x1, y1, x2, y2){
    (y1 - y2) / (x1 - x2)
  }
 aP1P2 <- a(x1, y1, x2, y2)
 aP3P4 <- a(x3, y3, x4, y4)
 
 if(aP1P2 == aP3P4){
   TRUE
 } else {
   FALSE
 }
}

lines.parallel(1,-1,-1,1,2,-2,-2,2)
  