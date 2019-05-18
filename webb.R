
PN <- sample(c(-50:10), 35, replace = TRUE)
SM <- sample(c(-10:50), 35, replace = TRUE)

i <- 1
WB <- c(NA)

webb <- function(PN,SM) {

  while (i <= length(SM)) {
    
    if (PN[i] > 0 && SM[i] > 0) { #B i C
      if (PN[i]>SM[i]){
        WB[i] <-  "B"
    } else if (PN[i]<SM[i]){
        WB[i] <-  "C"
      }
      
  } else if (PN[i] > 0 && SM[i] < 0) { #A i H
      if (PN[i]>SM[i]){
        WB[i] <-  "A"
    } else if (PN[i]<SM[i]){
        WB[i] <-  "H"
      }
    
  } else if (PN[i] < 0 && SM[i] > 0) { #E i D
      if (PN[i]>SM[i]){
        WB[i] <-  "E"
    } else if (PN[i]<SM[i]){
        WB[i] <-  "D"
      }
    
  } else if (PN[i] < 0 && SM[i] < 0) { #F i G
      if (PN[i]>SM[i]){
        WB[i] <-  "G"
    } else if (PN[i]<SM[i]){
        WB[i] <-  "F"
      }
  
   } else {
        print("=")
      }
    
   i <-  i+1
  }#while
}#function
