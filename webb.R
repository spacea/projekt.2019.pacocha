
PN <- sample(c(-50:10), 35, replace = TRUE)
SM <- sample(c(-10:50), 35, replace = TRUE)

i <- 1
WB <- c(NA)

webb <- function(PN,SM) {

  while (i <= length(SM)) {
    if (PN > 0 && SM > 0) { #B i C
      
      if (PN[i]>SM[i]){
        WB[i] <-  "B"
      }
      
      if (PN[i]<SM[i]){
        WB[i] <-  "C"
      }
    }
    
    else if (PN > 0 && SM < 0) { #A i H
      
      if (PN[i]>SM[i]){
        WB[i] <-  "A"
      }
      
      if (PN[i]<SM[i]){
        WB[i] <-  "H"
      }
    }
    
    else if (PN < 0 && SM > 0) { #E i D
      
      if (PN[i]>SM[i]){
        WB[i] <-  "E"
      }
      
      if (PN[i]<SM[i]){
        WB[i] <-  "D"
      }
    }
    
    else if (PN < 0 && SM < 0) { #F i G
      
      if (PN[i]>SM[i]){
        WB[i] <-  "G"
      }
      
      if (PN[i]<SM[i]){
        WB[i] <-  "F"
      }
    }
    else {
      print("równe")
    }
    
   i <-  i+1
  }#while
}#function
