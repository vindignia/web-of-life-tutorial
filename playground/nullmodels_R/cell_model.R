cell_model <- function(M_in){
  
  rows <- nrow(M_in)
  columns <- ncol(M_in)
  
  PR <- matrix(0, rows, 1)
  PC <- matrix(0, columns, 1)
  B <- matrix(0, rows, columns)
  
  for (i in 1:rows){
    number_ones <- 0
    for (j in 1:columns){
      if(M_in[i,j] == 1){
        number_ones <- number_ones + 1
      }
    }
    PR[i] <- number_ones/columns
  }
  
  for (j in 1:columns){
    number_ones <- 0
    for (i in 1:rows){
      if(M_in[i,j] == 1){
        number_ones <- number_ones + 1
      }
    }
    PC[j] <- number_ones/rows
  }
  
  for (i in 1:rows){
    for (j in 1:columns){
      p <- (PR[i]+PC[j])/2;
      r <- runif(1)
      if( r < p ){
        B[i,j] <- 1;
      }
    }
    
  }
  
  return(B)
}