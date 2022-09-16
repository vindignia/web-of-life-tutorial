count_nonzero <- function(M){
  MM <- as.matrix((M>0))  
  class(MM) <- "numeric" 
  return(sum(MM))
}

equifrequent_model <- function(M_in){
  
  rows <- nrow(M_in)
  columns <- ncol(M_in)
  number_ones <- count_nonzero(M_in)
  count_ones <- 0
  
  B <- matrix(0, rows, columns)
  
  while (count_ones < number_ones) {
    
    x <- ceiling(runif(1, min = 0, max = rows))
    y <- ceiling(runif(1, min = 0, max = columns))
    
    while (B[x,y] == 1) {
      
      x <- ceiling(runif(1, min = 0, max = rows))
      y <- ceiling(runif(1, min = 0, max = columns))
      
    }
    
    B[x,y] <- 1
    count_ones <- count_ones + 1;
  }
  
  # Remove unconected species if present
  # B = trim_network(B)
  
  return(B)
}
