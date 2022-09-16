# Code for the swap null model

swap_model_F = function(d,iter_max){

  # Make sure we are working with a matrix
  d = as.matrix(d)

  rows <- nrow(d)

  columns <- ncol(d)

  iter <- 0;

  while (iter < iter_max) {

    i <- ceiling(runif(1, min = 0, max = rows))
    j <- ceiling(runif(1, min = 0, max = columns))

    while (d[i,j] == 0) {
      # for the first pair of numbers
      i <- ceiling(runif(1, min = 0, max = rows))
      j <- ceiling(runif(1, min = 0, max = columns))

    }
    # for the second pair of numbers
    q <- ceiling(runif(1, min = 0, max = rows))
    z <- ceiling(runif(1, min = 0, max = columns))

    # different row and column # A: what is this doing? 
    while ((q == i) || (q == j)){ # shouldn't it be z == j? 

      q <- ceiling(runif(1, min = 0, max = rows))

    }

    # different row and column
    while ((z == i) || (z ==j)){

      z <- ceiling(runif(1, min = 0, max = columns))

    }

    while (d[q,z] == 0) {

      q <- ceiling(runif(1, min = 0, max = rows))

      while ((q == 1) || (q == j)) {

        q <- ceiling(runif(1, min = 0, max = rows))

      }

      z <- ceiling(runif(1, min = 0, max = columns))

      while ((z == i) || (z ==j)){
        z <- ceiling(runif(1, min = 0, max = columns))
      }
    }



    if (d[i,z] == 0 && d[q,j] == 0) {
      d[i,j] <- 0;
      d[q,z] <- 0;
      d[i,z] <- 1;
      d[q,j] <- 1;


      d = trim_network(d)

      iter <- iter + 1;
    }

  }
  return(d)
}
