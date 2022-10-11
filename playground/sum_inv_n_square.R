
print("Series 1/n^2")

my_inv_square <- function(x){
  tmp <- 1./as.double(x*x)
  res <- if(is.na(tmp)) 0  else tmp 
  return(res)
}


my_sum_inv_square <- function(N){
  res <- Reduce(function (x, y) x +  my_inv_square(y), x = 1:N, accumulate = FALSE)
  return(res)
}

warnings()  

print("theo")
theo <- pi*pi/6.
theo

my_res <- my_sum_inv_square(200000) 
print("my res")
my_res
print(paste0("Delta R =", my_res - theo))