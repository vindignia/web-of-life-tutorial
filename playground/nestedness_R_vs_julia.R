library(devtools)
library(rjson)
# library(dplyr)
# library(formattable)
library(rbenchmark)
library("JuliaCall")
julia <- julia_setup()

setwd("/home/alessandro/web-of-life-tutorial/playground") 
julia_source("nestedness.jl")

# LOAD EXTERNAL PACKAGES
# PACKAGE FROM bascompte-lab github 
remove.packages("weboflife")
devtools::install_github("bascompte-lab/weboflife", force=TRUE)
library(weboflife)

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)


n <- 347 # number of rows
m <- 423 # number of columns 
M <- matrix(0,n,m)

for(i in 1:n){
  for (j in 1:m){
    #M[i,j] <- floor(runif(1, min = 0, max = 1.2)) # binary sparse
    M[i,j] <- runif(1, min = 0, max = 1.2)
    M[i,j] <- if(M[i,j]>0.6) M[i,j] else 0.
  }
}
print("matrix dimensions")
dim(M)

# julia_source("nestedness.jl")
# julia_call("nestedness",M)



  rbenchmark::benchmark("R_nestedness" =  {weboflife::nestedness(M)},
                              "julia_nestedness" = {julia_call("nestedness",M)}, 
                              "julia_nestedness_ev" = {julia_eval("nestedness(M)")}, 
                              replications = 3,
                              columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"))


# this code computes the nestedness of a given incident matrix M 
# according to the definition given in 
# Fortuna, M.A., et al.: Coevolutionary dynamics shape the structure of bacteria‐phage infection networks. Evolution 1001-1011 (2019). 
# DOI 10.1111/evo.13731

    # Binarize the matrix
    B = M.>0.5
    B = convert(Matrix{UInt8}, B)

    # nestedness of rows
    nested_rows = 0
    for i=1:(size(B,1)-1) 
       j = i + 1
       while  j <= size(B,1)
       
                shared=sum(B[i,:].*B[j,:]) # sum of common interactions
                k_i = sum(B[i,:])
                k_j = sum(B[j,:])

                # Handle disconnected nodes 
                if !(k_i == 0 || k_j==0) 
                  min_shared = min(k_i,k_j) # min of the degrees
                  nested_rows = nested_rows + (shared/min_shared)
                end
                
            j = j + 1 # while iterator
        end
    end

    # nestedness of columns
    nested_columns = 0
    for i=1:(size(B,2) - 1) 
       j = i + 1 
       while  j <= size(B,2)
                shared=sum(B[:,i].*B[:,j]) # sum of common interactions
                k_i = sum(B[:,i])
                k_j = sum(B[:,j])

                # Handle disconnected nodes 
                if !(k_i == 0 || k_j==0) 
                  min_shared = min(k_i,k_j) # min of the degrees
                  nested_columns = nested_columns+(shared/min_shared)
                end   
                
            j = j + 1 # while iterator 
        end 
    end
    
    # nestedness of the network
    nestedness_network = (nested_rows+nested_columns)/((size(B,1)*(size(B,1)-1)/2)+(size(B,2)*(size(B,2)-1)/2))
    return nestedness_network
end")

julia_eval("nestedness(M)")
