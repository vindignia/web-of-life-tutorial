#Network properties

nw_metrics <- function(A){

# species  
n_sp <- ncol(A)

# Leading eigenvalues
A_eigen<-eigen(A)
A_eigenvalues<-A_eigen$values
tmp <- sort(A_eigenvalues, decreasing = TRUE) %>% head(.,2)
# largesteigenvalue
leading_eigen <- tmp[1]
# Second largest eigenvalue
second_leigen <- tmp[2]

# Spectral gap
spectral_gap<-abs(second_leigen-leading_eigen)
norm_spectralgap<-abs(spectral_gap/leading_eigen) # Rescaled relative to largest eigenvalue because it grows with the absolute values

degrees<-rowSums(A) #Calculating adjacency matrix degrees
degree_matrix<-diag(degrees) #Creating degree matrix
laplacian<-degree_matrix-A #Obtaining the Laplacian matrix

lap_eigen<-eigen(laplacian) #Laplacian eigenvalues and eigenvectors
lap_eigenvalues<-lap_eigen$values #Laplacian eigenval

lap_eigenvalues[which(lap_eigenvalues<1e-10)]<-0 #Setting up as zero any value smaller than 1e-10 (problems with the numerical approximation of eigenvalues)
components<-sum(lap_eigenvalues == 0) #Counting number of laplacian eigenvalues equals to zero, i.e. number of components
lap_firsteigen<-max(lap_eigenvalues)
lap_seceigen<-min(lap_eigenvalues[lap_eigenvalues > 0])
eigenratio<-lap_seceigen/lap_firsteigen # Related to diffusion time

# Average degree, difference between the maximum and minimum degrees, variance in the degrees of species
graph<-graph_from_adjacency_matrix(A, mode=c("undirected"))
average_degree<-mean(igraph::degree(graph))
dif_degree=max(igraph::degree(graph))-min(igraph::degree(graph))
var_degree=var(igraph::degree(graph))
max_degree=max(igraph::degree(graph))

# Average shortest path length
dist<-distances(graph)
invdist<-1/dist
diag(invdist)<-0
avg_spt<-sum(invdist)/(n_sp*(n_sp-1))

# Density of pathways
decay<-1/(leading_eigen+0.1)
A<-A*decay
I<-diag(1, nrow(A), ncol(A))
katz<-solve(I-A)
path_density<-mean(katz)

return(data.frame(path_density, second_leigen, eigenratio, components, lap_firsteigen, lap_seceigen, spectral_gap, norm_spectralgap, leading_eigen, avg_spt, max_degree, average_degree, dif_degree, var_degree, n_sp))

}
