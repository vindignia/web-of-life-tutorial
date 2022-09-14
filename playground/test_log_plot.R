library(ggplot2)
library(latex2exp)

alpha <-function(p){
  return(log(p, 2))  
}

sigma_exp <-function(p){
  return((2. + alpha(p))/(1. + alpha(p)))  
}

sigma_exp(0.7)

ggplot() + 
  stat_function(fun = function(x) sigma_exp(x), color ="blue", linetype = "solid", size = 0.5) +
  theme_minimal() + 
  theme(aspect.ratio = 1,legend.position="none")  +   
  xlim(c(0.51,0.9))  
  ylab("exponent") + 
  xlab(TeX("$p$"))
