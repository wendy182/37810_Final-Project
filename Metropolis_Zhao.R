#goal function is beta(6,4)
#proposal function is beta with param c
#input param c, times of iterations and start value
#output a chain of samples


Metropolis_Hastings_sampler <- function(c,chain_length,start_value){
  chain <- numeric(chain_length)
  accept_ratio <- 0
  for (i in 1:chain_length){
    print(accept_ratio)
    proposed <- rbeta(1,c*start_value,c*(1-start_value))
    print(proposed)
    accept_ratio <- dbeta(proposed,6,4)*dbeta(start_value,c*proposed,c*(1-proposed))/(dbeta(start_value,6,4)*dbeta(proposed,c*start_value,c*(1-start_value)))
    temp <- runif(1,0,1)
    if (temp < accept_ratio){
      chain[i] <- proposed
      start_value <- proposed
    }else{
      chain[i] <- start_value
    }
  }
  return(chain)
}

chain <- Metropolis_Hastings_sampler(1,1000,0.5)
print(chain[900:1000])

