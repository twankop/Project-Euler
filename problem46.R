#Project Euler problem 46

# What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
# runtime: 5.5 sec

Sieve_of_Eratosthenes <- function(n){
  list <- c(2:n)
  primes <- {}
  h <- 0
  while(list[1] <= sqrt(n) && h < n){
    h <- h+1 
    p <- list[1]
    primes[h] <- p
    m <- seq(p,n,p)
    list <- setdiff(list,m)
  }
  primes <- union(primes,list)
  return(primes)
}

Goldbach_sieve <- function(n){
  list <- seq(from = 3, to = n, by = 2)
  primes <- Sieve_of_Eratosthenes(n)
  squares <- c(1:sqrt(n))^2
  
  list <- setdiff(list, primes)
  
  prime.length <- length(primes)
  square.length <- floor(sqrt(n))
  
  for(i in 1:prime.length){
    for(j in 1:square.length){
      list <- setdiff(list, primes[i]+2*squares[j])
      if(length(list)==0){
        return(list)
      }
    }
  }
  return(list)
}

# Start the clock!
ptm <- proc.time()

ans <- Goldbach_sieve(10000)

cat(ans[1])

# Stop the clock
ptm <- proc.time()-ptm
