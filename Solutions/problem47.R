#Project Euler problem 47

#Find the first four consecutive integers to have four distinct prime factors. 
#What is the first of these numbers?

#runtime: 1271 sec

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

primeDivisors <- function(n, primes = primes){
  m <- {}
  L <- length(subset(primes, primes<=n/2))
  for(i in 1:L){
    if((n%%primes[i])==0){
      m<-union(m,primes[i])
    }
  }   
  return(m)
}

# Start the clock!
ptm <- proc.time()


primes<-Sieve_of_Eratosthenes(1000000)
M<-primes[length(primes)]

is.done<-FALSE
i<-30
while(!is.done && i < M){
  i<-i+1
  if(length(primeDivisors(i, primes))>=4 &&
     length(primeDivisors(i+1, primes))>=4 &&
     length(primeDivisors(i+2, primes))>=4 &&
     length(primeDivisors(i+3, primes))>=4){
    is.done <- TRUE
  }
}

cat(i)

primeDivisors(i,primes)
primeDivisors(i+1,primes)
primeDivisors(i+2,primes)
primeDivisors(i+3,primes)

# Stop the clock
ptm <- proc.time()-ptm


