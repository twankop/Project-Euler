#Project Euler problem 41
#What is the largest n-digit pandigital prime that exists?

#Note that all 1 to 9 pandigitals are divisable by 3, since sum(1:9)=45
#Note that all 1 to 8 pandigitals are divisable by 3, since sum(1:9)=36

#Find all prime numbers no larger than n.
Sieve_of_Eratosthenes<-function(n){
  list<-c(2:n)
  primes<-{}
  h<-0
  while(list[1]<=sqrt(n) && h<n){
    h<-h+1 
    p<-list[1]
    primes[h]<-p
    m<-seq(p,n,p)
    list<-setdiff(list,m)
  }
  primes<-union(primes,list)
  primes
}

# Start the clock!
ptm <- proc.time()


primes<-Sieve_of_Eratosthenes(10^7)
# Stop the clock
proc.time() - ptm
