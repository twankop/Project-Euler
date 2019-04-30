#Project Euler problem 49

#The arithmetic sequence, 1487, 4817, 8147 is unusual in two ways: 
#each of the three terms are prime
#each of the 4-digit numbers are permutations of one another.
#What 12-digit number do you form by concatenating the three 
#terms in the other sequence with these two properties?

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

primes<-Sieve_of_Eratosthenes(10000)
L<-length(primes)

p<-{}
ans<-{}
for(i in 1:(L-2)){
  p[1]<-primes[i]
  if(p[1]>=1000){
    for(j in (i+1):(L-1)){
      p[2]<-primes[j]
      p[3]<-2*p[2]-p[1]
      if(p[3]<10000 && is.element(p[3],primes)){
        d1<-digitsBase(p[1],10)
        d2<-digitsBase(p[2],10)
        d3<-digitsBase(p[3],10)
        if(setequal(d1,d2) && setequal(d2,d3)){
          ans<-p
        }
      }
    }
  }
}
ans

# Stop the clock
proc.time() - ptm

