library(sfsmisc)
#digitsBase(x, base = 2, ndigits = 1 + floor(1e-9+ log(max(x),base)))
#Converts 'x' to an array of length 'ndigits' containing its digits in base 'base'.

#Tests if x is a whole number.
is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

#Free up memory by restarting R session
FreeMemory <- function(){
  .rs.restartR()
}
  

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

#Find all proper divisors of n.
divisors <- function(n){
  m<-1
  for(i in 2:floor(sqrt(n))){
    if(n%%i==0){
      if(i==sqrt(n)){
        m<-c(m,i)
      }
      else{
        m<-c(m,i,n/i)
      }
    }
  }   
  m
}

#Tests if n is a palindromic number in base b
is.palindrome <- function(n,b){
  dign<-digitsBase(n, base=b)
  l<-length(dign)
  pal<-TRUE
  for(i in 1:(ceiling(l/2))){
    if(dign[i]!=dign[l-i+1]){
      pal<-FALSE
    }
  }
  pal
}

#Converts an array a of digits in base b to an integer.
Base_to_Int<-function(a,b){
  l<-length(a)
  n<-0
  for(i in 1:l){
    n<-n+a[i]*b^(l-i)
  }
  n
}

# Start the clock!
ptm <- proc.time()

# Stop the clock
proc.time() - ptm




