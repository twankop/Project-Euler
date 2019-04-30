#Project Euler problem 37
#Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
#This code only works on the assumption that all eleven truncatable primes are listen in my primes list.
setwd("~/Documents/Huiswerk/Project Euler")
library(sfsmisc)
primes<-scan("primes2.txt")

Base_to_Int<-function(a,b){
  l<-length(a)
  n<-0
  for(i in 1:l){
    n<-n+a[i]*b^(l-i)
  }
  n
}

len<-length(primes)
i<-0
c<-0
t<-{}
while(i<=len && c<11){
  i<-i+1
  truncable<-TRUE
  dp<-digitsBase(primes[i], base=10)
  l<-length(dp)
  for(j in 1:(l-1)){
    dpt<-dp[1:j]
    p<-Base_to_Int(dpt,10)
    if(!is.element(p,primes)){
      truncable<-FALSE
    }
  }
  if(truncable){
    for(j in 2:l){
      dpt<-dp[j:l]
      p<-Base_to_Int(dpt,10)
      if(!is.element(p,primes)){
        truncable<-FALSE
      }
    }
  }
  if(truncable){
    c<-c+1
    t[c]<-primes[i]
  }
}

t
sum(t)