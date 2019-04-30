#Project Euler problem 35
#How many circular primes are there below one million?
#The answer is also found literally on Wikipedia: 55
setwd("~/Documents/Huiswerk/Project Euler")
library(sfsmisc)
primes<-scan("~/Documents/Huiswerk/Project Euler/primes2.txt")
m<-max(which(primes<=1000000))
primes<-primes[1:m]

s<-2
for(p in 1:m){
  circular<-TRUE
  dp<-digitsBase(primes[p], base=10)
  if(max(is.element(c(0,2,4,5,6,8),dp))==0){
    l<-length(dp)
    dr<-mat.or.vec(l,1)
    for(i in 1:(l-1)){
      r<-0
      for(j in 1:l){
        rot<-(i+j)%%l
        if(rot==0){
          rot<-l
        }#if
        dr[j]<-dp[rot]
      }#forj
      for(k in 1:l){
        r<-r+dr[k]*10^(l-k)
      }#fork
      if(!is.element(r,primes)){
        circular<-FALSE
      }#if
    }#fori
    if(circular==TRUE){
      s<-s+1
    }#if
  }#if
}#forp
s


