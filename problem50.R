#Project Euler problem 50
#Which prime, below one-million, can be written as the sum of the most consecutive primes?

setwd("~/Documents/Huiswerk/Project Euler")
primes<-scan("~/Documents/Huiswerk/Project Euler/primes2.txt")
m<-max(which(primes<=1000000))
primes<-primes[1:m]

#First determine the maximum number of primes that can be summed which sum is less than one-million
large<-TRUE
maximum<-m+1
while(large){
  maximum<-maximum-1
  large<-(sum(primes[1:maximum])>1000000)
}
found<-FALSE
while(!found && maximum>21){
  maximum<-maximum-1
  for(i in 1:(m-maximum)){
    s<-sum(primes[i:(i+maximum)])
    if(is.element(s,primes)){
      found<-TRUE
      ans<-s
    }
  }
}
ans
sum(primes[4:(4+maximum)])

