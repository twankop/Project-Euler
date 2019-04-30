#Project Euler problem 40

#An irrational decimal fraction is created by concatenating the positive integers:  
#  0.123456789101112131415161718192021...
#If dn represents the nth digit of the fractional part, find the value of the following expression.
#d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

#d[1]=1
#d[10]=1
#d[100]=5
#d[1000]=3
#d[10000]=7
#d[100000]=2
#d[1000000]=1

library(sfsmisc)

CalculateFractionDigit<-function(dt){
  index<-1
  i<-0
  p<-0
  pt<-10^p
  while(index<=dt){
    i<-i+1
    if(i==pt){
      p<-p+1
      pt<-10^p
    }
    index<-index+p
  }
  dig<-digitsBase(i,10)
  return(dig[length(dig)+dt-index+1])
}

# Start the clock!
ptm <- proc.time()

ans<-1
for(d in 0:6){
  dt<-10^d
  ans<-ans*CalculateFractionDigit(dt)
}
ans

# Stop the clock
proc.time() - ptm

  