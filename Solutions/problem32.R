#Project Euler problem 32
#Find the sum of all products whose multiplicand/multiplier/product identity can 
#be written as a 1 through 9 pandigital.


library(sfsmisc)
is.pandigital<-function(a,b,c){
  da<-digitsBase(a, base=10)
  db<-digitsBase(b, base=10)
  dc<-digitsBase(c, base=10)
  n<-union(da,union(db,dc))
  if(length(n)==9 & length(da)+length(db)+length(dc)==9 & !is.element(0,n)){
    TRUE
  }
  else{
    FALSE
  }
}

s<-{}
for(i in 1:100){
  for(j in i:2000){
    if(is.pandigital(i,j,i*j)){
      s<-union(s,i*j)
    }
  }
}
sum(s)
