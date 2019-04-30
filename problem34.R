#Project Euler problem 34
#Find the sum of all factorions.

library(sfsmisc)
t<-0
f<-c(factorial(0),factorial(1),factorial(2),factorial(3),factorial(4),
     factorial(5),factorial(6),factorial(7),factorial(8),factorial(9))
for(i in 10:100000){
  di<-digitsBase(i, base=10)
  l<-length(di)
  s<-0
  for(j in 1:l){
    s<-s+f[di[j]+1]
  }
  if(i==s){
    t<-t+i
  }
}
t
