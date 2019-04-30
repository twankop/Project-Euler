#Project Euler problem 33
#Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

library(sfsmisc)
t<-1
for(n in 10:98){
  for(d in n+1:99){
    dn<-digitsBase(n, base=10)
    dd<-digitsBase(d, base=10)
    if(dn[2]==dd[1] && n/d==dn[1]/dd[2]){
      t<-t*n/d
    }
  }#ford
}#forn
t
