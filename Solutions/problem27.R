#Project Euler problem 
#

primes<-scan("~/Documents/Huiswerk/Project Euler/primes.txt")

n<-0
s<-0
t<-0
ans1<-0
ans2<-0
for(a in 1:168){
  pa<-primes[a]
  for(b in -1000:1000){
    n<-0
    s<-0
    while( is.element((n^2+b*n+pa),primes)){
      s<-s+1
      n<-n+1
    }#while
    if(s>t){
      t<-s
      ans1<-pa*b
      ans2<-c(b,pa)
    }#if
  }#forb
}#fora
ans1
ans2
t
#####