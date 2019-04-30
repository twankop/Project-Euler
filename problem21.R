#Project Euler problem 21
#Sum all amicable pairs under 10000

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

amicable <-function(n){
  if( sum(divisors(sum(divisors(n))))==n & sum(divisors(n))!=n ){
    sum(divisors(n))
  }
  else{
    0
  }
}
s<-0
for(i in 1:10000){
  s<-s+amicable(i)
}
s





