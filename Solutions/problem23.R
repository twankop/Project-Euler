#Project Euler problem 23
#What is the sum of all numbers that cannot be written as the sum of two abundant numbers.

divisors <- function(n){
  m<-c(1)
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

abundant<-function(n){
  m<-{}
  for(i in 12:n){
    if(sum(divisors(i))>i){
      m<-c(m,i)
    }
  }
  m
}

a<-as.numeric(abundant(28123))
len<-length(a)
s<-{}

for(i in 1:len){
  s<-union(s,a[]+a[i])
}

sum(setdiff(c(1:28123),s))







#IMPORTANT NOTE:
#The double for-loop
#
#for(i in 1:len){
#  for(j in 1:len){
#    s<-union(s,a[j]+a[i])
#  }
#}
#
#should give the same results but is to slow to actually finish the computation

