#Project Euler problem 12
#What is the first triangle number with at least 500 divisors?

#user  system elapsed 
#58.671   0.016  58.927 



#Find all proper divisors of n.
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

triangle<-function(n){
  n*(n+1)/2
}#triangle

# Start the clock!
ptm <- proc.time()

d<-0
n<-0
while(length(d)<500){
  n<-n+1
  d<-divisors(triangle(n))
}
# Stop the clock
proc.time() - ptm
