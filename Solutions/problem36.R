#Project Euler problem 26
#What is the sum of all numbers less than one million, which are palindromic in base 10 and base 2?
library(sfsmisc)
setwd("~/Documents/Huiswerk/Project Euler")

is.palindrome <- function(n,b){
  dign<-digitsBase(n, base=b)
  l<-length(dign)
  pal<-TRUE
  for(i in 1:(ceiling(l/2))){
    if(dign[i]!=dign[l-i+1]){
      pal<-FALSE
    }
  }
  pal
}


s<-0
for(i in 1:1000000){
  if(is.palindrome(i,2) && is.palindrome(i,10)){
    s<-s+i
  }
}
s



