#Project Euler problem 30
#Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

library(sfsmisc)
s<-0
for(i in 2:354294){
  n<-digitsBase(i, base=10, 6)
  if((n[1]^5+n[2]^5+n[3]^5+n[4]^5+n[5]^5+n[6]^5)==i){
    s<-s+i
  }
}
s

#Refined version 
# s<-0
# for(i in 2:354294){
#   if(sum(digitsBase(i, base=10, 6)^5)==i){
#     s<-s+i
#   }
# }
# s
