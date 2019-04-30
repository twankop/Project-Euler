#Project Euler problem 28
#What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

s<-1
for(i in seq(3,1001,2)){
  s<-s+2*(i^2+(i-2)^2+i-1)
}
s
