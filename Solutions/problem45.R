#Project Euler problem 45
#Find the next triangle number that is also pentagonal and hexagonal.

t<-(c(1:1000000)*(c(1:1000000)+1))/2
p<-(c(1:1000000)*(3*c(1:1000000)-1))/2
h<-c(1:1000000)*(2*c(1:1000000)-1)

ans<-intersect(intersect(t,p),h)
ans[1:3]
