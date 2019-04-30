#Project Euler problem 67
#Find the maximum total from top to bottom of the large triangle below


library(sfsmisc)
setwd("~/Documents/Huiswerk/Project Euler")
triangle2<-scan("~/Documents/Huiswerk/Project Euler/triangle2.txt", fill=TRUE)
l<-length(triangle2)
triangle<-mat.or.vec(100,100)
n<-1
m<-1
for(i in 1:l){
  triangle[n,m]<-triangle2[i]
  m<-m+1
  if(m>n){
    m<-1
    n<-n+1
  }
}

s<-100
trianglenew<-triangle
for(i in 1:(s-1)){
  for(j in 1:(s-1)){
    if(trianglenew[s-i,j]!=0){
      trianglenew[s-i,j]<-triangle[s-i,j]+max(trianglenew[s+1-i,j],trianglenew[s+1-i,j+1])
    }
  }
}
trianglenew[1,1]

