#Project Euler problem 18
#Find the maximum total from top to bottom of the triangle below

library(sfsmisc)
setwd("~/Documents/Huiswerk/Project Euler")
triangle1 <- read.csv("~/Documents/Huiswerk/Project Euler/triangle1.txt", header=F)

m<-0
r<-0
for(n in 1:(2^(14))){
  d<-digitsBase(n-1, base=2, 14)
  j<-1
  s<-triangle1[1,1]
  for(i in 1:14){
    if(d[i]==1){
      j<-j+1
    }#if
    s<-s+triangle1[i+1,j]
  }#fori
  if(s>m){
    r<-d
    m<-s
  }
}#forn
m
r

