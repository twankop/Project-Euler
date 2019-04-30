#Project Euler problem 100
#By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total, 
#determine the number of blue discs that the box would contain.

#We start with a bit of mathematics and some help from WolframAlpha.
#Let n be the total number of discs and b the number of blue discs.
#Then we have that n^2-n=2*(b^2-b) and thus that n=1/2*(1+sqrt(1-8*b+8*b^2)).
#For n>10^12 this gives b>=707106781187.
#We now want to find the smallest b>=707106781187 such that 1/2*(1+sqrt(1-8*b+8*b^2)) is a whole number.

is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

b<-c(707106781185+1:10000000)
ans<-is.wholenumber(1/2*(1+sqrt(1-8*b+8*b^2)))
a<-b[which(ans==TRUE)]
1/2*(1+sqrt(1-8*a+8*a^2))>10^12
ans2<-floor(1/2*(1+sqrt(1-8*b+8*b^2)))==1/2*(1+sqrt(1-8*b+8*b^2))


which(ans2==TRUE)[1:10]
a[1:10]
n<-1/2*(1+sqrt(1-8*a+8*a^2))
n^2-n
n
2*(a^2-a)




# n<-10^12
# r<-ceiling(1/sqrt(2)*10^12)
# b<-n-r
# 
# (r*(r-1))/(n*(n-1))


