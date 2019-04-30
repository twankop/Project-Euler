#Project Euler problem 39
#For which value of p ≤ 1000, is the number of integer right triangle solutions maximised?

setwd("~/Documents/Huiswerk/Project Euler")
is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}
ans<-mat.or.vec(1000,1)
for(a in 1:1000){
  for(b in 1:a){
    if(is.wholenumber(sqrt(a^2+b^2))){
      if(a+b+sqrt(a^2+b^2)<=1000){
        ans[a+b+sqrt(a^2+b^2)]<-ans[a+b+sqrt(a^2+b^2)]+1
      }
    }
  }
}
which(ans==max(ans))
ans[which(ans==max(ans))]
plot(ans)
