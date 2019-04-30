#Project Euler problem 473
#Find the sum of the positive integers not exceeding 10^10 whose phigital representation is palindromic.

library(sfsmisc)

is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

Fibonacci<-function(n){
  f<-0
  for(k in 0:(floor((n-1)/2))){
    f<-f+choose(n-k-1,k)
  }
  f
}

non_consecutive_list<-function(n){
  list1<-mat.or.vec(Fibonacci(1+2),1)
  list1[2]<-1
  list2<-mat.or.vec(Fibonacci(2+2),2)
  list2[1:2,1]<-list1
  list2[3,2]<-1
  for(i in 3:n){
    f1<-Fibonacci(i)
    f2<-Fibonacci(i+1)
    f3<-f1+f2
    list3<-mat.or.vec(f3,i)
    list3[1:f2, 1:(i-1)]<-list2
    list3[f2+1:f1, 1:(i-2)]<-list1
    for(j in 1:f1+f2){
      list3[j,(i-1):i]<-c(0,1)
    }
    list1<-list2
    list2<-list3
  }
  list2
}

rec_ncl<-function(n){
  list<-{}
  if(n==1){
    list<-mat.or.vec(2,1)
    list[2]<-1
  }
  else{
    if(n==2){
      list<-mat.or.vec(3,2)
      list[1:2,1]<-rec_ncl(n-1)
      list[3,2]<-1
    }
    else{
      f1<-Fibonacci(n)
      f2<-Fibonacci(n+1)
      f3<-f1+f2
      list<-mat.or.vec(f3,n)
      list[1:f2, 1:(n-1)]<-rec_ncl(n-1)
      list[f2+1:f1, 1:(n-2)]<-rec_ncl(n-2)
      list[f2+1:f1,n]<-1
    }
  }
  list
}

#Converts an array a of digits in base b to an integer.
Base_to_Int<-function(a,b){
  l<-length(a)
  n<-0
  for(i in 1:l){
    n<-n+a[i]*b^(l-i)
  }
  n
}

A072649<-function(n){
  -1+floor(log(((n+0.2)*sqrt(5)))/log((1+sqrt(5))/2))
}

fibinary<-function(n){
  if(n<3){
    n
  }
  else{
    (2^(A072649(n)-1))+fibinary(n-Fibonacci(1+A072649(n)))
  }
}


# time1<-Sys.time()
# ans<-1
# l<-14
# f<-Fibonacci(l+2)
# phi<-(1+sqrt(5))/2
# phipower<-phi^c(1:l)+phi^(-c(1:l)-1)
# pal<-{}
# c<-0
# for(i in 1:f){
#   di<-digitsBase(fibinary(i), base=2, ndigits=l)
#   s<-sum(phipower[which(di==1)])
#   if(s<=10^3 && is.wholenumber(s)){
#     c<-c+1
#     pal[c]<-s
#   }
# }
# time2<-Sys.time()
# difftime(time2,time1)
# ans
# sum(pal)
# sort(pal)

time1<-Sys.time()
ncl1<-rec_ncl(20)
time2<-Sys.time()
ncl2<-non_consecutive_list(20)
time3<-Sys.time()

min(ncl1==ncl2)
difftime(time2,time1)
difftime(time3,time2)
