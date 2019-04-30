#Project Euler problem 38
#What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product 
#of an integer with (1,2, ... , n) where n > 1?

library(sfsmisc)

is.pandigital<-function(a){
  if(length(a)==9 && max(a)==9 && min(a)==1 && setdiff(c(1:10),a)==10){
    TRUE
  }
  else{
    FALSE
  }
}

concatenated_product<-function(a,b){
  l<-length(b)
  da<-{}
  for(i in 1:l){
    di<-digitsBase(a*b[i], base=10)
    ldi<-length(di)
    lda<-length(da)
    da[(lda+1):(lda+ldi)]<-di
  }
  da
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

largest<-918273645
for(i in 2:5){
  for(j in 1:9999){
    c<-concatenated_product(j,c(1:i))
    if(is.pandigital(c)){
      l<-Base_to_Int(c,10)
      if(l>largest){
        largest<-l
      }
    }
  }
}
largest




