#Project Euler problem 26
#For what number d<1000 does the fraction 1/d contain the longest recurring 
#cycle in its decimal fraction part?

calculatCycleLength<-function(n,m){
  r<-{}
  ans<-{}
  i<-1
  index<-0
  isDone<-FALSE
  isCycle<-TRUE
  
  num<-n
  
  cycleLength<-0
  
  while(i<=m+1 && !isDone){
    r[i]<-num-floor(num/m)*m
    if(r[i]==0){
      isDone<-TRUE
      isCycle<-FALSE
    }
    else{if(i>1){
      for(j in 1:(i-1)){
        if(r[i]==r[j]){
          isDone<-TRUE
          index<-j
        }
      }
    }}
    ans[i]<-floor(num/m)
    num<-r[i]*10
    i<-i+1
  }
  if(isCycle){
    cycleLength<-i-1-index
  }
  #for long division return ans
  return(cycleLength)
}

cL<-{}
for(i in 1:999){
  cL[i]<-calculatCycleLength(1,i)
}

which(cL==max(cL))




