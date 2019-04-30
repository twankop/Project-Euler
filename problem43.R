#Project Euler problem 43
#What is the sum of all 0 to 9 pandigitals d1 d2 d3 d4 d5 d6 d7 d8 d9 d10, 
#such that: 
#  d2 d3 d4 mod 2=0
#  d3 d4 d5 mod 3=0
#  d4 d5 d6 mod 5=0
#  d5 d6 d7 mod 7=0
#  d6 d7 d8 mod 11=0
#  d7 d8 d9 mod 13=0
#  d8 d9 d10 mod 17=0?

#(This is one ugly piece of code, but it runs in less than the required minute.
#I might want to improve on this one.)

library(sfsmisc)

Base_to_Int<-function(a,b){
  l<-length(a)
  n<-0
  for(i in 1:l){
    n<-n+a[i]*b^(l-i)
  }
  return(n)
}

#First we make a table of all numbers that are divisible by the given primes.
makeDivisibleTable<-function(table){
  divisorList<-c(2,3,5,7,11,13,17)
  for(i in 1:7){
    for(j in 1:500){
      table[j,i]<-divisorList[i]*j
    }
  }
  return(table)
}

#Then we remove all numbers that are too large or contain double digits.
cleanTable<-function(table){
  for(i in 1:7){
    for(j in 1:500){
      if(table[j,i]>999){
        table[j,i]<-0
      }
      else{
        d<-digitsBase(table[j,i],10,3)
        if(length(d)!=length(unique(d))){
          table[j,i]<-0
        }
      }
    }
  }
  return(table)
}

#We now have a number of puzzle pieces that we have to fit together to get a 
#0 to 9 pandigital.
makeListOfValidPandigitals<-function(table){
  possiblePandigitals<-list()
  possiblePandigitals[[1]]<-subset(table[,1],table[,1]!=0)
  for(i in 1:6){
    L<-length(possiblePandigitals[[i]])
    count<-0
    anslist<-{}
    checklist<-{}
    checklist<-subset(table[,i+1],table[,i+1]!=0)
    l<-length(checklist)
    for(j in 1:L){
      d1<-digitsBase(possiblePandigitals[[i]][j],10,2+i)
      for(k in 1:l){
        d2<-digitsBase(checklist[k],10,3)
        if(!is.element(d2[3],d1) && d1[i+1]==d2[1] && d1[i+2]==d2[2]){
          count<-count+1
          anslist[count]<-Base_to_Int(append(d1,d2[3]),10)
        }
      }
    }
    possiblePandigitals[[i+1]]<-anslist
  }
  return(possiblePandigitals[[7]])
}

#now we still have to determine d1
determineD1<-function(pandigitals){
  L<-length(pandigitals)
  for(i in 1:L){
    d<-digitsBase(pandigitals[i],10,9)
    missingdigit<-setdiff(c(0:9),d)
    d<-append(d,missingdigit,0)
    pandigitals[i]<-Base_to_Int(d,10)
  }
  return(pandigitals)
}

# Start the clock!
ptm <- proc.time()

table<-mat.or.vec(500,7)
pandigitals<-{}

table<-makeDivisibleTable(table)
table<-cleanTable(table)
pandigitals<-makeListOfValidPandigitals(table)
pandigitals<-determineD1(pandigitals)

sum(pandigitals)

# Stop the clock
proc.time() - ptm

