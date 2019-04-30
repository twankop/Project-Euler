#Project Euler problem 22
#What is the sum of the 'name scores'?

setwd("~/Documents/Huiswerk/Project Euler")
names <- read.csv("~/Documents/Huiswerk/Project Euler/names.txt", header=F)
l<-length(names)
data<-mat.or.vec(l,1)
for(i in 1:l){
  data[i]<-as.character(names[1,i])
}
data[3303]<-"NA"

letters<-mat.or.vec(l,11)
for(i in 1:l){
  for(j in 1:nchar(data[i])){
    letters[i,j]<-substr(data[i],j,j)
  }
}
cijfers<-mat.or.vec(l,11)
for(i in 1:l){
  for(j in 1:nchar(data[i])){
    if(letters[i,j]=="A"){
      cijfers[i,j]<-1
    }
    if(letters[i,j]=="B"){
      cijfers[i,j]<-2
    }
    if(letters[i,j]=="C"){
      cijfers[i,j]<-3
    }
    if(letters[i,j]=="D"){
      cijfers[i,j]<-4
    }
    if(letters[i,j]=="E"){
      cijfers[i,j]<-5
    }
    if(letters[i,j]=="F"){
      cijfers[i,j]<-6
    }
    if(letters[i,j]=="G"){
      cijfers[i,j]<-7
    }
    if(letters[i,j]=="H"){
      cijfers[i,j]<-8
    }
    if(letters[i,j]=="I"){
      cijfers[i,j]<-9
    }
    if(letters[i,j]=="J"){
      cijfers[i,j]<-10
    }
    if(letters[i,j]=="K"){
      cijfers[i,j]<-11
    }
    if(letters[i,j]=="L"){
      cijfers[i,j]<-12
    }
    if(letters[i,j]=="M"){
      cijfers[i,j]<-13
    }
    if(letters[i,j]=="N"){
      cijfers[i,j]<-14
    }
    if(letters[i,j]=="O"){
      cijfers[i,j]<-15
    }
    if(letters[i,j]=="P"){
      cijfers[i,j]<-16
    }
    if(letters[i,j]=="Q"){
      cijfers[i,j]<-17
    }
    if(letters[i,j]=="R"){
      cijfers[i,j]<-18
    }
    if(letters[i,j]=="S"){
      cijfers[i,j]<-19
    }
    if(letters[i,j]=="T"){
      cijfers[i,j]<-20
    }
    if(letters[i,j]=="U"){
      cijfers[i,j]<-21
    }
    if(letters[i,j]=="V"){
      cijfers[i,j]<-22
    }
    if(letters[i,j]=="W"){
      cijfers[i,j]<-23
    }
    if(letters[i,j]=="X"){
      cijfers[i,j]<-24
    }
    if(letters[i,j]=="Y"){
      cijfers[i,j]<-25
    }
    if(letters[i,j]=="Z"){
      cijfers[i,j]<-26
    }
  }
}
sort<-mat.or.vec(l,1)
for(i in 1:l){
  for(j in 1:11){
    sort[i]<-sort[i]+cijfers[i,j]*27^(11-j)
  }
}

for(i in 1:l){
   temp<-cijfers[i,]
   tempsort<-sort[i]
   min<-max(which(sort==min(sort[i:l])))
   cijfers[i,]<-cijfers[min,]
   sort[i]<-sort[min]
   cijfers[min,]<-temp
   sort[min]<-tempsort
}

namescores<-mat.or.vec(l,1)
for(i in 1:l){
  namescores[i]<-i*sum(cijfers[i,])
}
cijfers[938,]
namescores[938]
sum(namescores)

