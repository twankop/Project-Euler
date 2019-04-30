#Project Euler problem 84
#Monopoly odds
#If, instead of using two 6-sided dice, two 4-sided dice are used, find the six-digit modal string.
#00=GO
#02=CC
#08=CH
#10=JAIL
#17=CC
#22=CH
#30=G2J
#33=CC
#36=CH
dice<-4
games<-100
throws<-10000

chance<-function(place){
  ch<-floor(runif(1,1,17))
  switch(as.character(ch),
         "1"=place<-0,
         "2"=place<-10,
         "3"=place<-11,
         "4"=place<-24,
         "5"=place<-39,
         "6"=place<-5,
         "7"=place<-floor(((place+5)%%40)/10)*10+5,
         "8"=place<-floor(((place+5)%%40)/10)*10+5,
         "9"=place<-(place-max((place-28)%%40,(place-11)%%40))%%40,
         "10"=place<-place-3)
  place
}

community_chest<-function(place){
  cc<-floor(runif(1,1,17))
  switch(as.character(cc),
         "1"=place<-0,
         "2"=place<-10)
  place
}

time1<-Sys.time()
freq<-mat.or.vec(40,1)
for(g in 1:games){
  place<-0
  freq[1]<-freq[1]+1
  doubles<-0
  for(t in 1:throws){
    d1<-floor(runif(1,1,dice+1))
    d2<-floor(runif(1,1,dice+1))
    if(d1==d2){
      doubles<-doubles+1
    }
    else{
      doubles<-0
    }
    place<-(place+d1+d2)%%40
    if(doubles==3){
      doubles<-0
      place<-10
    }
    else{
      switch(as.character(place),
             "30"=place<-10,
             "8"=place<-chance(place),
             "22"=place<-chance(place),
             "36"=place<-chance(place),
             "2"=place<-community_chest(place),
             "17"=place<-community_chest(place),
             "33"=place<-community_chest(place))
      if(place==33){
        place<-community_chest(place)
      }
    }
    freq[place+1]<-freq[place+1]+1
  }
}
time2<-Sys.time()
difftime(time2,time1)

freq2<-freq
ans<-{}
for(i in 1:40){
  ans[i]<-which(freq2==max(freq2))-1
  freq2[ans[i]+1]<--1
}
ans
plot(freq)
