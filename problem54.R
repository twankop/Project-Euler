#Project Euler problem 54
#How many hands does Player 1 win?

setwd("~/Documents/Huiswerk/Project Euler")
poker <- read.table("~/Documents/Huiswerk/Project Euler/poker.txt", quote="\"")


wins<-0
for(game in 1:1000){
  p1hand<-mat.or.vec(2,5)
  for(i in 1:5){
    p1hand[1,i]<-substr(poker[game,i],1,1)!!!!!!!as.integer
    p1hand[2,i]<-substr(poker[game,i],2,2)
  }
  p2hand<-poker[game,5:10]
}
clubs<-c("2C","3C","4C","5C","6C","7C","8C","9C","TC","JC","QC","KC","AC")
diamonds<-c("2D","3D","4D","5D","6D","7D","8D","9D","TD","JD","QD","KD","AD")
hearts<-c("2H","3H","4H","5H","6H","7H","8H","9H","TH","JH","QH","KH","AH")
spades<-c("2S","3S","4S","5S","6S","7S","8S","9S","TS","JS","QS","KS","AS")
p1hand

??string
poker[1,1]=="8C"

as.integer("8")
