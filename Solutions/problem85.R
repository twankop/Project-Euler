#Project Euler problem 85
#Although there exists no rectangular grid that contains exactly two million rectangles, 
#find the area of the grid with the nearest solution.

rect_count<-function(l,h){
  s<-0
  for(i in 1:l){
    for(j in 1:h){
      s<-s+(l+1-i)*(h+1-j)
    }
  }
  s
}
max1<-0
while(rect_count(max1,1)<2000000){
  max1<-max1+1
}
max2<-0
while(rect_count(max2,max2)<2000000){
  max2<-max2+1
}
count<-mat.or.vec(max2,max1)
for(i in 1:max2){
  large<-FALSE
  j<-0
  while(j<=max1 && !large){
    j<-j+1
    count[i,j]<-rect_count(i,j)
    if(count[i,j]>2000000){
      large<-TRUE
    }
  }
}
ncount<-abs(count-2000000)
ind<-which.min(ncount)
h<-(floor(ind/max2)+1)
if(ind%%max2){
  l<-(ind%%max2)
}else{
  l<-max2
}
rect_count(l,h)
l*h
