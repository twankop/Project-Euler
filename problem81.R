#Project Euler problem 81
#Find the minimal path sum, in a 80 by 80 matrix, 
#from the top left to the bottom right by only moving right and down.

matrix <- read.csv("~/Documents/Huiswerk/Project Euler/matrix.txt", header=F)
potential_matrix<-mat.or.vec(80,80)
potential_matrix[80,80]<-matrix[80,80]
for(i in 1:79){
  potential_matrix[80,80-i]<-matrix[80,80-i]+potential_matrix[80,80-i+1]
}
for(i in 1:79){
  potential_matrix[80-i,80]<-matrix[80-i,80]+potential_matrix[80-i+1,80]
}
for(i in 1:79){
  for(j in 1:79){
    potential_matrix[80-i,80-j]<-matrix[80-i,80-j]+
                                 min(potential_matrix[80-i,80-j+1],potential_matrix[80-i+1,80-j])
  }
}
potential_matrix[1,1]