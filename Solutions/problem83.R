#Project Euler problem 83
#Find the minimal path sum, in a 80 by 80 matrix, 
#from the top left cell to the bottom right cell by only moving in any direction.

matrix <- read.csv("/home/twan/Dropbox/Projects/Project Euler/matrix.txt", header=F)

applyDijkstra<-function(distanceMatrix){
  visitMatrix<-mat.or.vec(80,80)+1
  while(min(visitMatrix[80,79],visitMatrix[79,80])==1){
    testMatrix<-visitMatrix*distanceMatrix
    coords<-which(testMatrix==min(testMatrix), arr.ind=TRUE)
    coord1<-coords[1,1]
    coord2<-coords[1,2]
    visitMatrix[coord1,coord2]<-Inf
    distance<-distanceMatrix[coord1,coord2]
    
    if(coord1>1){
      if(distanceMatrix[coord1-1,coord2]>distance+matrix[coord1-1,coord2]){
        distanceMatrix[coord1-1,coord2]<-distance+matrix[coord1-1,coord2]
      }
    }
    if(coord1<80){
      if(distanceMatrix[coord1+1,coord2]>distance+matrix[coord1+1,coord2]){
        distanceMatrix[coord1+1,coord2]<-distance+matrix[coord1+1,coord2]
      }
    }
    if(coord2>1){
      if(distanceMatrix[coord1,coord2-1]>distance+matrix[coord1,coord2-1]){
        distanceMatrix[coord1,coord2-1]<-distance+matrix[coord1,coord2-1]
      }
    }
    if(coord2<80){
      if(distanceMatrix[coord1,coord2+1]>distance+matrix[coord1,coord2+1]){
        distanceMatrix[coord1,coord2+1]<-distance+matrix[coord1,coord2+1]
      }
    }
  }
  return(distanceMatrix)
}

# Start the clock!
ptm <- proc.time()

distanceMatrix<-mat.or.vec(80,80)+Inf
distanceMatrix[1,1]<-matrix[1,1]
distanceMatrix<-applyDijkstra(distanceMatrix)

distanceMatrix[80,80]

# Stop the clock
proc.time() - ptm

