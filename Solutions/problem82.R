#Project Euler problem 82
#Find the minimal path sum, in a 80 by 80 matrix, 
#from any left cell to any right cell by only moving right, up and down.

matrix <- read.csv("~/Documents/Huiswerk/Programeer Oefeningen/Project Euler/matrix.txt", header=F)

initialiseDistanceMatrix<-function(distanceMatrix){
  distanceMatrix[,1]<-matrix[,1]
  for(i in 2: 80){
    distanceMatrix[,i]<-rowSums(matrix[,1:i])
  }
  return(distanceMatrix)
}

applyDijkstra<-function(distanceMatrix){
  visitMatrix<-mat.or.vec(80,80)+1
  visitMatrix[,1]<-rep(Inf,80)
  while(min(visitMatrix[,79])==1){
    testMatrix<-visitMatrix*distanceMatrix
    coords<-which(testMatrix==min(testMatrix), arr.ind=TRUE)
    coord1<-coords[1,1]
    coord2<-coords[1,2]
    visitMatrix[coord1,coord2]<-Inf
    if(coord2<80){
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
      if(distanceMatrix[coord1,coord2+1]>distance+matrix[coord1,coord2+1]){
        distanceMatrix[coord1,coord2+1]<-distance+matrix[coord1,coord2+1]
      }
    }
  }
  return(distanceMatrix)
}

# Start the clock!
ptm <- proc.time()

distanceMatrix<-mat.or.vec(80,80)
distanceMatrix<-initialiseDistanceMatrix(distanceMatrix)
distanceMatrix<-applyDijkstra(distanceMatrix)

min(distanceMatrix[,80])

# Stop the clock
proc.time() - ptm

