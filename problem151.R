


draw.sheet <- function(sheets){
  if(length(sheets) > 1){
    sample <- sample(sheets, size = 1)
  } else{
    sample <- sheets
  }
  
  if(sample == 5){
    sheets <- sheets[-max(which(sheets == 5))]
  }
  else if(sample == 4){
    sheets <- c(sheets[-max(which(sheets == 4))], 5, 5)
  }
  else if(sample == 3){
    sheets <- c(sheets[-max(which(sheets == 3))], 4, 4)
  }
  else if(sample == 2){
    sheets <- c(sheets[-max(which(sheets == 2))], 3, 3)
  }
  return(sheets)
}

count.times.single.sheets <- function(sheet){
  count <- 0
  while(length(sheets) > 0){
    sheets <- draw.sheet(sheets)
    if(length(sheets) == 1){
      count <- count + 1
    }
  }
  return(count)
}


sheets <- c(2:5)
count <- 0
n <- 1000000
for(i in 1:n){
  count <- count + count.times.single.sheets(sheets)-1
}
mean.count <- count/n
mean.count


