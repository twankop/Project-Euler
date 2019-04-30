#Project Euler problem 42
#How many words in words.txt are triangl


words <- scan("~/Documents/Huiswerk/Programeer Oefeningen/Project Euler/words.txt", what="character", sep=",")

CharToValue<-function(a){
  return(which(LETTERS==a))
}

# Start the clock!
ptm <- proc.time()

L<-length(words)

M<-0
for(i in 1:L){
  if(nchar(words[i])>M){
    M<-nchar(words[i])
  }
}

triangle<-{}
t<-0
n<-1
while(t<=26*M){
  t<-n*(n+1)/2
  triangle[n]<-t
  n<-n+1
}

count<-0
for(i in 1:L){
  l<-0
  s<-0
  word<-words[i]
  letters<-substring(word, c(1:nchar(word)), c(1:nchar(word)))
  l<-length(letters)
  for(j in 1:l){
    s<-s+CharToValue(letters[j])
  }
  if(is.element(s,triangle)){
    count<-count+1
  }
}

count


# Stop the clock
proc.time() - ptm
