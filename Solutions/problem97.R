

time1<-Sys.time()

A<-1
for(i in 1:7830457){
 A<-2*A %% 10000000000
}
A <- (28433 * A +1) %% 10000000000

time2<-Sys.time()
difftime(time2,time1)

A