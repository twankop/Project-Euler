#Project Euler problem 493

#70 colored balls are placed in an urn, 10 for each of the seven rainbow colors.
#What is the expected number of distinct colors in 20 randomly picked balls?


# Start the clock!
ptm <- proc.time()

#Without replacement
print(7*(1-choose(7,6)*choose(60,20)/choose(70,20))+
6*(choose(7,6)*choose(60,20)/choose(70,20)-choose(7,5)*choose(50,20)/choose(70,20))+
5*(choose(7,5)*choose(50,20)/choose(70,20)-choose(7,4)*choose(40,20)/choose(70,20))+
4*(choose(7,4)*choose(40,20)/choose(70,20)-choose(7,3)*choose(30,20)/choose(70,20))+
3*(choose(7,3)*choose(30,20)/choose(70,20)-choose(7,2)*choose(20,20)/choose(70,20))+
2*(choose(7,2)*choose(20,20)/choose(70,20)), digits=20)


#With replacement
print(7*(1-choose(7,6)*(60/70)^20)+
6*(choose(7,6)*(60/70)^20-choose(7,5)*(50/70)^20)+
5*(choose(7,5)*(50/70)^20-choose(7,4)*(40/70)^20)+
4*(choose(7,4)*(40/70)^20-choose(7,3)*(30/70)^20)+
3*(choose(7,3)*(30/70)^20-choose(7,2)*(20/70)^20)+
2*(choose(7,2)*(20/70)^20-choose(7,1)*(10/70)^20)+
choose(7,1)*(10/70)^20, digits = 10)


#Simulation
N<-1000000
h<-seq(1,1, length=10)
balls<-c(h,2*h,3*h,4*h,5*h,6*h,7*h)
result<-{}
for(i in 1:N){
  result[i]<-length(unique(sample(balls, 20, replace = FALSE)))
}
mean(result)
# Stop the clock
proc.time() - ptm

var(result)

# print((7*(choose(70,20)-choose(7,6)*choose(60,20))+
#         6*(choose(7,6)*choose(60,20)-choose(7,5)*choose(50,20))+
#         5*(choose(7,5)*choose(50,20)-choose(7,4)*choose(40,20))+
#         4*(choose(7,4)*choose(40,20)-choose(7,3)*choose(30,20))+
#         3*(choose(7,3)*choose(30,20)-choose(7,2)*choose(20,20))+
#         2*(choose(7,2)*choose(20,20)))/choose(70,20), digits=20)


#N<-1000000
#> mean(result)
#[1] 6.818776
#var(result)
#[1] 0.160768
#user   system  elapsed 
#1975.328  190.631 2174.669 
