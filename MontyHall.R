#### The Monty Hall Problem ####

library(combinat)

x<-c('s1','s2','c') # s1=sheep1, s2=sheep2, c=car
per<-permn(x) # possibile permutations of the 3 prizes
per

doors<-c(1,2,3)
j<-sample(doors,1)

sim<-function(j) {
  choose<-sample(per,1)[[1]][j] # candidate's choice
  # 3 possible scenarios:
  # 1)
  if (choose=='s1') {
    open<-'s2' # door with s2 is opened
    choose<-'c' # the candidate changes his choice
  }
  # 2)
  else if (choose=='s2') {
    open<-'s1' # door with s1 is opened
    choose<-'c' # the candidate changes his choice
  }
  # 3)
  else if (choose=='c') {
    open<-sample(c('s1','s2'),1)[[1]]
    if (open=='s1') choose<-'s2'
    else if (open=='s2') choose<-'s1'
  }
  return(choose) }

# SIMULATION STUDY
m<-1000 # number of simulations
vec<-rep(0,m)
for (i in 1:m) {
  j<-sample(c(1,2,3),1)
  vec[i]<-sim(j)
}
table(vec)/m
# The proportion by which the candidate wins the car is approximately of 2/3.

# Plot:
M<-1000
car<-rep(0,M)
for(m in 1:M) {
  vec<-rep(0,m)
  for (i in 1:m) {
    j<-sample(c(1,2,3),1)
    vec[i]<-sim(j)
  } 
  car[m]<-(table(vec)/m)[1]
}
plot(car,type="l",xlab="Number of simulations",ylab="Chance of winning")
abline(h=2/3,col="red")
# As the number of simulations increases, the probability of winning becomes closer to the 
# theoretical value of 2/3.
