#Q1
a<-10
r<-2
GP<-NULL
for (i in 1:10){
  GP<-c(GP,a*r^(i-1))
}
#Q2
n<-10
ap<-5
cd<-5
AP<-a+(0:(n-1)*cd)
i<-1:(n-1)
sum_last_first<-AP[i]+AP[n-i+1]
#Q3
n<-100
pi_digits<-NULL
newpi<-pi
for (i in 1:n){
  pi_digits<-c(pi_digits,floor(newpi))
  newpi<-(newpi-pi_digits[length(pi_digits)])*10
}
#Q5
qgamma(0.5,shape=1.5,scale=1)
#Q6
x<-rnorm(25)
x[1]-mean(x[-1])
#Q7
v<-NULL
for (i in 1:25){
v<-c(v,x[i]-mean(x[-i]))
}
sum(v^2)
#Q8
library(lmreg)
data("worldpop")
worldpop_df<-data.frame(worldpop)
#Q9
plot(1981:1999,(worldpop_df$Pop.billion[-1]-worldpop_df$Pop.billion[-20]),type="l")
