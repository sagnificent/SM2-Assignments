x<-1e100
 y<-sin(x)^2 + cos(x)^2-1
fair_die_simulation<-sample(seq(1:6),10,replace=T,prob=rep((1/6),6))
biased_prob<-c(0.5,0.1,0.1,0.1,0.1,0.1)
biased_die_simulation<-sample(seq(1:6),10,replace=T,biased_prob)
roll_no_sampling<-sample(c("BS2445","BS2447","BS2448","BS2450","BS2452"),2)
roll_no_sampling_2450<-sample(c("BS2445","BS2447","BS2448","BS2450","BS2452"),2,prob=c((2/9),(2/9),(2/9),(1/9),(2/9)))
poisson_sample_1<-rpois(1,3)
exp_sample_1<-rexp(1, rate = (1/3))
normal_dist_sample_100<-rnorm(90000,mean=2)
mean(normal_dist_sample_100)
#P9
v<-rgeom(1,0.5)
sum_vector<-c()
for ( i in 1:90000){
B_tossing_sample<-sample(c(0,1),v,replace=T,prob=c(0.5,0.5))
sum_vector<-c(sum_vector,sum(B_tossing_sample==1))
}
mean(sum_vector)
plot(1:10000, cumsum(sum_vector)/(1:10000), type = "l", ylim = c(0,1), 
     xlab = "Cumulative number of trials", 
     ylab = "Cumulative average")