#Q4
U <- runif(100)
expo_sample<- (-1)*50*log(1-U)

#Q5
dist_sample<-c()
for ( i in 1:100){
  biased_coin_toss<-sample(c(0,1), size = 1,  rep = T, prob=c(0.3,0.7))
  if (biased_coin_toss==0){
    x<-rnorm(1,mean = 2)
    dist_sample<-c(dist_sample,x)
  }
  else{
    x<-rnorm(1,mean = -2)
    dist_sample<-c(dist_sample,x)
  }
}


#Q6
X_coord<-c()
Y_coord<-c()
while (length(X_coord)<100){
  X<-runif(1,max=3)
  Y<-runif(1,max=4)
  if (4*X + 3*Y - 12 < 0){
    X_coord<-c(X_coord,X)
    Y_coord<-c(Y_coord,Y)
  }
}

#Q7
X_coord1<-c()
Y_coord1<-c()
while (length(X_coord1)<100){
  X<-runif(1)
  Y<-runif(1)
  if ((X-0.5)^2 + (Y-0.5)^2 - 0.5 < 0){
    X_coord1<-c(X_coord1,X)
    Y_coord1<-c(Y_coord1,Y)
  }
}
 #Q8
t_dist_sampling<-function(mu,n){
P<-rnorm(n)
Q1<-rnorm(n)
Q2<-rnorm(n)
Q3<-rnorm(n)
Q<- Q1^2 + Q2^2 + Q3^2
X<-(P*sqrt(3))/sqrt(Q) + mu
return(X)
}

#Q9
hist(t_dist_sampling(2,10000),breaks=100,freq = FALSE,  # Density scale (not counts)
     col = "lightblue", 
     border = "white",
     main = "Histogram vs. Theoretical PDF (t-distribution, df=3)",
     xlab = "x", 
     ylim = c(0, 0.4))  # Adjust y-axis to fit PDF

# Overlay theoretical PDF (t-distribution with df=3)
curve(dt(x-2, df = 3), 
      add = TRUE, 
      col = "red", 
      lwd = 2, 
      lty = 2)

#Q10
# Empirical CDF (EDF)
plot(ecdf(t_dist_sampling(2,100)), col = "blue", lwd = 2, main = "EDF vs. Theoretical CDF (t-distribution, df=3)")

# Theoretical CDF of t(3)
curve(pt(x-2, df = 3), add = TRUE, col = "red", lwd = 2, lty = 2)

# Add legend
legend("bottomright", legend = c("Empirical CDF (EDF)", "Theoretical CDF (t(3))"), 
       col = c("blue", "red"), lwd = 2, lty = c(1, 2))

#Q11
mean_vec<-c()
median_vec<-c()
for (i in (1:1000)){
  p<-mean(t_dist_sampling(2,100))
  q<-median(t_dist_sampling(2,100))
  mean_vec<-c(mean_vec,p)
  median_vec<-c(median_vec,q)
}

hist(mean_vec,breaks=30,freq = FALSE,
     col = "lightblue", 
     border = "white",,
     xlab = "x")
hist(median_vec,breaks=30,freq = FALSE,
     col = "lightblue", 
     border = "white",
     xlab = "x")
#Q12
EDF_sample<-t_dist_sampling(2,100)
mean_edf<-c()
median_edf<-c()
for (i in 1:1000){
  x<-sample(EDF_sample, size = 100, rep = T)
  mean_edf<-c(mean_edf,mean(x))
  median_edf<-c(median_edf,median(x))
}
par(mfrow=c(1,2))
hist(mean_edf,breaks=30, freq = F,
     col = "lightblue", 
     border = "white",,
     xlab = "x")
hist(median_edf,breaks=30, freq = F,
     col = "lightblue", 
     border = "white",
     xlab = "x")
par(mfrow=c(1,1))
