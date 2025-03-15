#Q1
library(lmreg)
data("leprosy")
lm1<-lm(post~factor(treatment),data=leprosy)
s<-summary(lm1)$coefficient
x0<-rep(1,length(leprosy$treatment))
Ind_D<-rep(0,length(leprosy$treatment));Ind_D[leprosy$treatment=="D"]<-1
Ind_F<-rep(0,length(leprosy$treatment));Ind_F[leprosy$treatment=="F"]<-1
X<-cbind(x0,Ind_D,Ind_F)
residuals<-leprosy$post - (X %*% s[,1])
plot(leprosy$pre, residuals, xlab="Pre-Treatment Scores", ylab="Residual Error for the regression model fitted", main="Residual Plot")

#Q2
lm2<-lm(pre~factor(treatment),data=leprosy)
s2<-summary(lm2)$coefficient
residuals2<-leprosy$pre - (X %*% s2[,1])
plot(residuals2, residuals, xlab="Residual of regression of Pre-Treatment Scores on the Treatment methods", ylab="Residual Error for the regression model of post-treatment scores on treatment methods", main="Residual Plot")

#Q3
cor(residuals,residuals2)

#Q4
cor(leprosy$pre,leprosy$post)

#Q5
lm(residuals~residuals2)
abline(2.682e-15,9.872e-01)

