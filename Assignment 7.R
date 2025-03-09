#Q1
library(lmreg)
data("worldpop")
Ind<-c(rep(0,10),rep(1,10))
x1<-worldpop$Year
x2<-c(rep(0,10),1:10)
lm1<-lm(worldpop$Pop.billion~x1+x2)
summary(lm1)
summary(lm1)$r.squared

#Q3
x1_squared<-x1^2
x4<-Ind*((x1^2)-(1990^2))
lm2<-lm(worldpop$Pop.billion~x1+x1_squared+x2+x4)
summary(lm2)
summary(lm2)$r.squared

#Q7
x5<-Ind*((x1-1990)^2)
lm3<-lm(worldpop$Pop.billion~x1+x1_squared+x5)
summary(lm3)
summary(lm3)$r.squared

#Q10
data("leprosy")
head(leprosy)
lm4 <- lm(post~pre+factor(treatment),data=leprosy)
summary(lm4)

#Q11
Ind_A<-rep(0,length(leprosy$treatment));Ind_A[leprosy$treatment=="A"]<-1
Ind_D<-rep(0,length(leprosy$treatment));Ind_D[leprosy$treatment=="D"]<-1
lm5<-lm(post~pre+Ind_A+Ind_D,data=leprosy)
summary(lm5)

#Q12
Ind_F<-rep(0,length(leprosy$treatment));Ind_F[leprosy$treatment=="F"]<-1
lm6<-lm(post~pre+Ind_A+Ind_F,data=leprosy)
summary(lm6)