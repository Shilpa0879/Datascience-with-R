install.packages("survival") 
library(survival) 

survival_unemployment1<-read.csv("E:\\R-folder\\02-CSV files\\survival_unemployment1.csv")
attach(survival_unemployment1)  

# Define variables  
time <- spell 
group <- ui   ##Ui Unemployment insurance
X <- cbind(logwage, ui, age)
# Descriptive statistics 
summary(time)
summary(event)
summary(X)
summary(group) 

# Kaplan-Meier non-parametric analysis
## 1 - indicates combined group of unemployment and employment
kmsurvival <- survfit(Surv(time,event) ~ 1) 
plot(kmsurvival, xlab="Time", ylab="Survival Probability") 

# Kaplan-Meier non-parametric analysis by group
##group indicates unemployment and employment  
kmsurvival1 <- survfit(Surv(time, event) ~ group)
plot(kmsurvival1,xlab="Time", ylab="Survival Probability",col = c(2,3),conf.int = T)
