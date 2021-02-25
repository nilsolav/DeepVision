require(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
setwd('D:/repos/Github/deepVision')
install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")

dat <- data.table::fread("Predictions_vs_catchdata2018.csv", sep=";",header=T)
names(dat)

#
# Try different distributions
#

# try with poisson distribution
m1 <- glm(Herring_catch~Herring_pred+0, family="poisson", data=dat)
# Overdispersed?
E1 <- resid(m1, type = "pearson")
sum(E1^2)
N  <- nrow(dat)
p  <- length(coef(m1))
sum(E1^2) / (N - p)
# [1] 88443.13 Hugely overdispresed!!!

# An alternate approach for data with overdispersion is negative binomial regression. 
ggplot(dat,aes(y=Herring_catch,x=Herring_pred)) +geom_point()+geom_smooth(method = "glm.nb", formula=y~x-1, se=F)
# did not converge

# And then quasi poisson assuming the variance to be proportional to the mean
ggplot(dat,aes(y=Herring_catch,x=Herring_pred)) +geom_point()+geom_smooth(method = "glm", formula=y~x-1, se = F,method.args = list(family = "quasipoisson"))
# Resduals are clearly not ok

# Try to add 1 to a log log rtegression instead
ggplot(dat,aes(y=log(Herring_catch+1),x=log(Herring_pred+1))) +geom_point()+geom_smooth(method = "glm", formula=y~x, se = F)
# this looks somewhat better, but still there is a residual issue

#
# log-log model for stats
#

# Add in the other covariates
fit_Herring1=lm(log(Herring_catch+1)~log(Herring_pred+1),data=dat)
fit_Herring2a=lm(log(Herring_catch+1)~log(Herring_pred+1)+log(BW_pred+1),data=dat)
fit_Herring2b=lm(log(Herring_catch+1)~log(Herring_pred+1)+log(Mackerel_pred+1),data=dat)
fit_Herring3=lm(log(Herring_catch+1)~log(Herring_pred+1)+log(BW_pred+1)+log(Mackerel_pred+1),data=dat)

# compare models
anova(fit_Herring1,fit_Herring2a)
anova(fit_Herring1,fit_Herring2b) # Not significant
anova(fit_Herring2a,fit_Herring3) # Not significant
# Model 2a is used
summary(fit_Herring2a)
plot(fit_Herring2a)

p1<-ggplot(dat,aes(y=log(Herring_catch+1),x=log(Herring_pred+1),col=log(BW_pred+1))) +geom_point()+geom_smooth(method = "lm",  formula=y~x, se = F)

p1
# Blue whiting
fit_BW1=lm(log(BW_catch+1)~log(BW_pred+1),data=dat)
fit_BW2a=lm(log(BW_catch+1)~log(BW_pred+1)+log(Herring_pred+1),data=dat)
fit_BW2b=lm(log(BW_catch+1)~log(BW_pred+1)+log(Mackerel_pred+1),data=dat)
fit_BW3=lm(log(BW_catch+1)~log(BW_pred+1)+log(Mackerel_pred+1)++log(Herring_pred+1),data=dat)
anova(fit_BW1,fit_BW2a)
anova(fit_BW1,fit_BW2b)
anova(fit_BW1,fit_BW3)

summary(fit_BW1)

p2<-ggplot(dat,aes(y=log(BW_catch+1),x=log(BW_pred+1))) +geom_point()+geom_smooth(method = "lm",  formula=y~x, se = F)

p2
anova(fit_Herring1,fit_Herring2a)
anova(fit_Herring1,fit_Herring2b) # Not significant
anova(fit_Herring2a,fit_Herring3) # Not significant


p2<-ggplot(dat,aes(y=log(BW_catch+1),x=log(BW_pred+1),col=log(Herring_pred+1))) +geom_point()+geom_smooth(method = "glm",  formula=y~x, se = F)

# Mackerel
fit_Mackerel=lm(log(Mackerel_catch+1)~log(Herring_pred+1)+log(BW_pred+1)+log(Mackerel_pred+1),data=dat)
summary(fit_Mackerel)
p3<-ggplot(dat,aes(y=log(Mackerel_catch+1),x=log(Mackerel_pred+1),col=log(Herring_pred+1))) +geom_point()+geom_smooth(method = "lm",  formula=y~x, se = F)

require(grid)
require(ggpubr)
p1
p2
p3


#
# Linear models for comparison with confusion matrix
#

# Linear model
fit_Herring_lin =lm(Herring_catch ~0+Herring_pred+BW_pred+Mackerel_pred,data=dat)
summary(fit_Herring_lin)
fit_BW_lin      =lm(BW_catch      ~0+Herring_pred+BW_pred+Mackerel_pred,data=dat)
summary(fit_BW_lin)
fit_Mackerel_lin=lm(Mackerel_catch~0+Herring_pred+BW_pred+Mackerel_pred,data=dat)
summary(fit_BW_Mackerel)

# Extract linear coefficients
lin <- fit_Herring_lin$coefficients
lin <- rbind(lin,fit_BW_lin$coefficients)
lin <- rbind(lin,fit_Mackerel_lin$coefficients)

p3<-ggplot(dat,aes(y=log(1+Herring_catch),x=log(1+Herring_pred))) +
  geom_point() + 
  geom_smooth(method = "lm",  formula=y~0+x, se = F) +
  geom_smooth(method = "lm",  formula=y~x, se = F, colour="red") +
  scale_x_continuous(name="log(1+Herring Predicted)", limits=c(0, 10)) +
  scale_y_continuous(name="log(1+Herring Catch)", limits=c(0, 8))
p3<-ggplot(dat,aes(y=Herring_catch,x=Herring_pred)) +
  geom_point() + 
  geom_smooth(method = "lm",  formula=y~0+x, se = F) +
  geom_smooth(method = "lm",  formula=y~x, se = F, colour="red") 
  scale_x_continuous(name="log(1+Herring Predicted)", limits=c(0, 10)) +
  scale_y_continuous(name="log(1+Herring Catch)", limits=c(0, 8))
p3
p3<-ggplot(dat,aes(y=log(Mackerel_catch+1),x=log(Mackerel_pred+1),col=log(Herring_pred+1))) +
  geom_point()+geom_smooth(method = "lm",  formula=y~x, se = F)+

p3
# Herring_pred  BW_pred       Mackerel_pred
# 0.0606497897 -0.005805911   0.008048434
#-0.0013641536  0.110024823  -0.004669844
#-0.0008735669  0.002313991   0.024529442
1/lin


