require(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
require(moonBook)
require(ggplot2)
library(grid)
require(gridExtra)
require(ggiraph)
require(ggiraphExtra)
library(colorspace)
setwd('D:/repos/Github/DeepVision')

dat <- data.table::fread("Predictions_vs_catchdata2018.csv", sep=";",header=T)
dat$Herring_other = dat$Mackerel_catch+dat$BW_catch
dat$BW_other = dat$Mackerel_catch+dat$Herring_catch
dat$Mackerel_other = dat$Herring_catch+dat$BW_catch
dat$Pred = dat$Herring_pred+dat$BW_pred+dat$Mackerel_pred
dat$Catch = dat$Herring_catch+dat$BW_catch+dat$Mackerel_catch

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
# Settle with log-log model for stats
#

#
# Cathces combined
#

# Add in the other covariates
fit_all=lm(log(Pred+1)~log(Catch+1),data=dat)
#fit_all=lm(Catch~Pred,data=dat)
summary(fit_all)
anova(fit_all)
fit <- fit_all

#plot(fit_all)
p1log<-ggplot(dat,aes(y=log(Pred+1),x=log(Catch+1))) +
  geom_point()+#geom_smooth(method = "lm",  formula=y~x, se = F) +
  #geom_point()+geom_smooth(method = "lm",  formula=y~0+x, se = F, col='red') +
  geom_abline(slope=coef(fit)[2], intercept=coef(fit)[1], col="black") + # for value 0 of covariate 2   (BW_other+1)
  scale_y_continuous(name="log(1+Predicted)") +
  scale_x_continuous(name="log(1+Catch)")
p1log
coef(fit)
#
# Herring
#

# Add in the other covariates
fit_Herring1=lm(log(Herring_pred+1)~log(Herring_catch+1),data=dat)
fit_Herring2=lm(log(Herring_pred+1)~log(Herring_catch+1)*log(Herring_other+1),data=dat)
# compare models
anova(fit_Herring1,fit_Herring2)
# Model 2 is used for Herring
summary(fit_Herring2)
fit <- fit_Herring2

# Herring model plot
p2log<-ggplot(dat,aes(y=log(Herring_pred+1),x=log(Herring_catch+1),col=log(Herring_other+1))) +
  geom_point()+#geom_smooth(method = "lm",  formula=y~x, se = F) +
  geom_point()+#geom_smooth(method = "lm",  formula=y~0+x, se = F, col='red') +
  scale_y_continuous(name="log(1+Herring Predicted)") +
  scale_x_continuous(name="log(1+Herring Catch)")  +
  scale_color_continuous(limits=c(0,10), low = "blue",
                         high = "red", space = "Lab", name = "")+
  theme(legend.position = c(0.8, 0.3)) +
  geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*0),intercept=(coef(fit)[1]+coef(fit)[3]*0), col="blue") + # for value 0 of covariate 2   (BW_other+1)
  #geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*2),intercept=(coef(fit)[1]+coef(fit)[3]*2), col="green") + # for value 2 of covariate 2   (BW_other+1)
  #geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*4),intercept=(coef(fit)[1]+coef(fit)[3]*4), col="green") + # for value 4 of covariate 2   (BW_other+1)
  #geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*6),intercept=(coef(fit)[1]+coef(fit)[3]*6), col="green") + # for value 6 of covariate 2   (BW_other+1)
  geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*8),intercept=(coef(fit)[1]+coef(fit)[3]*8), col="red")   # for value 8 of covariate 2   (BW_other+1)
p2log

#
# Blue whiting
#

fit_BW1=lm(log(BW_pred+1)~log(BW_catch+1),data=dat)
fit_BW2=lm(log(BW_pred+1)~log(BW_catch+1)*log(BW_other+1),data=dat)

anova(fit_BW1,fit_BW2)
summary(fit_BW2)
fit <- fit_BW2

# Blue whiting model plot
p3log<-ggplot(dat,aes(y=log(BW_pred+1),x=log(BW_catch+1),col=log(BW_other+1))) +
  geom_point()+#geom_smooth(method = "lm",  formula=y~x, se = F) +
  geom_point()+#geom_smooth(method = "lm",  formula=y~0+x, se = F, col='red') +
  scale_y_continuous(name="log(1+BW Predicted)") +
  scale_x_continuous(name="log(1+BW Catch)") +
  theme(legend.position = c(0.9, 0.2),legend.title = element_blank()) +
  scale_color_continuous(limits=c(0,10), low = "blue",
                         high = "red", space = "Lab", guide = FALSE)+
  geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*0),intercept=(coef(fit)[1]+coef(fit)[3]*0), col="blue") + # for value 0 of covariate 2   (BW_other+1)
  #geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*2),intercept=(coef(fit)[1]+coef(fit)[3]*2), col="green") + # for value 2 of covariate 2   (BW_other+1)
  #geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*4),intercept=(coef(fit)[1]+coef(fit)[3]*4), col="green") + # for value 4 of covariate 2   (BW_other+1)
  #geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*6),intercept=(coef(fit)[1]+coef(fit)[3]*6), col="green") + # for value 6 of covariate 2   (BW_other+1)
  geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*8),intercept=(coef(fit)[1]+coef(fit)[3]*8), col="red")   # for value 8 of covariate 2   (BW_other+1)
p3log
#
# Mackerel
#

fit_mackerel1=lm(log(Mackerel_catch+1)~log(Mackerel_pred+1),data=dat)
fit_mackerel2=lm(log(Mackerel_pred+1)~log(Mackerel_catch+1)*log(Mackerel_other+1),data=dat)
anova(fit_mackerel1,fit_mackerel2)
summary(fit_mackerel2)
#mackerel_other = 0:2:10
fit <- fit_mackerel2
# Mackerel plot
p4log<-ggplot(dat,aes(y=log(Mackerel_pred+1),x=log(Mackerel_catch+1),col=log(Mackerel_other+1))) +
  geom_point()+#geom_smooth(method = "lm",  formula=y~x, se = F) +
  geom_point()+#geom_smooth(method = "lm",  formula=y~0+x, se = F, col='red') +
  scale_y_continuous(name="log(1+Mackerel Predicted)") +
  scale_x_continuous(name="log(1+Mackerel Catch)") +
  #scale_color_gradient(guide=FALSE,limits=c(0,10))+
  scale_color_continuous(limits=c(0,10), low = "blue",
                       high = "red", space = "Lab", guide = FALSE)+
  # this plots the multiple regression with interaction:
  geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*0),intercept=(coef(fit)[1]+coef(fit)[3]*0), col="blue") + # for value 0 of covariate 2   (BW_other+1)
  #geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*2),intercept=(coef(fit)[1]+coef(fit)[3]*2), col="green") + # for value 2 of covariate 2   (BW_other+1)
  #geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*4),intercept=(coef(fit)[1]+coef(fit)[3]*4), col="green") + # for value 4 of covariate 2   (BW_other+1)
  #geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*6),intercept=(coef(fit)[1]+coef(fit)[3]*6), col="green") + # for value 6 of covariate 2   (BW_other+1)
  geom_abline(slope=(coef(fit)[2]+coef(fit)[4]*8),intercept=(coef(fit)[1]+coef(fit)[3]*8), col="red")   # for value 8 of covariate 2   (BW_other+1)
p4log

myplot1 <- arrangeGrob(p1log, 
                       top = textGrob("(A)", x = unit(0, "npc")
                                      , y   = unit(1, "npc"), just=c("left","top"),
                                      gp=gpar(col="black", fontsize=18)))

myplot2 <- arrangeGrob(p2log, top = textGrob("(B)", x = unit(0, "npc")
                                               , y = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(col="black", fontsize=18)))

myplot3 <- arrangeGrob(p3log, top = textGrob("(C)", x = unit(0, "npc")
                                               , y  = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(col="black", fontsize=18)))

myplot4 <- arrangeGrob(p4log, top = textGrob("(D)", x = unit(0, "npc")
                                               , y = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(col="black",    fontsize=18)))
g <- arrangeGrob(myplot1,myplot2,myplot3,myplot4, ncol=2,nrow=2)

ggsave(file="CatchVsPredictions.png", g)




