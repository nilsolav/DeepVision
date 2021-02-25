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
setwd('D:/repos/Github/deepVision')

dat <- data.table::fread("Predictions_vs_catchdata2018.csv", sep=";",header=T)
dat$Herring_other = dat$Mackerel_pred+dat$BW_pred
dat$BW_other = dat$Mackerel_pred+dat$Herring_pred
dat$Mackerel_other = dat$Herring_pred+dat$BW_pred
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
fit_all=lm(log(Catch+1)~log(Pred+1),data=dat)
fit_all=lm(Catch~Pred,data=dat)
summary(fit_all)
#plot(fit_all)
p1log<-ggplot(dat,aes(y=log(Catch+1),x=log(Pred+1))) +
  geom_point()+geom_smooth(method = "lm",  formula=y~x, se = F) +
  geom_point()+geom_smooth(method = "lm",  formula=y~0+x, se = F, col='red') +
  scale_x_continuous(name="log(1+Predicted)") +
  scale_y_continuous(name="log(1+Catch)")
p1log

#
# Herring
#

# Add in the other covariates
fit_Herring1=lm(log(Herring_catch+1)~log(Herring_pred+1),data=dat)
fit_Herring2=lm(log(Herring_catch+1)~log(Herring_pred+1) + log(Herring_other+1),data=dat)
# compare models
anova(fit_Herring1,fit_Herring2)
# Model 2 is used for Herring
summary(fit_Herring2)

# This does not work...
#ggPredict(fit_Herring2,interactive=TRUE)
#ggPredict(fit_Herring2,dat)

# Herring model plot
p2log<-ggplot(dat,aes(y=log(Herring_catch+1),x=log(Herring_pred+1),col=log(Herring_other+1))) +
  geom_point()+geom_smooth(method = "lm",  formula=y~x, se = F) +
  geom_point()+geom_smooth(method = "lm",  formula=y~0+x, se = F, col='red') +
  scale_x_continuous(name="log(1+Herring Predicted)") +
  scale_y_continuous(name="log(1+Herring Catch)")  +
  scale_color_continuous(limits=c(0,13),name="") +
  theme(legend.position = c(0.8, 0.3))
p2log

#
# Blue whiting
#

fit_BW1=lm(log(BW_catch+1)~log(BW_pred+1),data=dat)
fit_BW2=lm(log(BW_catch+1)~log(BW_pred+1)+(BW_other+1),data=dat)
anova(fit_BW1,fit_BW2)
summary(fit_BW2)

# Blue whiting model plot
p3log<-ggplot(dat,aes(y=log(BW_catch+1),x=log(BW_pred+1),col=log(BW_other+1))) +
  geom_point()+geom_smooth(method = "lm",  formula=y~x, se = F) +
  geom_point()+geom_smooth(method = "lm",  formula=y~0+x, se = F, col='red') +
  scale_x_continuous(name="log(1+BW Predicted)") +
  scale_y_continuous(name="log(1+BW Catch)") +
  theme(legend.position = c(0.9, 0.2),legend.title = element_blank()) +
  scale_color_continuous(guide=FALSE,limits=c(0,13))
p3log

#
# Mackerel
#

fit_mackerel1=lm(log(Mackerel_catch+1)~log(Mackerel_pred+1),data=dat)
fit_mackerel2=lm(log(Mackerel_catch+1)~log(Mackerel_pred+1)+(Mackerel_other+1),data=dat)
anova(fit_mackerel1,fit_mackerel2)
summary(fit_BW2)

# Mackerel plot
p4log<-ggplot(dat,aes(y=log(Mackerel_catch+1),x=log(Mackerel_pred+1),col=log(Mackerel_other+1))) +
  geom_point()+geom_smooth(method = "lm",  formula=y~x, se = F) +
  geom_point()+geom_smooth(method = "lm",  formula=y~0+x, se = F, col='red') +
  scale_x_continuous(name="log(1+Mackerel Predicted)") +
  scale_y_continuous(name="log(1+Mackerel Catch)") +
  scale_color_continuous(guide=FALSE,limits=c(0,13))
#theme(legend.position = c(0.9, 0.2),legend.title = element_blank())
#theme(legend.position="bottom")



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
g


# Extract linear coefficients
lin <- t(fit_all$coefficients)
lin <- cbind(lin,0)
lin <- rbind(lin,fit_Herring2$coefficients)
lin <- rbind(lin,fit_BW2$coefficients)
lin <- rbind(lin,fit_mackerel2$coefficients)

# Herring_pred  BW_pred       Mackerel_pred
# 0.0606497897 -0.005805911   0.008048434
#-0.0013641536  0.110024823  -0.004669844
#-0.0008735669  0.002313991   0.024529442
1/lin


