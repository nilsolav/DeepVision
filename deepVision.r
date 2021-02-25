library(ggplot2)
library(dplyr)
library(tidyr)
require(plyr)
#install.packages("devtools")
#devtools::install_github("cardiomoon/ggiraphExtra")
devtools::install_github("cardiomoon/moonBook")
require(ggiraph)
require(ggiraphExtra)
require(moonBook)
require(ggplot2)

require(foreign)
require(MASS)

setwd('D:/repos/Github/deepVision')


dat <- data.table::fread("Predictions_vs_catchdata2018.csv", sep=";",header=T)
names(dat)

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
ggplot(dat,aes(y=log(Herring_catch+1),x=log(Herring_pred+1))) +geom_point()+geom_smooth(method = "glm", formula=y~x-1, se = F)



# Add in the other covariates
fit=lm(log(Herring_catch+1)~log(Herring_pred+1)+log(BW_pred+1)+log(Mackerel_pred+1)+0,data=dat)
summary(fit)



fit3=lm(log(Herring_catch+1)~log(Herring_pred+1)+log(BW_pred+1)+log(Mackerel_pred+1)+0,data=dat)
summary(fit3)
dat

ggplot(dat,aes(y=log(Herring_catch+1),x=log(Herring_pred+1),col=log(BW_pred+1))) +geom_point()+geom_smooth(method = "glm",  formula=y~x-1, se = F)



ggPredict(fit,interactive=TRUE,data=dat)



#df <- data.frame(matrix(as.numeric(as.character((unlist(dat)))), nrow=20, byrow=T),stringsAsFactors=FALSE)

#df <- data.frame(matrix(unlist(dat), nrow=20, byrow=T),stringsAsFactors=FALSE)
#names(df) <- names(dat)
#df <- sapply(dat, function(x) as.numeric(x))
#df


#data2 <- gather(data,set,count,Both,Årsmaterialet,NMDbiotic)
#df2 <- gather(df,set,count,Both,Årsmaterialet,NMDbiotic)


# Split into pred and observations
d = NULL
# The prediction from the deep model
d$pred <- df[,c(2,4,6)]
# The count from the trawl
d$obs <- df[,c(3,5,7)]

#d
d$bw_obs <- d$obs[,1]
d$herring_obs <- d$obs[,2]
d$mackerel_obs <- d$obs[,3]


# Overall fraction across stations
fraction <- colSums(d$pred)/colSums(d$obs)
print(fraction)

# Run linear model
lmod <- lm(obs~pred+0,data=d)
lmodbw <- lm(bw_obs~pred+0,data=d)
lmodherr <- lm(herring_obs~pred+0,data=d)
lmodmack <- lm(mackerel_obs~pred+0,data=d)
summary(lmodbw)
# Predict
d$mod <- predict(lmod)

ggPredict(lmodbw,interactive = TRUE)

i=1
plot(log(d$pred[,i]),log(d$mod[,i]))
points(log(d$pred[,i]),log(d$obs[,i]),col='red')
i=2
plot(d$pred[,i],d$mod[,i])
points(d$pred[,i],d$obs[,i],col='red')
i=3
plot(d$pred[,i],d$mod[,i])
points(d$pred[,i],d$obs[,i],col='red')


#lmodmack$fitted.values
?ggplot
ggplot(data=d, aes(bw_obs,pred)) +
  geom_line()


lmod$coefficients
summary(lmod)
summary(lmodbw)
summary(lmodherr)
summary(lmodmack)
summary(lmod)
plot(lmodbw)

summary(lmodherr)
summary(lmodmack)

plot(lmod)
?lm

