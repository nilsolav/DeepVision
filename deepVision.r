library(ggplot2)
require(plyr)
library(dplyr)
library(tidyr)
#install.packages("devtools")
install.packages("ggpubr")
#devtools::install_github("cardiomoon/ggiraphExtra")
devtools::install_github("cardiomoon/moonBook")
require(ggiraph)
require(ggiraphExtra)
require(moonBook)
require(ggplot2)

require(foreign)
require(MASS)

require(gridExtra)
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
ggplot(dat,aes(y=log(Herring_catch+1),x=log(Herring_pred+1))) +geom_point()+geom_smooth(method = "glm", formula=y~x, se = F)
# this looks somewhat better, but still there is a residual issue

#
# log-log model for stats
#

# Add in the other covariates
fit_Herring=lm(log(Herring_catch+1)~log(Herring_pred+1)+log(BW_pred+1)+log(Mackerel_pred+1),data=dat)
summary(fit_Herring)
p1<-ggplot(dat,aes(y=log(Herring_catch+1),x=log(Herring_pred+1),col=log(BW_pred+1))) +geom_point()+geom_smooth(method = "glm",  formula=y~x, se = F)

# Blue whiting
fit_BW=lm(log(BW_catch+1)~log(Herring_pred+1)+log(BW_pred+1)+log(Mackerel_pred+1),data=dat)
summary(fit_BW)
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


Predictions, splitting on grouping vars
library(plyr)

# Using lm, predict mpg from wt, and split on cyl
# Using dlply will create a list of lm objects for each subset
models_lm <- dlply(mtcars, "cyl", lm, formula = mpg ~ wt)

# Now we use ldply to create a prediction data frame that uses each model
predicted_lm_cyl <- ldply(models_lm, predictvals, xvar = "wt", yvar = "mpg")

# Finally, make a plot
p + geom_line(data=predicted_lm_cyl, colour="blue") + facet_wrap(~cyl)

#
# Linear models for comparison with confusion matrix
#

# Linear model
fit_Herring_lin =lm(Herring_catch ~Herring_pred+BW_pred+Mackerel_pred+0,data=dat)
summary(fit_Herring_lin)
fit_BW_lin      =lm(BW_catch      ~Herring_pred+BW_pred+Mackerel_pred+0,data=dat)
summary(fit_BW_lin)
fit_Mackerel_lin=lm(Mackerel_catch~Herring_pred+BW_pred+Mackerel_pred+0,data=dat)
summary(fit_BW_Mackerel)

# Extract linear coefficients
lin <- fit_Herring_lin$coefficients
lin <- rbind(lin,fit_BW_lin$coefficients)
lin <- rbind(lin,fit_Mackerel_lin$coefficients)

# Herring_pred  BW_pred       Mackerel_pred
# 0.0606497897 -0.005805911   0.008048434
#-0.0013641536  0.110024823  -0.004669844
#-0.0008735669  0.002313991   0.024529442
1/lin


