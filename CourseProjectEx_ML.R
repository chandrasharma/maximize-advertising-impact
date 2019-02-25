# PREDICT 422 Practical Machine Learning

# Course Project - Example R Script File

# OBJECTIVE: A charitable organization wishes to develop a machine learning
# model to improve the cost-effectiveness of their direct marketing campaigns
# to previous donors.

# 1) Develop a classification model using data from the most recent campaign that
# can effectively capture likely donors so that the expected net profit is maximized.

# 2) Develop a prediction model to predict donation amounts for donors - the data
# for this will consist of the records for donors only.

library(ggplot2)

# load the data
charity <- read.csv(file.choose()) # load the "charity.csv" file

# # predictor transformations
# 
charity.t <- charity
# charity.t$avhv <- log(charity.t$avhv)
# hist(charity.t$incm)
# charity.t$incm <- log(charity.t$incm)
# hist(charity.t$incm)
# 
# charity.t$inca <- log(charity.t$inca)
# hist(charity.t$inca)

# add further transformations if desired
# for example, some statistical methods can struggle when predictors are highly skewed



#EDA
qplot()
summary(charity)
data.train$donr <- as.character(data.train$donr)
qplot(inca, data=data.train, fill=donr, xlab = "Avg Family Income in Potential Donor's Neighborhood", main = "Donation by Average Income")
# qplot(incm, data=charity, fill=donr, xlab = "Avg Family Income in Potential Donor's Neighborhood", main = "Donation by Average Income")

data.train$chld <- as.character(data.train$chld)
qplot(damt,hinc, data=data.train, color=chld, xlab = "Amount Donated in Thousand", ylab = "High Income Categories", main = "Donation Amount of High income household by Children")

data.train$genf <- as.character(data.train$genf)
qplot(log(damt), data=data.train, fill=genf, bins=15, xlab = "Log Amount Donated", main = "Distribution of Amount Donated by Gender")

data.train$home <- as.character(data.train$home)
qplot(home, data=data.train, fill=donr, xlab = "Have home or not", main = "Donor or not by Home Owenership")

qplot(avhv, data=data.train, fill=donr, xlab = "Avg Home value in Potential Donor's Neighborhood", main = "Donor or not by Average Home Value")

data.train$donr <- as.character(data.train$donr)
qplot(donr, data=data.train, fill=genf, xlab = "Donated or not",main = "Donated or Not by Gender M(0) and F(1)")

qplot(damt, plow,data=data.train, xlab = "Donated Amount",ylab= "%age Low Income",main = "Donation Amount by low income category")

plot(data.train$npro,data.train$tgif, xlab = "Life Time No of Promotions Recieved", ylab = "Dollar amount of lifetime gifts to date",
     main = "Life Time Dollar Amount by No of Promotions", col="red")

data.train.nonfac <- cbind(data.train[,c(11:21,23)])#collection of all non factor columns
data.train.nonfac.t <- data.train.nonfac
data.train.nonfac.t$damt <- log(data.train.nonfac.t$damt)#taking log of amount donated for better correlation plot

# pairs(charity.nonfac)
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

pairs(data.train.nonfac.t, panel = panel.smooth,
      cex = 1.5, bg = "light blue",
      diag.panel = panel.hist, cex.labels = 2, font.labels = 2)

# plot(charity$npro,charity$lgif, xlab = "Life Time No of Promotions Recieved", ylab = "Dollar amount of lifetime gifts to date",
     # main = "Life Time Dollar Amount by No of Promotions", col="red")

boxplot(data.train.nonfac[,1:5], col = "green",xlab="tgif")

par(mfrow =c(2,2))
boxplot(data.train.nonfac[,6], col = "red",xlab="tgif", main = "Dollar amount of lifetime gifts to date - More  outliers") #tgif most number of outlliers
boxplot(data.train.nonfac[,7], col = "red",xlab="lgif", main = "Dollar amount of largest gift to date - More  outliers") #lgif most number of outlliers
boxplot(data.train.nonfac[,8],col ="red",xlab="rgif", main = "Dollar amount of most recent gift - More outliers")#rgif most number of outlliers
boxplot(data.train.nonfac[,11],col ="red",xlab="agif", main = "Average dollar amount of gifts to date - More outliers")#agif most number of outlliers

boxplot(data.train.nonfac[,9:10],col ="green", main = "Outliers present")


hist(charity$lgif)

# set up data for analysis

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

# predictor transformations

charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)
hist(charity.t$incm, col = "green", xlab = "Median Family Income in $ Thousands", main = "Distribution of Median Family Income")
charity.t$incm <- log(charity.t$incm)
hist(charity.t$incm, col = "green")

charity.t$inca <- log(charity.t$inca)
hist(charity.t$inca)

hist(charity.t$plow)
charity.t$plow <- log(charity.t$plow)
hist(charity.t$plow, col="green",  xlab = "Percent categorized as low income", main = "Percent categorized as low income in potential donor's neighborhood")

hist(charity.t$tgif)
charity.t$tgif <- log(charity.t$tgif)
hist(charity.t$tgif, col="green",  xlab = "Dollar amount of lifetime gifts to date", main = "Histogram of Dollar amount of lifetime gifts to date")


hist(charity.t$lgif)
charity.t$lgif <- log(charity.t$lgif)
hist(charity.t$lgif, col="green",  xlab = "Dollar amount of largest gift to date", main = "Histogram of Dollar amount of largest gift to date")

hist(charity.t$rgif)
charity.t$rgif <- log(charity.t$rgif)
hist(charity.t$rgif, col="green",  xlab = "Dollar amount of most recent gift", main = "Histogram of Dollar amount of most recent gift")


hist(charity.t$tdon)
charity.t$tdon <- log(charity.t$tdon)
hist(charity.t$tdon, col="green",  xlab = "Number of months since last donation", main = "Histogram of Number of months since last donation")


# hist(charity.t$tlag)
# charity.t$tlag <- exp(charity.t$tlag)
# hist(charity.t$tlag)

hist(charity.t$agif)
charity.t$agif <- log(charity.t$agif)
hist(charity.t$agif)

##### CLASSIFICATION MODELING ######
########### LDA ######################################################################################################

# linear discriminant analysis

library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

# model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
#                     avhv + incm + inca + plow + npro + log(tgif) + log(lgif) + log(rgif) + log(tdon) + tlag + agif, 
#                   data.train.std.c) 

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

lda.pred <- predict(model.lda1, data.valid.std.c)


post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs. 2nd coulmn under posterior has probabilities for those who donate

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329
# check profit = 14.5*985-2*1329 = 11624.5

##Error rates - LDA ######
mean(chat.valid.lda1==data.train.std.c$donr) #train error rate
# 0.503012
mean(chat.valid.lda1==c.valid) #test error rate
# 0.8225966

post.test.lda <- predict(model.lda1, data.test.std, type="prob") # post probs for test data

########### GLM ######################################################################################################

# logistic regression
# 
# model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
#                     avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
#                   data.train.std.c, family=binomial("logit"))

model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                                        avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                                      data.train.std.c, family=binomial("logit"))

############### Cross validation ##################
# library (boot)
#check for plow variable
cv.error.plow.10= rep (0 ,10)
for (i in 1:10) {
  glm.fit=glm(donr~poly(plow ,i),data=data.train.std.c)
  cv.error.plow.10[i]=cv.glm (data.train.std.c ,glm.fit ,K=10) $delta [1]
  }
cv.error.plow.10
#no plolynomials required

#check for incm variable
cv.error.incm.10= rep (0 ,10)
for (i in 1:8) {
  glm.fit=glm(donr~poly(incm ,i),data=data.train.std.c)
  cv.error.incm.10[i]=cv.glm (data.train.std.c ,glm.fit ,K=8) $delta [1]
}
cv.error.incm.10

#check for wrat variable
cv.error.wrat.10= rep (0 ,10)
for (i in 1:8) {
  glm.fit=glm(donr~poly(wrat ,i),data=data.train.std.c)
  cv.error.wrat.10[i]=cv.glm (data.train.std.c ,glm.fit ,K=8) $delta [1]
}
cv.error.wrat.10

cv.error.npro.10= rep (0 ,10)
for (i in 1:8) {
  glm.fit=glm(donr~poly(npro ,i),data=data.train.std.c)
  cv.error.npro.10[i]=cv.glm (data.train.std.c ,glm.fit ,K=8) $delta [1]
}
cv.error.npro.10

cv.error.rgif.10= rep (0 ,10)
for (i in 1:8) {
  glm.fit=glm(donr~poly(rgif ,i),data=data.train.std.c)
  cv.error.rgif.10[i]=cv.glm (data.train.std.c ,glm.fit ,K=8) $delta [1]
}
cv.error.rgif.10

cv.error.agif.10= rep (0 ,10)
for (i in 1:8) {
  glm.fit=glm(donr~poly(agif ,i),data=data.train.std.c)
  cv.error.agif.10[i]=cv.glm (data.train.std.c ,glm.fit ,K=8) $delta [1]
}
cv.error.agif.10

# #basis function
# library (splines)
# model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
#                     bs (avhv,knots =c(25 ,40 ,60)) + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
#                   data.train.std.c, family=binomial("logit"))
# 
# model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
#                     bs (avhv,df=16) + bs (incm,df=16) + bs(inca, df=16) + ns(plow, df=16) + bs(npro,df=16) + tgif + bs(lgif,df=16) + 
#                     rgif + tdon + tlag + agif, 
#                   data.train.std.c, family=binomial("logit"))
# #gam
# library(gam)
# model.log1 <- gam(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat +
#                     bs (avhv,df=16) + bs (incm,df=16) + bs(inca, df=16) + ns(plow, df=16) + bs(npro,df=16) + tgif + bs(lgif,df=16) +
#                     rgif + tdon + tlag + agif,
#                   data.train.std.c, family=binomial("logit"))

# model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + I(hinc^3) + genf + wrat + 
#                     avhv + incm + inca + plow + npro +I(npro^2) + tgif + lgif + rgif + tdon + tlag + agif, 
#                   data.train.std.c, family=binomial("logit"))
# taking log did not help
# model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
#                     avhv + incm+log(plow) + npro + log(tgif) + log(lgif) + log(rgif) + log(tdon) + tlag + agif, 
#                   data.train.std.c, family=binomial("logit"))

# knn.pred=knn (train .X,test.X,train .Y,k=1)
# > mean(test.Y!= knn.pred)
# [1] 0.118
# > mean(test.Y!=" No")


post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response")
# post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)

plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit




# 1291.0 11642.5
cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) 

##Error rates - GLM ######
mean(chat.valid.log1==data.train.std.c$donr) #train error rate
# 0.50251
mean(chat.valid.log1==c.valid) #test error rate
  # 0.8374628

# classification table
#               c.valid
#chat.valid.log1   0   1
#              0 709  18
#              1 310 981
# check n.mail.valid = 310+981 = 1291
# check profit = 14.5*981-2*1291 = 11642.5

# Results

# n.mail Profit  Model
# 1329   11624.5 LDA1
# 1291   11642.5 Log1

# select model.log1 since it has maximum profit in the validation sample
post.test.log <- predict(model.log1, data.test.std, type="response") # post probs for test data
###insert
###########caret############
library(caret)
library(mlbench)
library(aod)
library(ggplot2)
library(pROC)
library(lubridate)
library(grDevices)
require(miscTools)
library(lubridate)

# set up data for analysis

charity <- read.csv(file.choose()) # load the "charity.csv" file

dim(charity)
str(charity)

# predictor transformations
hist(charity$avhv)
hist(log((charity$avhv)))

charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)
# add further transformations if desired
# for example, some statistical methods can struggle when predictors are highly skewed

# set up data for analysis

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

#EDA
cor(data.train[,-c(1,24)])

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

# objControl <- trainControl(method='repeatedcv', number=3, classProbs = TRUE)
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary,
                           savePredictions = T)

nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))


data.train.std.c$donr <- ifelse(data.train.std.c$donr=='1','yes','no')
data.train.std.c$donr <- as.factor(data.train.std.c$donr)

tic <- Sys.time()

fit.nnet<- train(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv + incm + inca + plow + npro + I(npro^2) + tgif + lgif + rgif + tdon + tlag + agif,
                 data=data.train.std.c, method="nnet", metric="ROC",trControl = fitControl,tuneGrid = nnetGrid,verbose = FALSE)

toc <- Sys.time()
time <- toc-tic
time
#Time difference of 37.92295 mins

###15 layers ####
fitControl15 <- trainControl(method = "repeatedcv", 
                             number = 15, 
                             repeats = 5, 
                             classProbs = TRUE, 
                             summaryFunction = twoClassSummary,
                             savePredictions = T)

nnetGrid15 <-  expand.grid(size = seq(from = 1, to = 15, by = 1),
                           decay = seq(from = 0.1, to = 0.5, by = 0.1))


data.train.std.c$donr <- ifelse(data.train.std.c$donr=='1','yes','no')
data.train.std.c$donr <- as.factor(data.train.std.c$donr)

tic <- Sys.time()

fit.nnet15<- train(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                     avhv + incm + inca + plow + npro + I(npro^2) + tgif + lgif + rgif + tdon + tlag + agif,
                   data=data.train.std.c, method="nnet", metric="ROC",trControl = fitControl,tuneGrid = nnetGrid,
                   verbose = FALSE)



toc <- Sys.time()
time <- toc-tic
time

post.valid.nnet <- predict(fit.nnet, data.valid.std.c, type="prob") 
profit.nnet <- cumsum(14.5*c.valid[order(post.valid.nnet[,2], decreasing=T)]-2)

plot(profit.nnet) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.nnet) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.nnet)) # report number of mailings and maximum profit

# 1203.0 11789.5 10 hidden layers input
# 1314.0 11799.5 15 hidden layers as input


#Plotting Variable importance for Neural Network
plot(varImp(object=fit.nnet),main="NNET - Variable Importance")

cutoff.nnet <- sort(post.valid.nnet[,2], decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.nnet <- ifelse(post.valid.nnet>cutoff.nnet, 1, 0) # mail to everyone above the cutoff
table(chat.valid.nnet[,2], c.valid) # classification table

#     c.valid
#     0   1
# 0 700   4
# 1 319 995


#               c.valid
#chat.valid.log1   0   1
#              0 709  18
#              1 310 981
# check n.mail.valid = 310+981 = 1291
# check profit = 14.5*981-2*1291 = 11642.5
##Error rates - LDA ######
mean(chat.valid.nnet==data.train.std.c$donr) #train erro rate
# 0.5037166 10 hidden layers
# 0.5091675 15 hidden layers
mean(chat.valid.nnet==c.valid) #test error rate
# 0.5056987
# 0.5106541 15 hidden layer as input.
post.test.nnet <- predict(fit.nnet, data.test.std, type="prob") # post probs for test data
#####
confusionMatrix(fit.nnet) #gives average model accuracy

# Plot the neural nets
# https://beckmw.wordpress.com/2013/11/14/visualizing-neural-networks-in-r-update/
#import the function from Github
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(fit.nnet)

# #get AUC value
# predictions.nnet.insample <- predict(fit.nnet, newdata=data.valid.std.c,type='raw')
# valid.label.name <- ifelse(data.valid.std.c$donr=='1','yes','no')
# auc <- roc(valid.label.name, predictions.nnet.insample) 

#get AUC value
max(fit.nnet15$results[,"ROC"])
# 0.9630187
# 0.9632212 15 hidden layers as input 

###Tree based ######

library (gbm)
library(mlbench)
library(caret)
# tree.carseats =tree(High∼.-Sales ,Carseats )

data.train.std.c$donr <- ifelse(data.train.std.c$donr=='1','yes','no')
data.train.std.c$donr <- as.factor(data.train.std.c$donr)

fit.tree<- gbm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv + incm + inca + plow + npro + I(npro^2) + tgif + lgif + rgif + tdon + tlag + agif,
                 data=data.train.std.c, 
               distribution="bernoulli",n.trees =3000, interaction.depth =1, shrinkage =0.01,verbose =F)

post.valid.tree <- predict(fit.tree, data.valid.std.c,n.trees =3000) 
profit.tree <- cumsum(14.5*c.valid[order(post.valid.tree, decreasing=T)]-2)

plot(profit.tree) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.tree) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.tree)) # report number of mailings and maximum profit

# [1]  1234 11887 #tree calssification  table
# [1]  1216 11894 #second time 
# [1]  1229.0 11882.5 #third time
# [1]  1231 11893 #fourth time

# plot(fit.tree )
# text(fit.tree ,pretty =0)
summary(fit.tree)
varImp(fit.tree,n.trees =3000)
par(mfrow =c(1,2))
plot(fit.tree ,i="chld")
plot(fit.tree,i="hinc")
  
cutoff.tree <- sort(post.valid.tree, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.tree <- ifelse(post.valid.log1>cutoff.tree, 1, 0) # mail to everyone above the cutoff
table(chat.valid.tree, c.valid) 

#                   c.valid
# chat.valid.tree   0   1
                # 0 993 472
                # 1  26 527


##Error rates - GBM ######
mean(fit.tree$train.error) #train erro rate
# 0.1073687
mean(chat.valid.nnet==c.valid) #test error rate
# 0.5056987

post.test <- predict(fit.tree, data.test.std, n.trees =3000, type="response") # post probs for test data

# confusionMatrix(fit.tree)
# auc.nnet <- roc(c.valid, post.valid.nnet[,1])


################# SVM #################################################################################
library (e1071)
# library(libsvm)
data.train.std.c$donr <- ifelse(data.train.std.c$donr=='1','yes','no')
data.train.std.c$donr <- as.factor(data.train.std.c$donr)

# tune.out=tune(svm ,donr ~.,data=data.train.std.c ,kernel ="linear",
#               ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))

tune.out=tune(svm ,donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat +
                 avhv + incm + inca + plow + npro + I(npro^2) + tgif + lgif + rgif + tdon + tlag + agif,
               data=data.train.std.c,kernel ="linear",probability=TRUE,
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))

summary(tune.out)

bestmod =tune.out$best.model
summary (bestmod)

#for the sake of plotting
fit.svm <- svm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat +
                 avhv + incm + inca + plow + npro + I(npro^2) + tgif + lgif + rgif + tdon + tlag + agif,
               data=data.train.std.c,kernel ="linear", cost=5, gamma=0.05, probability=TRUE)

post.valid.svm <- predict(fit.svm, data.valid.std.c, probability=TRUE) 
profit.svm <- cumsum(14.5*c.valid[order(post.valid.svm, decreasing=T)]-2)


plot(profit.svm, data= data.train.std.c) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.svm) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svm)) # report number of mailings and maximum profit

# [1]  1053 10973
# [1]  1047 10956
# [1]  1040 10912
# https://stackoverflow.com/questions/7782501/how-to-interpret-predict-result-of-svm-in-r
# https://stat.ethz.ch/pipermail/r-help/2006-February/088476.html


cutoff.svm <- sort((attr(post.valid.svm,"prob"))[,2], decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.svm <- ifelse((attr(post.valid.svm,"prob"))>cutoff.svm, 1, 0) # mail to everyone above the cutoff
table(chat.valid.svm[,2], c.valid)

# c.valid
#   0   1
# 0 875 103
# 1 144 896

# plot(tune.out$best.model, data.train.std.c)
##Error rates - SVM ######
mean(post.valid.svm==data.train.std.c$donr) #train erro rate
# 0.5077811
mean(chat.valid.svm==c.valid) #test error rate
# 0.4997522

post.test <- predict(fit.tree, data.test.std, n.trees =3000, type="response") # post probs for test data

library (ROCR)
rocplot =function (pred , truth , ...){
  predob = prediction (pred , truth )
  perf = performance (predob , "tpr ", "fpr ")
  plot(perf ,...)}

fitted =attributes (predict (fit.svm,data.train.std.c, decision.values =TRUE))$decision.values
rocplot (fitted ,data.valid.std.c, main=" Training Data")
############################################################################################################

##### PREDICTION MODELING ######

# Least squares regression

model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

summary(model.ls1)

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
lm.obs <- cbind(pred.valid.ls1, data.valid.std.y$damt)
plot(lm.obs, xlab="Observed", ylab="Predicted", main="Observed versus Predicted")
# abline(model.ls1)

plot(model.ls1)

mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.867523
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1696615

# drop wrat for illustrative purposes
model.ls2 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls2)^2) # mean prediction error
# 1.867433
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error
# 0.1696498

# Results

# MPE  Model
# 1.867523 LS1
# 1.867433 LS2

# select model.ls2 since it has minimum mean prediction error in the validation sample

yhat.test <- predict(model.ls2, newdata = data.test.std) # test predictions g

## gam regression###
library(gam)
library(splines)

model.gam.reg <- gam(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat +
                    bs (avhv,df=16) + bs (incm,df=16) + bs(inca, df=16) + ns(plow, df=16) + bs(npro,df=16) + tgif + bs(lgif,df=16) +
                    rgif + tdon + tlag + agif, family = "gaussian", data= data.train.std.y)

summary(model.gam.reg)

pred.valid.gam.reg <- predict(model.gam.reg, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.gam.reg)^2) # mean prediction error
# 1.566529
sd((y.valid - pred.valid.gam.reg)^2)/sqrt(n.valid.y) # std error
# 0.1634219

yhat.test <- predict(model.gam.reg, newdata = data.test.std) # test predictions 
##### Neural net regression ##########
library(caret)
fitControl.reg <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5)

nnetGrid.reg <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))

tic <- Sys.time()

fit.nnet.reg<- train(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv + incm + inca + plow + npro + I(npro^2) + tgif + lgif + rgif + tdon + tlag + agif,
                 data=data.train.std.y, method="nnet", linout=TRUE,metric="RMSE",trControl = fitControl.reg,tuneGrid = nnetGrid.reg,verbose = FALSE)

toc <- Sys.time()
time <- toc-tic
time
# Time difference of 29.75292 mins
getTrainPerf(fit.nnet.reg)
pred.valid.nnet.reg <- predict(fit.nnet.reg, newdata = data.valid.std.y) # validation predictions

mean((y.valid - pred.valid.nnet.reg)^2) # mean prediction error
# [1] 1.674155
sd((y.valid - pred.valid.nnet.reg)^2)/sqrt(n.valid.y) # std error
# [1] 0.1811982

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(fit.nnet.reg)

##### regression trees ##########


fit.tree.reg<- gbm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                       avhv + incm + inca + plow + npro + I(npro^2) + tgif + lgif + rgif + tdon + tlag + agif,
                     data=data.train.std.y, distribution=
                      "gaussian",n.trees =5000 , interaction.depth =4, shrinkage =0.2,verbose =F)

summary(fit.tree.reg)
pred.valid.tree.reg <- predict(fit.tree.reg, newdata = data.valid.std.y,n.trees =5000) # validation predictions
mean((y.valid - pred.valid.tree.reg)^2) # mean prediction error
# 1.95497
sd((y.valid - pred.valid.tree.reg)^2)/sqrt(n.valid.y) # std error
# 0.1837006

##### Support vector machines ##########
library (e1071)
tune.out.reg=tune(svm ,damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat +
                avhv + incm + inca + plow + npro + I(npro^2) + tgif + lgif + rgif + tdon + tlag + agif,
              data=data.train.std.y,kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))

summary(tune.out.reg)

bestmod.reg =tune.out.reg$best.model
summary (bestmod.reg)

svm.reg <- svm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat +
              avhv + incm + inca + plow + npro + I(npro^2) + tgif + lgif + rgif + tdon + tlag + agif,
              data=data.train.std.y,kernel ="linear", cost= 0.01, gamma=0.05)


pred.svm.reg <- predict(svm.reg, newdata=as.data.frame(data.valid.std.y)) # validation predictions

#if predeict returns train response then run predict on a working predict output, this has corrected in svm 
#regression prediction
mean((y.valid - pred.gam.reg)^2) # mean prediction error
# [1] 1.864415
sd((y.valid - pred.svm.reg)^2)/sqrt(n.valid.y) # std error
# [1] 0.1776259

# Oversampling adjustment for calculating number of mailings for test set
n.mail.valid <- which.max(profit.nnet)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#came with the code
#    0    1 
# 1676  331
#neural net (10 hidden layers) predictions
# 0    1 
# 1687  320 
# based on this model we'll mail to the 331 highest posterior probabilities

# See below for saving chat.test into a file for submission

# FINAL RESULTS

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
# write.csv(ip, file="ABC.csv", row.names=FALSE) # use your initials for the file name
write.csv(ip, file="CMS.csv", row.names=FALSE) # use your initials for the file name

# submit the csv file in Angel for evaluation based on actual test donr and damt values
