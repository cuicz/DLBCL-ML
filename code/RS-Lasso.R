library(readr)
data<- read.csv("LAS.csv")
View(data)
summary(data)
str(data$TR)
data$TR<-factor(data$TR,levels = c(0,1),labels = c("ER","NER"))
data$TR<-as.factor(data$TR)
library(glmnet)
library(rms)
library(foreign)
library(readr)
library(corrplot)
library(glmnet)
data<-na.omit(data)
corrlate<-cor(as.matrix(data))
corrplot.mixed(corrlate)
X<-as.matrix(data[,2:268])
Y<-as.matrix(data[,1])

lasso<-glmnet(X,Y,alpha=1,family="binomial")
print(lasso)
plot(lasso,xva="lambda",label="TRUE")
lasso.coef<-coef(lasso,s=0.000068)
lasso.coef

cv.lasso<-cv.glmnet(X,Y,alpha=1)
plot(cv.lasso)
abline(v=log(c(cv.lasso$lambda.min,cv.lasso.lse)),lty=2)


cv.lasso$lambda.min
Coefficients <- coef(lasso, s = cv.lasso$lambda.min)
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index
Active.Coefficients
row.names(Coefficients)[Active.Index]
dev$minlassopred <-predict(fit,type="response",newx=x[1:101,],s=cv.lasso$lambda.min)


cv.lasso$lambda.1se
Coefficients <- coef(lasso, s = cv.lasso$lambda.1se)
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index
Active.Coefficients
row.names(Coefficients)[Active.Index]
dev$selassopred <-predict(fit,type="response",newx=x[1:101,],s=cv.fit$lambda.1se)