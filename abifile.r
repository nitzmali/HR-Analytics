install.packages('ggplot2')
install.packages("RColorBrewer")
install.packages('plyr')
install.packages('rpart.plot')
install.packages('caTools')
install.packages("gplots")
install.packages("randomForest")
install.packages("caret")
install.packages("e1071")
library('caTools')
library('rpart.plot')
library('rpart')
library('plyr')
library("ggplot2")
#library("reshape2")
library("RColorBrewer")
rm(list=ls())  #make your space Empty
mydata = read.csv("humanresource.csv", header = TRUE)
str(mydata)
sum(is.na(mydata))
summary(mydata)


table(mydata$perf)
hist(mydata$perf,col = "aquamarine3", main = "Distribution of performance ratings",
     xlab = "Performance rating",ylab = "# Employees")

#performanceratingvsproportionleaving
agg_sex=aggregate(vol_leave~sex,data = mydata,mean)
print(agg_sex)
ggplot(agg_sex, aes(x=sex,y=vol_leave)) + geom_bar(stat = "identity",fill = "aquamarine3", colour = "yellow")+ ggtitle("Voluntary Termination rate by sex")+
  labs(y="Proportion Leaving",x="Performance rating")

#businessareavsproportionleaving
agg_area=aggregate(vol_leave~area,data = mydata,mean)
print(agg_area)
ggplot(agg_area, aes(x=area,y=vol_leave,fill = area)) + geom_bar(stat = "identity", colour = "black")+ggtitle("Voluntary Termination rate by Business area")+
  labs(y="Proportion Leaving",x="Business Area")

#areaandsexvsproportionleaving
agg_as=aggregate(vol_leave~area+sex,data = mydata,mean)
print(agg_as)
ggplot(agg_as,aes(x=area,sex,y=vol_leave))+geom_bar(aes(fill = sex), stat = "identity", colour = "black", position = position_dodge()) + 
  ggtitle("Voluntary Leaving Rate by Area & Sex") + labs(y = "Proportion Leaving", x = "Business Area") 

#analyzing the age of employees
hist(mydata$age,breaks=100,col="aquamarine3",main="Age Distribution",border=F,xlab="Age")

#ANALYZING THE ROLES OF THE EMPLOYEES TO GET A BETTER IDEA OF THE AGE DISTRIBUTION
mydata$role<-factor(mydata$role,levels=c("Ind","Manager","Director"))
boxplot(age~role,data=mydata,col="aquamarine3",ylab="Age")

#Logarithmicage
mydata$log_age = log(mydata$age)
summary(mydata$log_age)

#ANALYZING VOLUNTARY TERMINATION BY AGE AND ROLE
#before that segmenting age
age_agg = aggregate(x = mydata$vol_leave, by = list(cut(mydata$age, 10)), mean)
age_agg

names(age_agg) = c("Age", "Probability")

ggplot(age_agg, aes(x = Age, y = Probability, fill = Age)) + geom_bar(stat = "identity", width = .7, colour = 'black') +
  scale_fill_brewer() + ggtitle("Voluntary Leaving Rate by Role and Age") + labs(y = "Proportion Leaving", x = "Age Cut") 


#analyzing the salary pattern
summary(mydata$salary)
hist(mydata$salary,breaks=50,col="aquamarine3",main="Salary Distribution",xlab="Salary")

#FINDING THE SALARY DISTRIBUTION BASED ON QUANTILE
quantile(mydata$salary,probs=seq(0,1,.2))

#PLOTTING THE BOXPLOT TO SHOW SALARY DISTRIBUTION BASED ON ROLES
boxplot(salary~role,data=mydata,col="aquamarine3",main="Salary")


#Data Modelling
set.seed(42)
spl<-sample.split(mydata$vol_leave,2/3)
train<-mydata[spl,]
test<-mydata[!spl,]
#1.Logistic regression
test_mean = mean(test$vol_leave)
train_mean = mean(train$vol_leave)

print(c(test_mean, train_mean))


#fit the model
model1<-glm(vol_leave~perf+role+log_age+sex+area+salary,data=train,family = "binomial")

summary(model1)
anova(model1,test="Chisq")



####Assessing the predictive ability of the model
fitted.results = predict(model1, test, type = 'response')
fitted.results = ifelse(fitted.results > 0.5,1,0)


# Confusion Matrix
confMat=table(actual = test$vol_leave, prediction = fitted.results)

accuracy = sum(diag(confMat))/sum(confMat)
accuracy


####Roc curve

p <- predict(model1,test, type = "response")
pr = prediction(p, test$vol_leave)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
#plot(prf)

####Auc Curve
#auc = performance(pr, measure = "auc")
#auc = auc@y.values[[1]]
#auc

####2.Decision tree
set.seed(42)
fit<-rpart(vol_leave~role+age+sex+area+perf,data=train,method = "class")
fit


par(mar = c(5,4,1,2))
rpart.plot(fit, sub = NULL, main = "Basic Decision Tree")

###Confusion Matrix
t_pred = predict(fit, test, type = 'class')
confMat = table(actual = test$vol_leave, prediction = t_pred)
confMat
##Accuracy
accuracy = sum(diag(confMat))/sum(confMat)

accuracy

p <- predict(fit,test, type = "prob")

##naivebyes
library("e1071")
modelnaive <- naiveBayes(vol_leave~perf+role+log_age+sex+area+salary, data = mydata)
predict(modelnaive, mydata[1:10,-1])
predict(modelnaive, mydata[1:10,-1], type = "raw")
pred <- predict(modelnaive, mydata[,1])
pred
table(pred, mydata$vol_leave)
class(modelnaive)
summary(modelnaive)
print(modelnaive)
preds <- predict(modelnaive, newdata = mydata)
preds
conf_matrix <- table(preds, mydata)

