c = 3
d = 4
c*d
library("caTools")
dt<-read.csv("C:\\Users\\Chaithra\\Downloads\\data for the exam0519-20190518 (1)\\Spam.csv")
dt
View(dt)
str(dt)
dt$yesno<-as.factor(dt$yesno)
#dt$yesno<-ifelse(dt$yesno=='yes',1,0)

#set.seed(123)
split<- sample.split(dt,SplitRatio= 0.8)
train<- subset(dt, split=='TRUE')
test<- subset(dt, split=='FALSE')
#model
model <- glm(yesno~.,data=train,family='binomial')
summary(model)
res1<-predict(model,train,type='response')
res2<-predict(model,test,type='response')
print(res2)
#confusion matrix
confmatrix<-table(Actual_value=train$yesno,Predicted_value=res1>0.5)
confmatrix

#Accuracy
acc <- sum(diag(confmatrix))/sum(confmatrix)
acc

#prediction
x<-data.frame(crl.tot=100,dollar =0.001,bang=0.78,money= 0.43,n000=0.00,make=0.20)
p<-predict(model,x)
p


####LDA###########################
library(caTools)
set.seed(123)

library(MASS)
# Fit the model
model <- lda(yesno~., data = train)
model
# Make predictions
x<-data.frame()
predictions <-predict(model, x)
names(predictions)

# Model accuracy
mean(predictions$class==dt$yesno)


#############Decision Trees#############################
library(tidyverse)
library(caret)
library(rpart)
library(caTools)

set.seed(123)
#formulate model
tree<- rpart(yesno~.,data=train)
print(tree)
par(xpd= NA)
plot(tree)
text(tree)

#pruning, removing unwanted extra branches
tree.prune<- prune(tree, cp=0.02)
plot(tree.prune)
text(tree.prune)

#prediction
x<- data.frame( = ,  = 3.0,
               Petal.Length = 5.2, Petal.Width = 2.0)
predict(tree,x, "class")

##############2nd question #############################################

dt <- read.csv("C:\\Users\\Chaithra\\Downloads\\data for the exam0519-20190518 (1)\\USArrests.csv")
View(dt)

model<- lm(Murder~ . , data = dt)
#abline(model)
summary(model)
predict(model,new_data= data.frame( height=c(5,6,10)))

##second question ###########
library("caTools")
min(dt$Murder)
max(dt$Murder)
med <- median(dt$Murder)
dt$Murder <- ifelse(dt$Murder<med,"Below","Above")
dt$Murder <-as.factor(dt$Murder)
set.seed(123)
split<- sample.split(dt,SplitRatio= 0.8)
train<- subset(dt, split=='TRUE')
test<- subset(dt, split=='FALSE')
#model
model<-glm(Murder~.,data=train,family='binomial')
summary(model)
res1<-predict(model,train,type='response')
res2<-predict(model,test,type='response')
print(res2)
#confusion matrix
confmatrix<-table(Actual_value=train$Murder,Predicted_value=res1>0.5)
confmatrix

#Accuracy
accuracy <- sum(diag(confmatrix))/sum(confmatrix)
accuracy


#standardise

scaled_data <- scale(dt)




#################third question###############################

dt <- read.csv("C:\\Users\\Chaithra\\Downloads\\data for the exam0519-20190518 (1)\\students.csv")
View(dt)

#Descriptive Stats 
summary(dt)
boxplot(dt)
cor(dt)

##Model 1
model<- lm(SCORE~ . , data = dt)
#abline(model)
summary(model)


###PREDICT##########

predict(model , data.frame( HOURS = c(35,55), ANXIETY = c(77,15),A_POINTS= c(22,26)))


#model 2

require(partykit)


require(rpart)
# decision tree
cart = rpart(SCORE ~ ., data=dt)
summary(cart)
plot(cart)
text(cart)

##PREDICTION ########

x <- data.frame(HOURS = c(35,55), ANXIETY = c(77,15),A_POINTS= c(22,26))
CART=predict(cart,newdata=x)
CART

################END###########################################