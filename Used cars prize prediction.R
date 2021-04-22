#load the file
train <- read.csv("C:/Users/phoeb/Desktop/ALY6020 Final Project/train-data.csv",header = TRUE)
pred <- read.csv("C:/Users/phoeb/Desktop/ALY6020 Final Project/test-data.csv",header = TRUE)

# data cleaning
train1 <- train[,c(-1,-2,-13)]
train1
train1[!complete.cases(train1),]
sum(is.na(train1))
dim(train1)
newtrain <- na.omit(train1)
dim(newtrain)
newtrain[!complete.cases(newtrain),]
newtrain$Seats <- as.factor(newtrain$Seats)
sum(is.na(newtrain))
# overview of the dataset
library(ggplot2)
library(dplyr)
library(ggthemems)
str(train)
#Price
plot.price <- ggplot(newtrain,aes(x=Price))+
  geom_histogram(aes(y =..density..),colour="black",fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  labs(title = "The overview of the Price")+
  xlab("Price (Lakh)")
plot.price

# location vs price
Location <- newtrain
x_order <- Location %>%
  group_by(Location) %>%
  summarize(mean_y=mean(Price))%>%
  ungroup()%>%
  arrange(desc(mean_y))%>%
  select(Location)
Location$Location<-factor(Location$Location,levels=as.factor(x_order$Location),ordered = TRUE)
plot.location<- ggplot(Location,aes(x = Location,y=Price,colour =Location))+
  geom_boxplot(outlier.colour="red")+
  labs(title = "Used cars price from different area")
plot.location

# Year vs Price
Year <- newtrain
x_order <- Year %>%
  group_by(Year) %>%
  summarize(mean_y=mean(Price))%>%
  ungroup()%>%
  arrange(desc(mean_y))%>%
  select(Year)
Year$Year<-factor(Year$Year,levels=as.factor(x_order$Year),ordered = TRUE)
plot.Year<- ggplot(Year,aes(x = Year,y=Price,colour =Year))+
  geom_boxplot(outlier.colour="red")+
  labs(title = "Used cars price from different year")+
  ylab("Price (Lakh)")
plot.Year

# Kilometer_Driven vs price

xlimit <- quantile(newtrain$Kilometers_Driven,0.75)
plot.Kilometers_Driven<- ggplot(newtrain,aes(x = Kilometers_Driven,y=Price,colour =Kilometers_Driven))+
  geom_point(shape=19)+
  labs(title = "Used cars price from different Kilometers_Driven")+
  xlim(0,xlimit)+
  ylim(0,max(train$Price))+
  ylab("Price (Lakh)")
plot.Kilometers_Driven

# Fuel_Type vs Price
Fuel_Type <- newtrain
x_order <- Fuel_Type %>%
  group_by(Fuel_Type) %>%
  summarize(mean_y=mean(Price))%>%
  ungroup()%>%
  arrange(desc(mean_y))%>%
  select(Fuel_Type)
Fuel_Type$Fuel_Type<-factor(Fuel_Type$Fuel_Type,levels=as.factor(x_order$Fuel_Type),ordered = TRUE)
plot.Fuel_Type<- ggplot(Fuel_Type,aes(x = Fuel_Type,y=Price,colour =Fuel_Type))+
  geom_boxplot(outlier.colour="red")+
  labs(title = "Used cars price from different Fuel_Type")+
  ylab("Price (Lakh)")
plot.Fuel_Type

# Transmission
Transmission <- newtrain
x_order <- Transmission %>%
  group_by(Transmission) %>%
  summarize(mean_y=mean(Price))%>%
  ungroup()%>%
  arrange(desc(mean_y))%>%
  select(Transmission)
Transmission$Transmission<-factor(Transmission$Transmission,levels=as.factor(x_order$Transmission),ordered = TRUE)
plot.Transmission<- ggplot(Transmission,aes(x = Transmission,y=Price,colour =Transmission))+
  geom_boxplot(outlier.colour="red")+
  labs(title = "Used cars price from different Transmission")+
  ylab("Price (Lakh)")
plot.Transmission

#Owner_Type
Owner_Type <- newtrain
x_order <- Owner_Type %>%
  group_by(Owner_Type) %>%
  summarize(mean_y=mean(Price))%>%
  ungroup()%>%
  arrange(desc(mean_y))%>%
  select(Owner_Type)
Owner_Type$Owner_Type<-factor(Owner_Type$Owner_Type,levels=as.factor(x_order$Owner_Type),ordered = TRUE)
plot.Owner_Type<- ggplot(Owner_Type,aes(x = Owner_Type,y=Price,colour =Owner_Type))+
  geom_boxplot(outlier.colour="red")+
  labs(title = "Used cars price from different Owner_Type")+
  ylab("Price (Lakh)")
plot.Owner_Type

#Mileage & Eengine & Power


#seats vs prize
Seats <- newtrain
x_order <- Seats %>%
  group_by(Seats) %>%
  summarize(mean_y=mean(Price))%>%
  ungroup()%>%
  arrange(desc(mean_y))%>%
  select(Seats)
Seats$Seats<-factor(Seats$Seats,levels=as.factor(x_order$Seats),ordered = TRUE)
plot.Seats<- ggplot(Seats,aes(x = Seats,y=Price,colour =Seats))+
  geom_boxplot(outlier.colour="red")+
  labs(title = "Used cars price from different Seats")+
  ylab("Price (Lakh)")
plot.Seats



# set train and teest data set (80% as train data set)
set.seed(1234)
nn=0.8
data=newtrain
length(train[,1])
sub<-sample(1:nrow(data),round(nrow(data)*nn))
length(sub)
data_train<-data[sub,]
data_test<-data[-sub,]
dim(data_train)
dim(data_test)

#knn model
library(kknn)
knn <-kknn(Price~.,data_train,data_test[,-11],k=1) 
fit <- fitted(knn)
fit
data_test$Price
knn.correct <- 1-mean(abs(fit-data_test$Price)/data_test$Price)
knn.correct
correct <- rep(0,22,step())
list <- seq(1,21,2)
list
for(i in list){
    knn <- kknn(Price~.,data_train,data_test[,-11],k=i)
    fit <- fitted(knn)
    correct[i] = 1-mean(abs(fit-data_test$Price)/data_test$Price)
    
}
a <- matrix(correct)
a <- na.omit(a)
k <- seq(1,21,2)
a <- cbind(k,a)
a
colnames(a) <- c("k-value","correction")
knn_correct <- a
knn_correct

# logistic regression
library(tidyverse)
library(caret)
library(nnet)
# change Price into low and high
log <- newtrain
mean <- mean(log$Price)
log$Price[which(log$Price < mean)] <- "low"
log$Price <- as.numeric(log$Price)
log$Price[which(log$Price > mean)] <- "high"
log$Price[is.na(log$Price)] <- "low"
log$Price <- as.factor(log$Price)
log$Price

set.seed(1234)
nn=0.8
data=log
length(log[,1])
sub<-sample(1:nrow(data),round(nrow(data)*nn))
length(sub)
log_train<-data[sub,]
log_test<-data[-sub,]
dim(log_train)
dim(log_test)
str(log_train)

model_1 <- glm(Price~.,data = log_train,family = binomial(link = "logit"))
summary(model_1)
model_2 <- glm(Price~Location+Year+Fuel_Type+Kilometers_Driven+Transmission+Owner_Type+Seats, data= log_train,family = binomial(link = "logit"))
summary(model_2)
model_3 <- glm(Price~Transmission+Year,data =log_train,family = binomial(link = "logit") )
summary(model_3)

prob <- predict(model_3,log_test,type="response")
logit.pred <- factor(prob>.5,levels=c(FALSE,TRUE),labels=c("high","low"))
logit.perf <- table(log_test$Price,logit.pred,dnn=c("Actual","Predicted"))
logit.perf
correction <- (206+825)/(206+825+78+85)
correction

##multinomial
library(nnet)
mul <- multinom(Price~Location+Transmission+Year,data= data_train,MaxNWts=17066)
mul.pred <- predict(mul,data_test,type ="response")
mul.correct <- 1-mean(abs(mul.pred-data_test$Price)/data_test$Price)
mul.correct
methods(predict)
##???


## Random Forest model
library(randomForest)
n <- length(col(log_train))
rf <- randomForest(Price~Location+Year+Fuel_Type+Kilometers_Driven+Transmission+Owner_Type+Seats,data=log_train,ntree=500)
plot(rf)
rf <- randomForest(Price~Location+Year+Fuel_Type+Kilometers_Driven+Transmission+Owner_Type+Seats,data=log_train,ntree=200)
importance <- importance(x=rf)
importance
pred <- predict(rf,log_test)
pref <- table(log_test$Price, pred, dnn=c("Actual","Predicted"))
pref
rf.correction <- (213+861)/(213+79+42+861)
rf.correction

rf1 <- randomForest(Price~Location+Year+Fuel_Type+Kilometers_Driven+Transmission+Owner_Type+Seats,data=data_train,ntree=500)
rf1.pred <- predict(rf1,data_test)
rf1.correct <- 1-mean(abs(rf1.pred-data_test$Price)/data_test$Price)
rf1.correct

rf2 <- randomForest(Price~Location+Transmission+Year,data=data_train,ntree=500)
rf2.pred <- predict(rf2,data_test)
rf2.correct <- 1-mean(abs(rf2.pred-data_test$Price)/data_test$Price)
rf2.correct