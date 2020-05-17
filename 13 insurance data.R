
data<- read.csv(file.choose()) #importing file
data$region<-NULL #deleting region column
names(data) #header name
str(data) #structure of dataset
data$age<-as.numeric(data$age) #converting to numeric
data$children<-as.numeric(data$children) #converting to numeric
table(data$smoker) 
table(data$sex)
data$smoker_flag<- ifelse(data$smoker=='yes',1,2)
data$sex_flag<-ifelse(data$sex =='male',2,1)
data$smoker_flag<-as.numeric(data$smoker_flag) #converting to numeric 
data$sex_flag<-as.numeric(data$sex_flag)
data$Month<-as.numeric(data$Month)
str(data)
#to check outlier
boxplot(data$age)
boxplot(data$bmi)
boxplot(data$children)
boxplot(data$charges)
boxplot(data$Month)
#treatment of outlier for bmi
summary(data$bmi)
upper<-34.69+1.5*IQR(data$bmi);upper #q3+1.5*IQR
data$bmi[data$bmi >upper] <-upper #any value in data$bmi >upper then replace that with upper value. 
boxplot(data$bmi)
summary(data$bmi)
#treatment for outliers for charges
summary(data$charges)
upper<-16640+1.5*IQR(data$charges);upper #q3+1.5*IQR
data$charges[data$charges>upper]<-upper #any value in data$charges>upper, replace that with upper value
boxplot(data$charges)
summary(data$charges)
#data subset
abc<- data[,-c(2,5)] #deleting 2 columns(2nd and 5th) from dataset
#data partition
library(caret)############error getting couldnot find function createdatapartition
Train<-createDataPartition(abc$charges, p=0.70, list = FALSE) #we are taking 70% of data from charges column and making it in column format. if list=TRUe, then it would have been in row format
#here we are assigning all to Train 
#we are doing data partition in charges as it is y 


training<-abc[Train, ] #in abc, whichever columns are matching Train, that will be assigned to training

testing<- abc[-Train,] #in abc, whichever columns not matching with Train, that will be assigned to testing

#model building and may lead to overfitting
cor(training)##########doubt how to see correlation from output
model<-lm(charges~.,data=training) #here ~. means y variable(charges) depend on all x variable
summary(model) #here we are getting accuracy 1 i.e. 100%. that means our data partition is wrong. data is going for overfitting
library(car)
vif(model) 
par(mfrow=c(2,2))
plot(model)
#model2

hist(training$charges) #hist is histogram (here we found right skewed data)

#data transformation (important for interview)
#transformation helps to increase accuracy and get better model
hist(1/training$charges) #1/training$charges means reciprocal 
hist(log(training$charges)) #log transformation to make data distribution normal shaped 
#1/data and log transformation used for same thing only different way
model2<-step(lm(log(charges)~., data=training),direction="backward")
summary(model2) #accuracy =91% from output

#multicolinearity
vif(model2) #vif helps to remove multicolinearity
#assumption
par(mfrow=c(2,2))
plot(model2)
library(lmtest)
dwtest(model2)
library(car)
ncvTest(model2)
#prediction
################doubt why testing data not traing data
testing$fitted<-predict(model2, testing) #we are predicting the b0 and b1 in model testing data using formula y=b0+b1x1+b2x2 and so on

testing$original<-exp(testing$fitted) #exp means exponential i.e. antilog 
#we are using exp or antilog as we have got the output in log format bcz we have applied log for transformation of data to make it normal distributed
#so to convert the log format to normal format we are using antilog or exp
training$new_charges<-log(training$charges)
