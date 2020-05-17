#multiple linear regression
#problem statement: 
#objective: the goal of the model is to establish the relationship between "mpg" as a response variable with "disp", "hp", "wt" as predictor variables.
# consider dataset "mtcars" available in the R environment

#it gives comparison between different car models in terms of

#mpg= mileage per gallon
#disp= cylinder displacement
#hp= horse power
#wt= weight of the car


# predicting mileage

input<-mtcars[,c("mpg", "disp", "hp", "wt")]
names 

cor(input)
attach(input)

plot(hp,mpg)
#doubt
model1<-lm(mpg~disp+hp+wt, data=input)#lm stands for linear model
model1
summary(model1)
 
#H0= there is no linear relation between mileage and other independent variable
#H1 = there is linear corelation between mileage and other independent variable

#model 1 accuracy = 80.83 (adjusted r sqr value)
model2<-lm(mpg~hp+wt, data=input)
model2
summary(model2)


#model 2 accuracy = 81.48

par(mfrow= c(2,2))
plot(model2)
