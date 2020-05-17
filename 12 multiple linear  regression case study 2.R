
#vif formula(automated) is used to remove the column which has highest colinearity with others
#vif helps to remove multi colinearity
library(car)

input<-mtcars

names(input)
cor(input)
model1<- lm(mpg~., data = input) #enter method
# . means all variable
model1
summary(model1)
vif(model1)

#removing disp from model based on vif value
model2<- lm(mpg~.-disp, data = input) # . means all variable, here we removed disp from model, not from data
#here we are removing disp bcz it has highest vif value
summary(model2)
vif(model2)
#model1 accuracy = 80.66 model2 accuracy = 81.05

#removing cyl based on vif value
model3<- lm(mpg~.-disp-cyl, data = input) # . means all variable, here we removed disp and cyl from model, not from data
summary(model3)
vif(model3)

#model 3 accuracy = 81.87 

#doing forward, backward and stepwise

model1<-lm(mpg~., data=input) ##enter method

summary(model1)
vif(model1)

#backward method
model2<- step(lm(mpg~., data=input), direction = "backward")

summary(model2)
vif(model2)
#here we are getting output that all variables p value <alpha (0.05). so we reject H0. 
#means there is relationship between x and y. that means all these parameters can be used to predict mileage.
#here we find accuracy as 83.86(in backward vif method)

par(mfrow=c(2,2))
plot(model2)

library(lmtest)
dwtest(model2) ## from dw test we found that p value>alpha(0.05). so we accept H0.
#means there is auto correlation between variables

#non constant variance test(ncvTest) 
#ncv test is used to find variance is constant or not from numbers
#scatter plot is used for graphical representation of variance
#ncv test is the numerical representation to know vaiance is constant or not
ncvTest(model2)
#from ncv test we found that p value > alpha(0.05). so we accept H0
#means the variance is constant. (there is no diference in variance)

#stepwise method
model3<-step(lm(mpg~., data = input), direction = "both")
summary (model3)
vif(model3)


#forward method
#forward methiod is not applicable to this data as forward starts with checking intercept p value and as it is > alpha (0.05) so it will be stopped here only. so nothing will be removed from  model. so we cant predict any variance
model4<-step(lm(mpg~., data = input), direction = "forward")
summary(model4)
vif(model4)

