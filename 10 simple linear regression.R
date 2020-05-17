

data() #to see the whole dataset
View(faithful)
attach(faithful)

names(faithful)

str(faithful) #structure of dataset

head(faithful, 10) #top 10 rows of dataset faithful

summary(faithful) #it gives behaviour of one column also checking missing value. also called univariant analysis

sapply(faithful, function(x) sum(is.na(x)))#another way of missing value check. function(X) is by default for R
#sapply gives ans in column format

#scatter plot and correlation
plot(waiting, eruptions) #x=independent(waiting), y= dependent(eruption)

plot(eruptions~waiting) #~is called TELDA USED FOR SHOWING X DEPANDS ON Y

cor(faithful) #COR IS FOR CORRELATION

# IN THIS EXAMPLE CORRELATION MEANS SMALL CHANGE IN WAITING WILL AFFECT ERRUPTION IN 90% as correlation is showing 90% in output

input<-faithful

#LM STANDS FOR LINEAR MODEL
eruption.lm = lm(eruptions ~ waiting, data = input)

summary(eruption.lm) #gives decision  whether linear relationship is there or not and can find B0 and B1

Y= B0+B1*X
y= -1.87402+0.075628*80
y

anova(eruption.lm) #decison can be taken whether linear relationship is there or not but B0 and B1 can not be found

#just to chck mathematically
input<- faithful
Probability <-data.frame(eruption.lm$fitted.values)

Residual<- data.frame(eruption.lm$residuals)

input[,"Probablity"]<-Probability$eruption.lm.fitted.values
input[,"Residual"]<- Residual$eruption.lm.residuals
summary(eruption.lm)


sum(input$Residual)  

plot(eruptions~waiting)

abline(eruption.lm , col="Red")


par(mfrow=c(2,2)) #to show plot in 2cross2 matrix form

plot(eruption.lm)

library(lmtest) #doubt (update c, c++, java , java d)

dwtest(eruption.lm)

plot(eruptions~waiting)
plot(eruptions~waiting)
abline(eruption.lm, col="red")

