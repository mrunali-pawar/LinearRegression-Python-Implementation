install.packages("ggplot2")
install.packages("car")
install.packages("caret")
install.packages("corrplot")
library(ggplot2)
library(car)
library(caret)
library(corrplot)

#Loading data
data(mtcars)  

# Looking at variables
str(mtcars)
head(mtcars)
summary(mtcars)

#Data Prepration
mtcars$am   = as.factor(mtcars$am)
mtcars$cyl  = as.factor(mtcars$cyl)
mtcars$vs   = as.factor(mtcars$vs)
mtcars$gear = as.factor(mtcars$gear)

#Dropping dependent variable for calculating Multicollinearity
mtcars_a = subset(mtcars, select = -c(mpg))

#Identifying numeric variables
numericData <- mtcars_a[sapply(mtcars_a, is.numeric)]

#Calculating Correlation
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)

# Visualize Correlation Matrix
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7,
         tl.col = rgb(0, 0, 0))

# Checking Variables that are highly correlated
highlyCorrelated = findCorrelation(descrCor, cutoff=0.7)

#Identifying Variable Names of Highly Correlated Variables
highlyCorCol = colnames(numericData)[highlyCorrelated]

#Print highly correlated attributes
highlyCorCol

#Remove highly correlated variables and create a new dataset
dat3 = mtcars[, -which(colnames(mtcars) %in% highlyCorCol)]
dim(dat3)

#ACTUAL MODEL CREATION
#Build Linear Regression Model
fit = lm(mpg ~ ., data=dat3)

#Check Model Performance
summary(fit)

#Extracting Coefficients
summary(fit)$coeff
anova(fit)

par(mfrow=c(2,2))
plot(fit)

#Check summary of model and ANOVA table
summary(fit)
anova(fit)

#CALCULATING MODEL PERFORMANCE
#Extracting R-squared value
summary(fit)$r.squared
#Extracting Adjusted R-squared value
summary(fit)$adj.r.squared
AIC(fit)
BIC(fit)

#Variable selection using different methods
#Stepwise Selection based on AIC
library(MASS)
step1 <- stepAIC(fit, direction="both")
summary(step1)


#Backward Selection based on AIC
step2 <- stepAIC(fit, direction="backward")
summary(step2)


#Forward Selection based on AIC
step3 <- stepAIC(fit, direction="forward")
summary(step3)


#Stepwise Selection with BIC
n = dim(dat3)[1]
stepBIC = stepAIC(fit,k=log(n))
summary(stepBIC)

AIC(stepBIC)
BIC(stepBIC)

vif(stepBIC)

#See Predicted Value
pred = predict(stepBIC,dat3)
#See Actual vs. Predicted Value
finaldata = cbind(mtcars,pred)
print(head(subset(finaldata, select = c(mpg,pred))))




#K fold cross validation
install.packages("DAAG")
library(DAAG)
kfold = cv.lm(data=dat3, stepBIC, m=5)
