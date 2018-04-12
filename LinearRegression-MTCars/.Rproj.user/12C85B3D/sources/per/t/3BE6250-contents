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
step <- stepAIC(fit, direction="both")
summary(step)


#Backward Selection based on AIC
step <- stepAIC(fit, direction="backward")
summary(step)


#Forward Selection based on AIC
step <- stepAIC(fit, direction="forward")
summary(step)


#Stepwise Selection with BIC
n = dim(dat3)[1]
stepBIC = stepAIC(fit,k=log(n))
summary(stepBIC)

AIC(stepBIC)
BIC(stepBIC)

#calculating standerd coeficient
#Standardised coefficients
library(QuantPsyc)
lm.beta(stepBIC)


#R Function : Manual Calculation of Standardised coefficients
stdz.coff <- function (regmodel)
{ b <- summary(regmodel)$coef[-1,1]
sx <- sapply(regmodel$model[-1], sd)
sy <- sapply(regmodel$model[1], sd)
beta <- b * sx / sy
return(beta)
}

std.Coeff = data.frame(Standardized.Coeff = stdz.coff(stepBIC))
std.Coeff = cbind(Variable = row.names(std.Coeff), std.Coeff)
row.names(std.Coeff) = NULL

#Calculating Variance Inflation Factor (VIF)
vif(stepBIC)

#testing other assumption
#Autocorrelation Test
durbinWatsonTest(stepBIC)
#Normality Of Residuals (Should be > 0.05)
res=residuals(stepBIC,type="pearson")
shapiro.test(res)
#Testing for heteroscedasticity (Should be > 0.05)
ncvTest(stepBIC)
#Outliers – Bonferonni test
outlierTest(stepBIC)
#See Residuals
resid = residuals(stepBIC)
#Relative Importance
install.packages("relaimpo")
library(relaimpo)
calc.relimp(stepBIC)

#See Predicted Value
pred = predict(stepBIC,dat3)
#See Actual vs. Predicted Value
finaldata = cbind(mtcars,pred)
print(head(subset(finaldata, select = c(mpg,pred))))


#Other useful functions
#Calculating RMSE
rmse = sqrt(mean((dat3$mpg - pred)^2))
print(rmse)

#Calculating Rsquared manually
y = dat3[,c("mpg")]
R.squared = 1 - sum((y-pred)^2)/sum((y-mean(y))^2)
print(R.squared)

#Calculating Adj. Rsquared manually
n = dim(dat3)[1]
p = dim(summary(stepBIC)$coeff)[1] - 1
adj.r.squared = 1 - (1 - R.squared) * ((n - 1)/(n-p-1))
print(adj.r.squared)

#Box Cox Transformation
library(lmSupport)
modelBoxCox(stepBIC)


#K fold cross validation
library(DAAG)
kfold = cv.lm(data=dat3, stepBIC, m=5)
