
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL building ------##
##----------------------------------------------------------------------------------##


##--- Step 1: Clear environment variables ------------------------------------------##


##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##

##setwd


getwd()
setwd("E:/SEM 3/Machine Learning")


##__________________________________________________________________________________##


##--- Step 3: Read the data from the csv file --------------------------------------##

cars_data = read.csv(file="Toyota_SimpleReg.csv",header=TRUE)

summary(cars_data)
str(cars_data)


##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop any irrelevant attribute(s):

cars_data = cars_data[,-c(1,2)] #dropping model and id

str(cars_data)

## Summary of the data and look for any missing values:



## Correlation and Covariance between the attributes:

#Describe how the covarainace and correlation coefficients 

cov(cars_data)
#The covariance of the age of car and price is -59136.11
#It indicates a negative linear relationship between two variables.
#This relations could be observed from the scatter plot also.


plot(cars_data$Age_06_15, cars_data$Price)
plot(cars_data$Age_06_15, cars_data$Price, xlab = "Age of the car", ylab = "Price in ($)", pch=18, col = "red")


cor(cars_data)
cor(cars_data$Age_06_15, cars_data$Price)

#The correlation coefficient of the age of car and price is -8765905.
#Since the value is close to 1 and has a -ve sighn, we can concldue that the variables are strongly negatively correlated


#Do the attributes have a good enough correlation coefficient to support linear regression model building?
# between -1 and 1 



##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (80:20) ratio

rows = seq(1, nrow(cars_data),1)

set.seed(123)

trainRows = sample(rows,(70*nrow(cars_data))/100)
cars_train = cars_data[trainRows,]
cars_test = cars_data[trainRows,]


trainRows1 = sample(rows,(80*nrow(cars_data))/100)
cars_train1 = cars_data[trainRows1,]
cars_test1 = cars_data[trainRows1,]


trainRows2 = sample(rows,(90*nrow(cars_data))/100)
cars_train2 = cars_data[trainRows2,]
cars_test2 = cars_data[trainRows2,]



##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##


LinReg = lm(Price ~ Age_06_15, data = cars_train)
LinReg
coefficients(LinReg)

LinReg1 = lm(Price ~ Age_06_15, data = cars_train1)
LinReg
coefficients(LinReg1)

LinReg2 = lm(Price ~ Age_06_15, data = cars_train2)
LinReg
coefficients(LinReg2)



## Summary of model:

summary(LinReg)
plot(LinReg$residuals)
summary(LinReg)
summary(LinReg)


#Extract the intercept coefficient from the linear regression model


#Extract the residual values


##__________________________________________________________________________________##


##--- Step 7: Check for validity of linear regression assumptions ------------------##
#HINT: plot the 4 graphs to check. Write your comments



##__________________________________________________________________________________##


##--- Step 8: Predict on testdata --------------------------------------------------##



##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##


#Error verification on train data



#Error verification on test data



##__________________________________________________________________________________##


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
#Find the confidence and prediction intervals and plot them for the WHOLE dataset





##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##
