# SETTING A WORKING DIRECTORY

getwd()
setwd("E:/SEM 3/Machine Learning")


# Importing a CSV file
df1 <- read.table(file ="German-Credit_1.csv", header = T, sep = ",")

# when we know that the delimiter is a comma
df1 <- read.csv(file = "German-Credit_1.csv", header=T)


# readLines() function to look at how our data is stored
readLines(con = "German-Credit_1.csv", n = 5)

# read.table() function to read in the data
head(df1)


## IMPORTING SHEETS

#  LIBRARIES
library(XLConnect)
library(rJava)


# MERGEING two datasets

df1 <- read.csv(file = "German-Credit_1.csv", header = TRUE)
df2 <- read.csv(file = "German-Credit_2.csv", header = TRUE)

head(df1)
colnames(df1)

head(df2)
colnames(df2)

# Merge both the data frames, df1 and df2 by the common variable OBS

df3 <- merge(x = df1, y = df2, by = "OBS", all = T)

head(df3)


# EXPLORING the data

head(df3) # First few rows of the dataset
colnames(df3) # Column Names of the dataset
summary(df3) # Helps you arrive at the numerical distribution of each variable
str(df3) # Helps you understand the data type of each variable in the dataset


# CONVERT variable into appropriate data

# Create a vector of all the column names that you know have categorical attributes

num_Attr <- c("DURATION","AMOUNT","INSTALL_RATE","AGE","NUM_CREDITS","NUM_DEPENDENTS")

# setdiff() function is a set operation that returns a vector of all the elements that other than the intersection of the two vectors
cat_Attr <- setdiff(x = colnames(df3), y = num_Attr)

# Use the as.character() function to convert the OBS variable as it is just a record number
# Whenever you convert numeric attributes into factors, do not forget to convert them into characters first
# When you convert a numeric attribute into a factor and then reconvert it into a numeric the values of the original attribute change

df3$OBS <- as.character(df3$OBS)

test_vec <- c(0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0)

test_vec

fac_vec <- as.factor(test_vec)

fac_vec

reconverted_vec <- as.numeric(fac_vec)

reconverted_vec

# convert the numerically encoded variables to a factor by first converting it into a character

df3$RESPONSE <- as.factor(as.character(df3$RESPONSE))

# Use the apply function to convert all the numerically encoded categorical variables.
# Then replace them in the original dataframe.

df_cat <- subset(df3,select =cat_Attr)

df3[,cat_Attr] <- data.frame(apply(df_cat, 2, function(x) as.factor(as.character(x))))
                                                          
str(df3)



# HANDLING Missing Values

colSums(is.na(x = df3))

sum(is.na(df3))


                                   
# DROPPING the records with missing values

df4 <- na.omit(df3)
dim(df3)
dim(df4)
sum(is.na(df4))


                                   
# INPUTING/SUBSTITUTING missing values

library(DMwR)

manyNAs(df3, 0.1)

df3_imputed <- centralImputation(data = df3) #Cenral Imputation

sum(is.na(df3_imputed))

df3_imputed1 <- knnImputation(data = df3, k=5) #KNN Imputation

sum(is.na(df3_imputed1))



# Binning / Discretizing the variable
                                   
library(infotheo)
x <- c(5,6,7,8,8,8,8,8,11,20,21,22)

length(x)

x0 <- discretize(x, disc = "equalfreq", nbins = 4)

table(x0)

x1 <- discretize(x, disc = "equalwidth", nbins = 4)

table(x1)

# binning the AMOUNT variable from the given dataset

AmtBin <- discretize(df3_imputed$AMOUNT, disc="equalfreq",nbins=4)

table(AmtBin)

AmtBin <- discretize(df3_imputed$AMOUNT, disc="equalwidth",nbins=4)

table(AmtBin)

                                   
# DUMMY Variables

library(dummies)

df_ex <- datasets::warpbreaks

table(df_ex$tension)

dummy_ex <- dummy(df_ex$tension)

head(dummy_ex)

df_cat <- subset(df3_imputed,select =cat_Attr)

df_cat_dummies <- data.frame(apply(df_cat, 2, function(x) dummy(x)))

dim(df_cat_dummies)


#STANDARDIZING the data

library(vegan)

df_num <- df3_imputed[, num_Attr]
df_num2 <- decostand(x = df_num, method = "range") # using range method
summary(df_num2)

df_num3 <- decostand(x = df_num, method = "standardize") # Using Z score method
summary(df_num3)

summary(df_num3)

df_final <- cbind(df_num3,df_cat)
head(df_final)


# TRAIN TEST SPLIT

rows <- seq(1,1000,1)
set.seed(123)

trainRows <- sample(rows,600)
train_data <- df_final[trainRows,]
test_data <- df_final[-c(trainRows),]
dim(train_data)

dim(test_data)


# Build model

lm_model <- lm(AMOUNT~DURATION, data=train_data)
summary(lm_model)


                                   
                                   
# BASIC DATA VISUALIZATIONS

df <- df_final # Store the final dataset in the df variable


## Histogram
hist(df$AGE)
hist(df$AGE,col = "yellow")


## Box plot
boxplot(df$AGE,horizontal = TRUE)
boxplot(AMOUNT~RESPONSE, data = df, xlab ="TARGET", ylab = "AMOUNT", main =
          "Continuous v/s Categorical")

## Bar plot
barplot(table(df$RESPONSE)) #should have unique level and count
barplot(table(df$RESPONSE),col = "Green")

                                   
## Scatter Plot                                 
plot(x=df$AGE,y =df$AMOUNT ,xlab = "DURATION",ylab="AMOUNT",main= "Continuous v/s Continuous")
