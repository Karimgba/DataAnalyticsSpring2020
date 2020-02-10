##################### Abalone dataset work ############################
# reading the dataset from UCI repository URL
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE,
                    sep = ",")
# Column names
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight',
                       'rings' )
# summary on abalone
summary(abalone)
# structure of the abalone data
str(abalone)
# summary of the abalone rings column
summary(abalone$rings)
# “rings” have a range between 1-29.

# break the rings into 3 levels" “young” for abalones less than 8, “adult” for abalones between 8-11,
# and “old” for abalones older than 11.
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)
# remove the "sex" varialbe in abalone, because KNN requires all numeric variables for prediction
# z <- abalone
aba <- abalone
aba$sex <- NULL
# normalize the data using min max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)
# After Normalization, each variable has a min of 0 and a max of 1.
# in otherwords, values are in the range from 0 to 1.
# split the data into training and testing sets.
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)
# make k equal to the square root of 2918, the number of observations in the training set.
# sqrt(2918) ~= 54.01852 round it to 55 and use k = 55 # We usually take an Odd number for k value,
# knn model
# knn() is in the "class" library.Make sure to install it first on your RStudio.
library(class)
help("knn") # Read the knn documentation on RStudio.
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)

################ KNN example using ISLR package (Textbook) ######################
library(ISLR) # you need to install the ISLR package first
# Caravan dataset is about the insurance
library(class)
head(Caravan)
str(Caravan)
# Purchase: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
# "Yes" or "No" indicates wheater people purchased the insurance policy or not. 
dim(Caravan) # it has 5822 rows (observations) and 86 features/columns  
summary(Caravan)
summary(Caravan$Purchase)
# Yes: 348 people purchased the insurance and No:5474 people did not purchase the insurance
# check for any NA and missing values
any(is.na(Caravan)) # FALSE, that means no NA values in this dataset 

# Checking the Variances on features/Colums using the var() function in R
var(Caravan[,1]) # Variance of the 1st column is 165.0378
var(Caravan[,2]) # Variance of the 2nd column is 0.1647
var(Caravan[,3]) # Variance of the 3rd column is 0.6238

purchase <- Caravan[,86] # you can write the same as 
# purchase <-Caravan[,'Purchase'] with the column name, we use the column number 86 for the simplcity.
purchase
# Now we want to Standardize the columns except the 86th column
StandardizedCaravan <- scale(Caravan[,-86]) # when we use -86 it will not include the 86th column.
var(StandardizedCaravan[,1])
var(StandardizedCaravan[,2])
var(StandardizedCaravan[,3])

# test set
test_index <- 1:1000
test_data <- StandardizedCaravan[test_index,]
test_purchase <- purchase[test_index]

# train set
train_data <- StandardizedCaravan[-test_index,]
train_purchase <- purchase[-test_index]

# set seed
set.seed(101)
predicted_purchase <- knn(train_data,test_data,train_purchase, k = 10)
head(predicted_purchase)

missClassError <- mean(test_purchase != predicted_purchase)
print(missClassError)

# Choosing the K value
# we can write a for-loop
predicted_purchase <- NULL
error_rate <- NULL

for (i in 1:20) {
  set.seed(101)
  predicted_purchase <- knn(train_data, test_data, train_purchase, k =i)
  error_rate[i] <- mean(test_purchase != predicted_purchase)
}

print(error_rate)

# Plot the K value on a graph
library(ggplot2)
k_values <- 1:20

error_df <- data.frame(error_rate, k_values)
print(error_df)
plot1 <- ggplot(error_df,aes(k_values,error_rate)) + geom_point() + geom_line(lty='dotted', color='blue')

