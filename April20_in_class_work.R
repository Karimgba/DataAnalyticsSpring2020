# LOESS Example 1:
data(economics, package="ggplot2") # load data
economics$index <- 1:nrow(economics) # create index variable
economics <- economics[1:80, ] # retail 80rows for better graphical understanding
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span
loessMod50 <- loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span

# predict Loess
smoothed10 <- predict(loessMod10)
smoothed25 <- predict(loessMod25)
smoothed50 <- predict(loessMod50)

# plot the predictions
plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction",
     xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")

# LOESS Example 2:
# Fitting a curve to the data
data("cars")
str(cars) # we see 50 observation and 2 variables
# create a plot, speed Vs distance
plot(speed ~ dist, data = cars)

help("lowess")

lowess(cars$speed ~ cars$dist)
# use the lowess() function along with the line() function to draw the lines
lines(lowess(cars$speed ~ cars$dist, f=2/3), col="blue")
# here the f value is the the smoother span, f= 2/3 = 0.666
# the default value for smoother span is 0.666 in RStudio.
#This gives the proportion of points in the plot which influence the smooth at eachvalue.
# Larger values give more smoothness.
# Change the "f" value and observe the shape of the line.
# lines(lowess(cars$speed ~ cars$dist, f=0.75), col="gray") # f = 0.75
lines(lowess(cars$speed ~ cars$dist, f=0.8), col="red") # f = 0.8
lines(lowess(cars$speed ~ cars$dist, f=0.9), col="green") # f = 0.9
lines(lowess(cars$speed ~ cars$dist, f=0.1), col= 5) # f = 0.1
lines(lowess(cars$speed ~ cars$dist, f=0.01), col= 6) # f = 0.01

# Linear Discriminant Analysis Example:
# Multiclass Classification
library(MASS)
names(iris)
dim(iris) # check the dimensions of the iris dataset, you will see 150 rows and 5 columns
head(iris)

# set the seed value and create training dataset
set.seed(555)
Train <- sample(1:nrow(iris), nrow(iris)/2)
iris_Train <- iris[Train,] # Traning dataset
irist_Test <- iris[-Train,] # Testing dataset

help(lda)

fit1 <- lda(Species ~ Sepal.Length + Sepal.Width +
              Petal.Length + Petal.Width, data = iris_Train)

predict1 <- predict(fit1, iris_Train)
predict1_class <- predict1$class

# generating the confusion matrix using the table() function
table1 <- table(predict1_class, iris_Train$Species)
table1
# Calculating the Accuracy of the prediction
sum(diag(table1))/sum(table1)

