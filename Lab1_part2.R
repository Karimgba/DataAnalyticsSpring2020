#CSV reading in EPI data
EPI_data <-read.csv(file.choose(), header=T)
EPI_data
attach(EPI_data)
head(EPI_data)

#Exercise 1: play with EPI data plots like in Lab1
plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE)
par(pty="s")

qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

#completed distribution part of Exercise 1 in Lab1

#Linear basis and least-squares constraints
multivariate <- read.csv(file.choose(), header=T)
attach(multivariate)
head(multivariate)
help(lm)
mm <- lm(Homeowners~Immigrant)  #making a linear model using homeowners and immigrants
mm
summary(mm)$coef

plot(Homeowners~Immigrant)
help(abline)
abline(mm)  #line in intercept slope form
abline(mm,col=2,lwd=3)
newImmigrantdata <- data.frame(Immigrant = c(0, 20))
predict(mm, newImmigrantdata)

#Creating plots using ggplot2
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data=mtcars)
ggplot(mtcars,aes(x=wt, y=mpg)) + geom_point()
plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")   #plot both points and lines for data
points(pressure$temperature, pressure$pressure/2, col="blue")

qplot(pressure$temperature, pressure$pressure, geom="line")  
qplot(temperature, pressure, data=pressure, geo="line")
ggplot(pressure, aes(x=temperature, y=pressure))+geom_line()+geom_point()

#Creating bar graphs with ggplot2
barplot(BOD$demand, names.arg=BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))  #make a table of counts
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl), data=mtcars)
ggplot(mtcars, aes(x=factor(cyl)))+geom_bar()

#Creating histograms with ggplot2
hist(mtcars$mpg)
hist(mtcars$mpg, breaks=10)
hist(mtcars$mpg, breaks=5)
hist(mtcars$mpg, breaks=12)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth=4)      
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth=5)      
      
      
  
  
  


