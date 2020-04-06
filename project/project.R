#Pragati Pant
#Data Analytics
#Adult education project work

#havent started poster
#working on exploratory data analysis, starting work on linear reg model (using year to predict edu %)

adult_education_data <- read.csv(file.choose(), header=T)
attach(adult_education_data)
summary(adult_education_data)

model <- lm(Value~TIME, data=adult_education_data)    #lm model with whole data
model

plot(Value~TIME)
abline(model)

hist(Value)   #initial reading: many countries have only 10%-50% of adults that are educated
#need to break down by country, year, edu level

USA_values <- adult_education_data[adult_education_data$LOCATION=="USA",]$Value
USA_values

USA_years <- adult_education_data[adult_education_data$LOCATION=="USA",]$TIME

USA_data <- data.frame(USA_values, USA_years)
USA_data

hist(USA_values)

USA_model <- lm(USA_data$USA_values~USA_data$USA_years, data=USA_data)  #all usa values/years in model, need to break down by edu level
plot(USA_data$USA_values~USA_data$USA_years)
abline(USA_model)
