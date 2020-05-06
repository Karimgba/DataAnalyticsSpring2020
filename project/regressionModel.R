#Pragati Pant
#Data Analytics
#Adult education project model work


############ cleaning, filtering data #################
adult_education_data <- read.csv(file.choose(), header=T)
attach(adult_education_data)
summary(adult_education_data)

adult_education_data <- adult_education_data[adult_education_data$LOCATION!="OAVG", ]  #clean out the random oavg value

USA_data <- adult_education_data[adult_education_data$LOCATION=="USA", ]
USA_values_1981 <- USA_data[USA_data$TIME==1981, ]$Value

USA_data_TRY <- USA_data[USA_data$SUBJECT=="TRY", ]
USA_data_UPPSRY <- USA_data[USA_data$SUBJECT=="UPPSRY", ]
USA_data_BUPPSRY <- USA_data[USA_data$SUBJECT=="BUPPSRY", ]
USA_data_TRY_MEN <- USA_data[USA_data$SUBJECT=="TRY_MEN", ]
USA_data_UPPSRY_MEN <- USA_data[USA_data$SUBJECT=="UPPSRY_MEN", ]
USA_data_TRY_WOMEN <- USA_data[USA_data$SUBJECT=="TRY_WOMEN", ]
USA_data_UPPSRY_WOMEN <- USA_data[USA_data$SUBJECT=="UPPSRY_WOMEN", ]

world_data_1981_TRY <- filter(adult_education_data, (adult_education_data$TIME==1981 & adult_education_data$SUBJECT=="TRY"))

world_data_2000_TRY <- filter(adult_education_data, (adult_education_data$TIME==2018 & adult_education_data$SUBJECT=="TRY" & adult_education_data$LOCATION!="OAVG"))

world_data_2018_TRY <- filter(adult_education_data, (adult_education_data$TIME==2018 & adult_education_data$SUBJECT=="TRY" & adult_education_data$LOCATION!="OAVG"))

library(dplyr)
USA_values_1981_TRY <- filter(USA_data, (USA_data$TIME==1981 & USA_data$SUBJECT=="TRY"))
USA_values_1981_TRY

USA_years <- adult_education_data[adult_education_data$LOCATION=="USA",]$TIME

USA_data <- data.frame(USA_values, USA_years)
USA_data

world_data_TRY <- adult_education_data[adult_education_data$SUBJECT=="TRY", ]
world_data_UPPSRY <- adult_education_data[adult_education_data$SUBJECT=="UPPSRY", ]
world_data_BUPPSRY <- adult_education_data[adult_education_data$SUBJECT=="BUPPSRY", ]
world_data_TRY_MEN <- adult_education_data[adult_education_data$SUBJECT=="TRY_MEN", ]
world_data_UPPSRY_MEN <- adult_education_data[adult_education_data$SUBJECT=="UPPSRY_MEN", ]
world_data_TRY_WOMEN <- adult_education_data[adult_education_data$SUBJECT=="TRY_WOMEN", ]
world_data_UPPSRY_WOMEN <- adult_education_data[adult_education_data$SUBJECT=="UPPSRY_WOMEN", ]

############ model definitions #################

#US models
USA_linear_model_UPPSRY <- lm(Value~TIME, data=USA_data_UPPSRY)  #all usa values/years in model, need to break down by edu level
summary(USA_linear_model_UPPSRY)  #Multiple R-squared:  0.9213,	Adjusted R-squared:  0.9191 

USA_linear_model_TRY <- lm(Value~TIME, data=USA_data_TRY)  #all usa values/years in model, need to break down by edu level
summary(USA_linear_model_TRY)  #Multiple R-squared:  0.9613,	Adjusted R-squared:  0.9602 

USA_linear_model_BUPPSRY <- lm(Value~TIME, data=USA_data_BUPPSRY)  #all usa values/years in model, need to break down by edu level
summary(USA_linear_model_BUPPSRY)  #Multiple R-squared:  0.9171,	Adjusted R-squared:  0.9148 

#US GENDER models
USA_linear_model_UPPSRY_MEN <- lm(Value~TIME, data=USA_data_UPPSRY_MEN)  #all usa values/years in model, need to break down by edu level
summary(USA_linear_model_UPPSRY_MEN)  #Multiple R-squared:  0.8194,	Adjusted R-squared:  0.8144 

USA_linear_model_UPPSRY_WOMEN <- lm(Value~TIME, data=USA_data_UPPSRY_WOMEN)  #all usa values/years in model, need to break down by edu level
summary(USA_linear_model_UPPSRY_WOMEN)  #Multiple R-squared:  0.9505,	Adjusted R-squared:  0.9491 

USA_linear_model_TRY_MEN <- lm(Value~TIME, data=USA_data_TRY_MEN)  #all usa values/years in model, need to break down by edu level
summary(USA_linear_model_TRY_MEN)  #Multiple R-squared:  0.9361,	Adjusted R-squared:  0.9343 

USA_linear_model_TRY_WOMEN <- lm(Value~TIME, data=USA_data_TRY_WOMEN)  #all usa values/years in model, need to break down by edu level
summary(USA_linear_model_TRY_WOMEN)  #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.971 

#world models from 1981 to 2018
world_model_TRY <- lm(Value~TIME, data=world_data_TRY)    #lm model with whole data 
summary(world_model_TRY) #Multiple R-squared:  0.2062,	Adjusted R-squared:  0.2054    shows there is a weak linear relationship

world_model_UPPSRY <- lm(Value~TIME, data=world_data_UPPSRY)    #lm model with whole data 
summary(world_model_UPPSRY) #Multiple R-squared:  0.006034,	Adjusted R-squared:  0.0049    shows there is a weak linear relationship

world_model_BUPPSRY <- lm(Value~TIME, data=world_data_BUPPSRY)    #lm model with whole data 
summary(world_model_BUPPSRY) #Multiple R-squared:  0.09964,	Adjusted R-squared:  0.09864    shows there is a weak linear relationship

#world GENDER models
world_model_TRY_WOMEN <- lm(Value~TIME, data=world_data_TRY_WOMEN)    #lm model with whole data 
summary(world_model_TRY_WOMEN) #Multiple R-squared:  0.2615,	Adjusted R-squared:  0.2607     shows there is a weak linear relationship

world_model_UPPSRY_WOMEN <- lm(Value~TIME, data=world_data_UPPSRY_WOMEN)    #lm model with whole data 
summary(world_model_UPPSRY_WOMEN) #Multiple R-squared:  0.001757,	Adjusted R-squared:  0.0006192    shows there is a weak linear relationship

world_model_TRY_MEN <- lm(Value~TIME, data=world_data_TRY_MEN)    #lm model with whole data 
summary(world_model_TRY_MEN) #Multiple R-squared:  0.1174,	Adjusted R-squared:  0.1164   shows there is a weak linear relationship

world_model_UPPSRY_MEN <- lm(Value~TIME, data=world_data_UPPSRY_MEN)    #lm model with whole data 
summary(world_model_UPPSRY_MEN) #Multiple R-squared:  0.01215,	Adjusted R-squared:  0.01102    shows there is a weak linear relationship

#world models from 2000 to 2018         CHANGE DATA TO 2000
world_model_TRY <- lm(Value~TIME, data=world_data_TRY)    #lm model with whole data 
summary(world_model_TRY) #Multiple R-squared:  0.2101,	Adjusted R-squared:  0.2092   shows there is a weak linear relationship

world_model_UPPSRY <- lm(Value~TIME, data=world_data_UPPSRY)    #lm model with whole data 
summary(world_model_UPPSRY) #Multiple R-squared:  0.006004,	Adjusted R-squared:  0.004897    shows there is a weak linear relationship

world_model_BUPPSRY <- lm(Value~TIME, data=world_data_BUPPSRY)    #lm model with whole data 
summary(world_model_BUPPSRY) #Multiple R-squared:  0.1013,	Adjusted R-squared:  0.1003    shows there is a weak linear relationship


############ model plots #################
library(ggplot2)

#world plots from 1981 to 2018
world_BUPPSRY_regression_plot <- ggplot(world_model_BUPPSRY, aes(x=TIME, y=Value)) + geom_point(aes(color=world_data_BUPPSRY$LOCATION))+labs(color="Countries")+geom_smooth(method=lm)+ggtitle("World Below Upper Secondary Education Regression")+xlab("Years")+ylab("Percentage of Adults")
world_BUPPSRY_regression_plot

world_UPPSRY_regression_plot <- ggplot(world_model_UPPSRY, aes(x=TIME, y=Value)) + geom_point(aes(color=world_data_UPPSRY$LOCATION))+labs(color="Countries")+geom_smooth(method=lm)+ggtitle("World Upper Secondary Education Regression")+xlab("Years")+ylab("Percentage of Adults")
world_UPPSRY_regression_plot

world_UPPSRY_regression_plot_MEN <- ggplot(world_model_UPPSRY_MEN, aes(x=TIME, y=Value)) + geom_point(aes(color=world_data_UPPSRY$LOCATION))+labs(color="Countries")+geom_smooth(method=lm)+ggtitle("World Upper Secondary Education Regression (Men)")+xlab("Years")+ylab("Percentage of Adults")
world_UPPSRY_regression_plot_MEN

world_UPPSRY_regression_plot_WOMEN <- ggplot(world_model_UPPSRY_WOMEN, aes(x=TIME, y=Value)) + geom_point(aes(color=world_data_UPPSRY$LOCATION))+labs(color="Countries")+geom_smooth(method=lm)+ggtitle("World Upper Secondary Education Regression (Women)")+xlab("Years")+ylab("Percentage of Adults")
world_UPPSRY_regression_plot_WOMEN

world_TRY_regression_plot <- ggplot(world_model_TRY, aes(x=TIME, y=Value)) + geom_point(aes(color=world_data_TRY$LOCATION))+labs(color="Countries")+geom_smooth(method=lm)+ggtitle("World Tertiary Education Regression")+xlab("Years")+ylab("Percentage of Adults")
world_TRY_regression_plot

world_TRY_regression_plot_MEN <- ggplot(world_model_TRY_MEN, aes(x=TIME, y=Value)) + geom_point(aes(color=world_data_TRY$LOCATION))+labs(color="Countries")+geom_smooth(method=lm)+ggtitle("World Tertiary Education Regression (Men)")+xlab("Years")+ylab("Percentage of Adults")
world_TRY_regression_plot_MEN

world_TRY_regression_plot_WOMEN <- ggplot(world_model_TRY_WOMEN, aes(x=TIME, y=Value)) + geom_point(aes(color=world_data_TRY$LOCATION))+labs(color="Countries")+geom_smooth(method=lm)+ggtitle("World Tertiary Education Regression (Women)")+xlab("Years")+ylab("Percentage of Adults")
world_TRY_regression_plot_WOMEN

#US plots from 1981 to 2018
ggplot(USA_linear_model_BUPPSRY, aes(x=TIME, y=Value)) + geom_point()+geom_smooth(method=lm)+ggtitle("United States Below Upper Secondary Education Regression")+xlab("Years")+ylab("Percentage of Adults")


ggplot(USA_linear_model_UPPSRY, aes(x=TIME, y=Value)) + geom_point()+geom_smooth(method=lm)+ggtitle("United States Upper Secondary Education Regression")+xlab("Years")+ylab("Percentage of Adults")
ggplot(USA_linear_model_UPPSRY_MEN, aes(x=TIME, y=Value)) + geom_point()+geom_smooth(method=lm)+ggtitle("United States Upper Secondary Education Regression (Men)")+xlab("Years")+ylab("Percentage of Adults")
ggplot(USA_linear_model_UPPSRY_WOMEN, aes(x=TIME, y=Value)) + geom_point()+geom_smooth(method=lm)+ggtitle("United States Upper Secondary Education Regression (Women)")+xlab("Years")+ylab("Percentage of Adults")

ggplot(USA_linear_model_TRY, aes(x=TIME, y=Value)) + geom_point()+geom_smooth(method=lm)+ggtitle("United States Tertiary Education Regression")+xlab("Years")+ylab("Percentage of Adults")
ggplot(USA_linear_model_TRY_MEN, aes(x=TIME, y=Value)) + geom_point()+geom_smooth(method=lm)+ggtitle("United States Tertiary Education Regression (Men)")+xlab("Years")+ylab("Percentage of Adults")
ggplot(USA_linear_model_TRY_WOMEN, aes(x=TIME, y=Value)) + geom_point()+geom_smooth(method=lm)+ggtitle("United States Tertiary Education Regression (Women)")+xlab("Years")+ylab("Percentage of Adults")

#US predictions -- 2020
test_2020_TRY <- predict(USA_linear_model_TRY, data.frame(TIME=2020))
test_2020_TRY       #50.24887 of adults will belong to tertiary level

test_2020_TRY_MEN <- predict(USA_linear_model_TRY_MEN, data.frame(TIME=2020))
test_2020_TRY_MEN       #46.0794 of men will belong to tertiary level

test_2020_TRY_WOMEN <- predict(USA_linear_model_TRY_WOMEN, data.frame(TIME=2020))
test_2020_TRY_WOMEN       #54.24446 of women will belong to tertiary level

test_2020_UPPSRY <- predict(USA_linear_model_UPPSRY, data.frame(TIME=2020))
test_2020_UPPSRY       #42.3097 of adults will belong to tertiary level

test_2020_UPPSRY_MEN <- predict(USA_linear_model_UPPSRY_MEN, data.frame(TIME=2020))
test_2020_UPPSRY_MEN       #45.29563 of men will belong to tertiary level

test_2020_UPPSRY_WOMEN <- predict(USA_linear_model_UPPSRY_WOMEN, data.frame(TIME=2020))
test_2020_UPPSRY_WOMEN       #39.45767 of women will belong to tertiary level

test_2020_BUPPSRY <- predict(USA_linear_model_BUPPSRY, data.frame(TIME=2020))
test_2020_BUPPSRY       #7.441435 of adults will belong to below upper secondary level

#US predictions -- 2050
test_2050_TRY <- predict(USA_linear_model_TRY, data.frame(TIME=2050))
test_2050_TRY       #73.85634 of adults will belong to tertiary level

test_2050_TRY_MEN <- predict(USA_linear_model_TRY_MEN, data.frame(TIME=2050))
test_2050_TRY_MEN       #62.98439 of men will belong to tertiary level

test_2050_TRY_WOMEN <- predict(USA_linear_model_TRY_WOMEN, data.frame(TIME=2050))
test_2050_TRY_WOMEN       #84.25239 of women will belong to tertiary level

test_2050_UPPSRY <- predict(USA_linear_model_UPPSRY, data.frame(TIME=2050))
test_2050_UPPSRY       #28.72742 of adults will belong to tertiary level

test_2050_UPPSRY_MEN <- predict(USA_linear_model_UPPSRY_MEN, data.frame(TIME=2050))
test_2050_UPPSRY_MEN       #37.56876 of men will belong to tertiary level

test_2050_UPPSRY_WOMEN <- predict(USA_linear_model_UPPSRY_WOMEN, data.frame(TIME=2050))
test_2050_UPPSRY_WOMEN       #20.29467 of women will belong to tertiary level

test_2050_BUPPSRY <- predict(USA_linear_model_BUPPSRY, data.frame(TIME=2050))
test_2050_BUPPSRY       #-2.583758 of adults will belong to below upper secondary level

#US predictions -- 2075
test_2075_TRY <- predict(USA_linear_model_TRY, data.frame(TIME=2075))
test_2075_TRY       #93.52923 of adults will belong to tertiary level

test_2075_TRY_MEN <- predict(USA_linear_model_TRY_MEN, data.frame(TIME=2075))
test_2075_TRY_MEN       #77.07188 of men will belong to tertiary level

test_2075_TRY_WOMEN <- predict(USA_linear_model_TRY_WOMEN, data.frame(TIME=2075))
test_2075_TRY_WOMEN       #109.259 of women will belong to tertiary level

test_2075_UPPSRY <- predict(USA_linear_model_UPPSRY, data.frame(TIME=2075))
test_2075_UPPSRY       #17.40886 of adults will belong to tertiary level

test_2075_UPPSRY_MEN <- predict(USA_linear_model_UPPSRY_MEN, data.frame(TIME=2075))
test_2075_UPPSRY_MEN       #31.1297 of men will belong to tertiary level

test_2075_UPPSRY_WOMEN <- predict(USA_linear_model_UPPSRY_WOMEN, data.frame(TIME=2075))
test_2075_UPPSRY_WOMEN       #4.325503 of women will belong to tertiary level

test_2075_BUPPSRY <- predict(USA_linear_model_BUPPSRY, data.frame(TIME=2075))
test_2075_BUPPSRY       #-10.93809 of adults will belong to below upper secondary level
