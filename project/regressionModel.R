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

world_TRY_regression_plot <- ggplot(world_model_TRY, aes(x=TIME, y=Value)) + geom_point(aes(color=world_data_TRY$LOCATION))+labs(color="Countries")+geom_smooth(method=lm)+ggtitle("World Tertiary Education Regression")+xlab("Years")+ylab("Percentage of Adults")
world_TRY_regression_plot

world_TRY_regression_plot_MEN <- ggplot(world_model_TRY_MEN, aes(x=TIME, y=Value)) + geom_point(aes(color=world_data_TRY$LOCATION))+labs(color="Countries")+geom_smooth(method=lm)+ggtitle("World Tertiary Education Regression (Men)")+xlab("Years")+ylab("Percentage of Adults")
world_TRY_regression_plot_MEN

world_TRY_regression_plot_WOMEN <- ggplot(world_model_TRY_WOMEN, aes(x=TIME, y=Value)) + geom_point(aes(color=world_data_TRY$LOCATION))+labs(color="Countries")+geom_smooth(method=lm)+ggtitle("World Tertiary Education Regression (Women)")+xlab("Years")+ylab("Percentage of Adults")
world_TRY_regression_plot_WOMEN

ggplot(USA_linear_model_TRY, aes(x=TIME, y=Value)) + geom_point()+geom_smooth(method=lm)



