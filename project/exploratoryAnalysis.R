#Pragati Pant
#Data Analytics
#Adult education project exploratory work

adult_education_data <- read.csv(file.choose(), header=T)
attach(adult_education_data)
summary(adult_education_data)
'LOCATION       INDICATOR            SUBJECT        MEASURE     FREQUENCY
 USA    : 266   EDUADULT:6389   BUPPSRY     :923   PC_25_64:6389   A:6389   
 CRI    : 224                   TRY         :922                            
 GRC    : 224                   TRY_MEN     :922                            
 CAN    : 210                   TRY_WOMEN   :922                            
 ESP    : 196                   UPPSRY      :900                            
 FRA    : 196                   UPPSRY_MEN  :900                            
 (Other):5073                   UPPSRY_WOMEN:900                            
      TIME          Value        Flag.Codes    
 Min.   :1981   Min.   : 2.858   Mode:logical  
 1st Qu.:2001   1st Qu.:20.226   NAs:6389     
Median :2007   Median :33.035                 
Mean   :2006   Mean   :33.877                 
3rd Qu.:2013   3rd Qu.:43.603                 
Max.   :2018   Max.   :89.667  '

USA_data <- adult_education_data[adult_education_data$LOCATION=="USA", ]
############ US data distributions ############
USA_data_TRY <- USA_data[USA_data$SUBJECT=="TRY", ]
USA_TRY_change <- plot(USA_data_TRY$Value~USA_data_TRY$TIME, main="Tertiary Education Level (USA)", xlab="Years", ylab="Percentage of Adults")

USA_data_UPPSRY <- USA_data[USA_data$SUBJECT=="UPPSRY", ]
USA_UPPSRY_change <- plot(USA_data_UPPSRY$Value~USA_data_UPPSRY$TIME, main="Upper Secondary Education Level (USA)", xlab="Years", ylab="Percentage of Adults")

USA_data_BUPPSRY <- USA_data[USA_data$SUBJECT=="BUPPSRY", ]
USA_BUPPSRY_change <- plot(USA_data_BUPPSRY$Value~USA_data_BUPPSRY$TIME, main="Below Secondary Education Level (USA)", xlab="Years", ylab="Percentage of Adults")

USA_data_TRY_MEN <- USA_data[USA_data$SUBJECT=="TRY_MEN", ]
USA_TRY_change_MEN <- plot(USA_data_TRY_MEN$Value~USA_data_TRY_MEN$TIME, main="Tertiary Education Level for Men (USA)", xlab="Years", ylab="Percentage of Men")

USA_data_UPPSRY_MEN <- USA_data[USA_data$SUBJECT=="UPPSRY_MEN", ]
USA_UPPSRY_change_MEN <- plot(USA_data_UPPSRY_MEN$Value~USA_data_UPPSRY_MEN$TIME, main="Upper Secondary Education Level for Men (USA)", xlab="Years", ylab="Percentage of Men")

USA_data_TRY_WOMEN <- USA_data[USA_data$SUBJECT=="TRY_WOMEN", ]
USA_TRY_change_WOMEN <- plot(USA_data_TRY_WOMEN$Value~USA_data_TRY_WOMEN$TIME, main="Tertiary Education Level for Women (USA)", xlab="Years", ylab="Percentage of Women")

USA_data_UPPSRY_WOMEN <- USA_data[USA_data$SUBJECT=="UPPSRY_WOMEN", ]
USA_UPPSRY_change_WOMEN <- plot(USA_data_UPPSRY_WOMEN$Value~USA_data_UPPSRY_WOMEN$TIME, main="Upper Secondary Education Level for Women (USA)", xlab="Years", ylab="Percentage of Women")

USA_TRY_change_GENDER <- plot(USA_data_TRY_WOMEN$Value~USA_data_TRY_WOMEN$TIME, main="Gendered Change in Tertiary Education Level (USA)", xlab="Years", ylab="Percentage of Adults", type="l", col="red", xlim=c(1980, 2020))
lines(USA_data_TRY_MEN$Value~USA_data_TRY_MEN$TIME, col="blue")
legend("topleft",
       c("Men","Women"),
       fill=c("blue","red"))

USA_UPPSRY_change_GENDER <- plot(USA_data_UPPSRY_WOMEN$Value~USA_data_UPPSRY_WOMEN$TIME, main="Gendered Change in Upper Secondary Education Level (USA)", xlab="Years", ylab="Percentage of Adults", type="l", col="red", xlim=c(1980, 2020))
lines(USA_data_UPPSRY_MEN$Value~USA_data_UPPSRY_MEN$TIME, col="blue")
legend("topright",
       c("Men","Women"),
       fill=c("blue","red"))

USA_change <- plot(USA_data_UPPSRY$Value~USA_data_UPPSRY$TIME, main="Education Level Changes (USA)", xlab="Years", ylab="Percentage of Adults", type="l", col="red", xlim=c(1980, 2020), ylim=c(0, 80))
lines(USA_data_BUPPSRY$Value~USA_data_BUPPSRY$TIME, col="blue")
lines(USA_data_TRY$Value~USA_data_TRY$TIME, col="green")
legend("topright",
       c("Below Upper Secondary","Upper Secondary", "Tertiary"),
       fill=c("blue","red", "green"))

############ world data distributions ############
world_data_TRY <- adult_education_data[adult_education_data$SUBJECT=="TRY", ]
world_TRY_change <- plot(world_data_TRY$Value~world_data_TRY$TIME, main="Tertiary Education Level (world)", xlab="Years", ylab="Percentage of Adults", xlim=c(1980, 2020))

world_data_UPPSRY <- adult_education_data[adult_education_data$SUBJECT=="UPPSRY", ]
world_UPPSRY_change <- plot(world_data_UPPSRY$Value~world_data_UPPSRY$TIME, main="Upper Secondary Education Level (world)", xlab="Years", ylab="Percentage of Adults")

world_data_BUPPSRY <- adult_education_data[adult_education_data$SUBJECT=="BUPPSRY", ]
world_BUPPSRY_change <- plot(world_data_BUPPSRY$Value~world_data_BUPPSRY$TIME, main="Below Upper Secondary Education Level (world)", xlab="Years", ylab="Percentage of Adults")

world_data_TRY_MEN <- adult_education_data[adult_education_data$SUBJECT=="TRY_MEN", ]
world_TRY_change_MEN <- plot(world_data_TRY_MEN$Value~world_data_TRY_MEN$TIME, main="Tertiary Education Level for Men (world)", xlab="Years", ylab="Percentage of Men")

world_data_UPPSRY_MEN <- adult_education_data[adult_education_data$SUBJECT=="UPPSRY_MEN", ]
world_UPPSRY_change_MEN <- plot(world_data_UPPSRY_MEN$Value~world_data_UPPSRY_MEN$TIME, main="Upper Secondary Education Level for Men (world)", xlab="Years", ylab="Percentage of Men")

world_data_TRY_WOMEN <- adult_education_data[adult_education_data$SUBJECT=="TRY_WOMEN", ]
world_TRY_change_WOMEN <- plot(world_data_TRY_WOMEN$Value~world_data_TRY_WOMEN$TIME, main="Tertiary Education Level for Women (world)", xlab="Years", ylab="Percentage of Women")

world_data_UPPSRY_WOMEN <- adult_education_data[adult_education_data$SUBJECT=="UPPSRY_WOMEN", ]
world_UPPSRY_change_WOMEN <- plot(world_data_UPPSRY_WOMEN$Value~world_data_UPPSRY_WOMEN$TIME, main="Upper Secondary Education Level for Women (world)", xlab="Years", ylab="Percentage of Women")

world_TRY_change_GENDER <- plot(world_data_TRY_WOMEN$Value~world_data_TRY_WOMEN$TIME, main="Gendered Change in Tertiary Education Level (world)", xlab="Years", ylab="Percentage of Adults", type="l", col="red", xlim=c(1980, 2020))
lines(world_data_TRY_MEN$Value~world_data_TRY_MEN$TIME, col="blue")
legend("topleft",
       c("Men","Women"),
       fill=c("blue","red"))

world_UPPSRY_change_GENDER <- plot(world_data_UPPSRY_WOMEN$Value~world_data_UPPSRY_WOMEN$TIME, main="Gendered Change in Upper Secondary Education Level (world)", xlab="Years", ylab="Percentage of Adults", type="l", col="red", xlim=c(1980, 2020))
lines(world_data_UPPSRY_MEN$Value~world_data_UPPSRY_MEN$TIME, col="blue")
legend("topright",
       c("Men","Women"),
       fill=c("blue","red"))

world_change <- plot(world_data_UPPSRY$Value~world_data_UPPSRY$TIME, main="Education Level Changes (world)", xlab="Years", ylab="Percentage of Adults", type="l", col="red", xlim=c(1980, 2020), ylim=c(0, 80))
lines(world_data_BUPPSRY$Value~world_data_BUPPSRY$TIME, col="blue")
lines(world_data_TRY$Value~world_data_TRY$TIME, col="green")
legend("topright",
       c("Below Upper Secondary","Upper Secondary", "Tertiary"),
       fill=c("blue","red", "green"))

########### distribution plots ###########
#world data histogram
hist(Value, main="Frequency of Education Levels in the World", xlab="Percentages of Educated Adults")   #initial reading: many countries have only 10%-50% of adults that are educated

#US data histogram
hist(USA_data$Value, main="Frequency of Education Levels in the United States", xlab="Percentages of Educated Adults", xlim=c(10, 65))

#other exploratory analysis
USA_values_1981 <- USA_data[USA_data$TIME==1981, ]$Value
hist(USA_values_1981)

USA_values_2018 <- USA_data[USA_data$TIME==2018, ]$Value
hist(USA_values_2018)

#world boxplots
boxplot(adult_education_data$Value)
boxplot(world_data_BUPPSRY$Value, main="World Below Secondary Education Distribution", xlab="Percentages of Adults", ylab="Frequency")
boxplot(world_data_UPPSRY$Value, main="World Upper Secondary Education Distribution", xlab="Percentages of Adults", ylab="Frequency")
boxplot(world_data_TRY$Value, main="World Tertiary Education Distribution", xlab="Percentages of Adults", ylab="Frequency")

boxplot(world_data_UPPSRY_MEN$Value, main="World Upper Secondary Education Distribution (Men)", xlab="Percentages of Adults", ylab="Frequency")
boxplot(world_data_UPPSRY_WOMEN$Value, main="World Upper Secondary Education Distribution (Women)", xlab="Percentages of Adults", ylab="Frequency")
boxplot(world_data_TRY_MEN$Value, main="World Tertiary Education Distribution (Men)", xlab="Percentages of Adults", ylab="Frequency")
boxplot(world_data_TRY_WOMEN$Value, main="World Tertiary Education Distribution (Women)", xlab="Percentages of Adults", ylab="Frequency")

#US boxplots
boxplot(USA_data$Value, xlab="Percentages of Adults", ylab="Frequency")
boxplot(USA_data_BUPPSRY$Value, main="United States Below Secondary Education Distribution", xlab="Percentages of Adults", ylab="Frequency")
boxplot(USA_data_UPPSRY$Value, main="United States Upper Secondary Education Distribution", xlab="Percentages of Adults", ylab="Frequency")
boxplot(USA_data_TRY$Value, main="United States Tertiary Education Distribution", xlab="Percentages of Adults", ylab="Frequency")

boxplot(USA_data_UPPSRY_MEN$Value, main="United States Upper Secondary Education Distribution (Men)", xlab="Percentages of Adults", ylab="Frequency")
boxplot(USA_data_UPPSRY_WOMEN$Value, main="United States Upper Secondary Education Distribution (Women)", xlab="Percentages of Adults", ylab="Frequency")
boxplot(USA_data_TRY_MEN$Value, main="United States Tertiary Education Distribution (Men)", xlab="Percentages of Adults", ylab="Frequency")
boxplot(USA_data_TRY_WOMEN$Value, main="United States Tertiary Education Distribution (Women)", xlab="Percentages of Adults", ylab="Frequency")






