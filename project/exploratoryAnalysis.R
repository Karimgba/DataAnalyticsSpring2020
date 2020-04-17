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

hist(Value, main="Frequency of Education Levels in the World", xlab="Percentages of Educated Adults")   #initial reading: many countries have only 10%-50% of adults that are educated
#need to break down by country, year, edu level

USA_data <- adult_education_data[adult_education_data$LOCATION=="USA", ]
USA_values_1981 <- USA_data[USA_data$TIME==1981, ]$Value
hist(USA_values_1981)

USA_values_2018 <- USA_data[USA_data$TIME==2018, ]$Value

boxplot(USA_data$Value)
boxplot(USA_values_1981, USA_values_2018, names=c("1981", "2018"), ylab="Years", xlab="Percentage of Adults", horizontal = TRUE)

USA_data_TRY <- USA_data[USA_data$SUBJECT=="TRY", ]   #MADE THIS A LINE
boxplot(USA_data_TRY$Value)
USA_TRY_change <- plot(USA_data_TRY$Value~USA_data_TRY$TIME, main="Tertiary Education Level (USA)", xlab="Years", ylab="Percentage of Adults", type="l")
        #there is a notable drop in 1992 for college educated ppl (double check what tertiary means)

USA_data_UPPSRY <- USA_data[USA_data$SUBJECT=="UPPSRY", ]
USA_UPPSRY_change <- plot(USA_data_UPPSRY$Value~USA_data_UPPSRY$TIME, main="Upper Secondary Education Level (USA)", xlab="Years", ylab="Percentage of Adults")
#there is a notable drop in 1992 for upper secondary educated ppl (double check what that means)

USA_data_BUPPSRY <- USA_data[USA_data$SUBJECT=="BUPPSRY", ]
USA_BUPPSRY_change <- plot(USA_data_BUPPSRY$Value~USA_data_BUPPSRY$TIME, main="Below Secondary Education Level (USA)", xlab="Years", ylab="Percentage of Adults")
#there is a notable drop in 1992 for below secondary educated ppl (double check what that means)

USA_data_TRY_MEN <- USA_data[USA_data$SUBJECT=="TRY_MEN", ]
USA_TRY_change_MEN <- plot(USA_data_TRY_MEN$Value~USA_data_TRY_MEN$TIME, main="Tertiary Education Level for Men (USA)", xlab="Years", ylab="Percentage of Men")
#there is a notable drop in 1992 for college educated ppl (double check what tertiary means)

USA_data_UPPSRY_MEN <- USA_data[USA_data$SUBJECT=="UPPSRY_MEN", ]
USA_UPPSRY_change_MEN <- plot(USA_data_UPPSRY_MEN$Value~USA_data_UPPSRY_MEN$TIME, main="Upper Secondary Education Level for Men (USA)", xlab="Years", ylab="Percentage of Men")
#there is a notable drop in 1992 for upper secondary educated ppl (double check what that means)

USA_data_TRY_WOMEN <- USA_data[USA_data$SUBJECT=="TRY_WOMEN", ]
USA_TRY_change_WOMEN <- plot(USA_data_TRY_WOMEN$Value~USA_data_TRY_WOMEN$TIME, main="Tertiary Education Level for Women (USA)", xlab="Years", ylab="Percentage of Women")
#there is a notable drop in 1992 for college educated ppl (double check what tertiary means)

USA_data_UPPSRY_WOMEN <- USA_data[USA_data$SUBJECT=="UPPSRY_WOMEN", ]
USA_UPPSRY_change_WOMEN <- plot(USA_data_UPPSRY_WOMEN$Value~USA_data_UPPSRY_WOMEN$TIME, main="Upper Secondary Education Level for Women (USA)", xlab="Years", ylab="Percentage of Women")
#there is a notable drop in 1992 for upper secondary educated ppl (double check what that means)

USA_TRY_change_GENDER <- plot(USA_data_TRY_WOMEN$Value~USA_data_TRY_WOMEN$TIME, main="Gendered Change in Tertiary Education Level (USA)", xlab="Years", ylab="Percentage of Adults", type="l", col="red", xlim=c(1980, 2020))
#there is a notable drop in 1992 for college educated ppl (double check what tertiary means)
lines(USA_data_TRY_MEN$Value~USA_data_TRY_MEN$TIME, col="blue")
legend("topleft",
       c("Men","Women"),
       fill=c("blue","red"))

USA_UPPSRY_change_GENDER <- plot(USA_data_UPPSRY_WOMEN$Value~USA_data_UPPSRY_WOMEN$TIME, main="Gendered Change in Upper Secondary Education Level (USA)", xlab="Years", ylab="Percentage of Adults", type="l", col="red", xlim=c(1980, 2020))
#there is a notable drop in 1992 for college educated ppl (double check what tertiary means)
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

world_data_1981_TRY <- filter(adult_education_data, (adult_education_data$TIME==1981 & adult_education_data$SUBJECT=="TRY"))

world_data_1981_TRY_plot <- barplot(world_data_1981_TRY$Value, names.arg=world_data_1981_TRY$LOCATION, main="World Tertiary Education Level Percentages in 1981", xlab="Countries", ylab="Percentage of Adults", ylim=c(0, 30))



world_data_2018_TRY <- filter(adult_education_data, (adult_education_data$TIME==2018 & adult_education_data$SUBJECT=="TRY" & adult_education_data$LOCATION!="OAVG"))

world_data_2018_TRY_plot <- barplot(world_data_2018_TRY$Value, names.arg=world_data_2018_TRY$LOCATION, main="World Tertiary Education Level Percentages in 2018", xlab="Countries", ylab="Percentage of Adults", ylim=c(0, 50))

plot(world_data_2018_TRY$Value, names.arg=world_data_2018_TRY$LOCATION)





library(dplyr)
USA_values_1981_TRY <- filter(USA_data, (USA_data$TIME==1981 & USA_data$SUBJECT=="TRY"))
USA_values_1981_TRY
USA_years <- adult_education_data[adult_education_data$LOCATION=="USA",]$TIME

USA_data <- data.frame(USA_values, USA_years)
USA_data

hist(USA_values)
plot(USA_data$USA_values~USA_data$USA_years)








