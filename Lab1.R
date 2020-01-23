#RPI weather exercise
days <- c('Mon', 'Tues', 'Wed', 'Thur','Fri', 'Sat', 'Sun')
temp <- c(28, 30.5, 32, 31.2, 29.3, 27.9, 26.4)
snowed <- c('T', 'T', 'F', 'F', 'T', 'T', 'F')
RPI_Weather <- data.frame(days, temp, snowed)     #make the data.frame

RPI_Weather
head(RPI_Weather)     #look at the head of the data.frame

str(RPI_Weather)

summary(RPI_Weather)

#Data frame specifics
RPI_Weather[1,]       #shows all columns for 1st row
RPI_Weather[,1]       #shows all rows for 1st column
RPI_Weather[,'snowed']       #shows all rows for 'snowed' column
RPI_Weather[1:5, c('days', 'temp')]     #shows first 5 rows for days and temp cols
RPI_Weather$temp    #shows all entries for 'temp' column
subset(RPI_Weather, subset = snowed==TRUE)

sorted.snowed <- order(RPI_Weather['snowed'])   #sorts 'snowed' col
sorted.snowed
RPI_Weather[sorted.snowed,]     #shows data sorted by 'snowed' col

dec.snow <- order(RPI_Weather$temp)     #sort 'temp' col
dec.snow

empty.DataFrame <- data.frame()     #make an empty data frame

vector1 <- letters[1:10]     #making a subset vector from letters
vector1

#CSV reading data exercise
EPI_data <-read.csv(file.choose(), header=T)
EPI_data
attach(EPI_data)
head(EPI_data)

summary(PopulationDensity)     #practicing plotting population density data
fivenum(PopulationDensity, na.rm=T)
pop_den_boxplot <- boxplot(PopulationDensity)
pop_den_boxplot 
pop_den_hist <- hist(PopulationDensity)
pop_den_hist
pop_den_stem <- stem(PopulationDensity)
pop_den_stem

summary(EPI)       #practicing plotting EPI column data
fivenum(EPI, na.rm=T)
EPI_boxplot <- boxplot(EPI)
EPI_boxplot
EPI_hist <- hist(EPI)
EPI_hist
EPI_stem <- stem(EPI)
EPI_stem

#From plotting both of these columns, I can tell that the EPI data is more readable as a boxplot and histogram.
#There is more spread data than in the PopulationDensity column.

EPI_hist_seq <- hist(EPI, seq(30., 95., 1.0), prob=TRUE)       #hist with defined range
EPI_hist_seq
lines(density(EPI,na.rm=TRUE,bw=1.))       #adds density data to hist
help(rug)
rug(EPI)       #gives a 1D representation of data

#Distribution beyond histograms
EPI_ecdf <- plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
EPI_ecdf

par(pty="s")
qqnorm(EPI); qqline(EPI)    #normal qq plot
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

#More practice with EPI data columns
summary(AIR_H)       #practicing plotting AIR_H column data
fivenum(AIR_H, na.rm=T)
AIR_H_boxplot <- boxplot(AIR_H)
AIR_H_boxplot
AIR_H_hist <- hist(AIR_H)
AIR_H_hist
AIR_H_stem <- stem(AIR_H)
AIR_H_stem

AIR_H_hist_seq <- hist(AIR_H, seq(30., 95., 1.0), prob=TRUE)       #hist with defined range
AIR_H_hist_seq
lines(density(AIR_H,na.rm=TRUE,bw=1.))       #adds density data to hist
help(rug)
rug(AIR_H)       #gives a 1D representation of data

AIR_H_ecdf <- plot(ecdf(AIR_H), do.points=FALSE, verticals=TRUE)
AIR_H_ecdf

par(pty="s")
qqnorm(AIR_H); qqline(AIR_H)    #normal qq plot

summary(WATER_H)       #practicing plotting WATER_H column data
fivenum(WATER_H, na.rm=T)
WATER_H_boxplot <- boxplot(WATER_H)
WATER_H_boxplot
WATER_H_hist <- hist(WATER_H)
WATER_H_hist
WATER_H_stem <- stem(WATER_H)
WATER_H_stem

WATER_H_hist_seq <- hist(WATER_H, seq(30., 95., 1.0), prob=TRUE)       #hist with defined range
WATER_H_hist_seq
lines(density(WATER_H,na.rm=TRUE,bw=1.))       #adds density data to hist
help(rug)
rug(WATER_H)       #gives a 1D representation of data

WATER_H_ecdf <- plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE)
WATER_H_ecdf

par(pty="s")
qqnorm(WATER_H); qqline(WATER_H)    #normal qq plot

#Comparing distributions
boxplot(EPI,DALY)
qqplot(EPI,DALY)

boxplot(WATER_H,AIR_H)
qqplot(WATER_H,AIR_H)

#Filtering

