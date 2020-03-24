# Lab1_ctree2
pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")
require(party)
swiss_ctree <- ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_ctree)

# Lab1_kknn1
require(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
              prob = rep(1/m, m)) 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
                  kernel = "triangular")
summary(iris.kknn)
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")[(iris.valid$Species != fit)+1])

# Lab1_kknn2
require(kknn)
data(ionosphere)
ionosphere.learn <- ionosphere[1:200,]
ionosphere.valid <- ionosphere[-c(1:200),]
fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
table(ionosphere.valid$class, fit.kknn$fit)
(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)

# Lab1_kknn3
data(swiss)
pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")

# Lab1_kmeans1
data(swiss)
sclass <- kmeans(swiss[2:6], 3) 
table(sclass$cluster, swiss[,1])  

# Lab1_nyt
nyt1<-read.csv("~/Dropbox/DataAnalytics/Assignment3/dds_ch2_nyt/nyt1.csv")
nyt1<-nyt1[which(nyt1$Impressions>0 & nyt1$Clicks>0 & nyt1$Age>0),]
nnyt1<-dim(nyt1)[1]		# shrink it down!
sampling.rate=0.9
num.test.set.labels=nnyt1*(1.-sampling.rate)
training <-sample(1:nnyt1,sampling.rate*nnyt1, replace=FALSE)
train<-subset(nyt1[training,],select=c(Age,Impressions))
testing<-setdiff(1:nnyt1,training)
test<-subset(nyt1[testing,],select=c(Age,Impressions))
cg<-nyt1$Gender[training]
true.labels<-nyt1$Gender[testing]
library(class)
knn(train,test,cg,k=5)
attributes(.Last.value)

# Titanic rpart
library(rpart)
library(rpart.plot)

data(Titanic)
require(rpart)
Titanic_rpart <- rpart(Survived ~ ., data=Titanic)
plot(Titanic_rpart) # try some different plot options
text(Titanic_rpart) # try some different text options

library(rattle)   #using rattle for a different plotting option
fancyRpartPlot(Titanic_rpart)

#Titanic ctree
require(party)

Titanic_ctree<-ctree(Survived ~ ., data=Titanic)
plot(Titanic_ctree)

#Titanic hclust
require(graphics)
Titanic_hclust <- hclust(dist(Titanic), "ave")
plot(Titanic_hclust)

#Titanic randomForest
library(randomForest)
set.seed(100)
train <- sample(nrow(Titanic), 0.7*nrow(Titanic), replace=FALSE)
TrainSet <- Titanic[train]
ValidSet <- Titanic[-train]

forest_model <- randomForest(Survived ~ ., data=TrainSet, importance=TRUE)
forest_model

forest_model2 <- randomForest(Survived ~ ., data=TrainSet, ntree=500, mtry=6, importance=TRUE)
forest_model2

predTrain <- predict(forest_model2, TrainSet, type="class")
table(predTrain, TrainSet$Survived)

predValid <- predict(forest_model2, ValidSet, type="class")
table(predValid, ValidSet$Survived)




