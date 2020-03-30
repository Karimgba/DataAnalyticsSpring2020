#model 3 work based on in class examples of models 1/2
library(e1071)
help("svm")
data("iris")

#make model 3
svm_model3 <- svm(Species~., data = iris, kernel="polynomial")

summary(svm_model3)

#plot the model 3
plot(svm_model3, data = iris,
     Petal.Width~Petal.Length, slice =
       list(Sepal.Width = 3, Sepal.Length = 4))

#predict using model 3
pred <- predict(svm_model3, iris)

#make a table for predictions
model_table <- table(Predicted = pred, Actual = iris$Species)
model_table

#find accuracy rate for model 3
Model3_accuracyRate = sum(diag(model_table))/sum(model_table)
Model3_accuracyRate     #rate=0.9533333

#find the missclassification rate for model 3
Model3_MissClassificationRate = 1 - Model3_accuracyRate
Model3_MissClassificationRate     #rate=0.04666667

