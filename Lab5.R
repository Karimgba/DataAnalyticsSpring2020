#Lab work with wine dataset
#had trouble reading in dataset so downloaded it locally
#wine_data <- read.table("http://archive.ics.uci.edu/ml/machinelearning-databases/wine/wine.data", sep = ",")
wine_data <- read.table(file.choose(), sep = ",", header = FALSE)
head(wine_data)

nrow(wine_data) # there are 178 rows
dim(wine_data)  #dimension is 14

colnames(wine_data) <- c("Cvs", "Alcohol",
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash",
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine",
                         "Proline")
head(wine_data)
help("heatmap")
heatmap(cor(wine_data),Rowv = NA, Colv = NA) 

cultivar_classes <- factor(wine_data$Cvs)
cultivar_classes

help(scale)
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)

