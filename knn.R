library(ggplot2)
library(class)
library(caTools)
library(ElemStatLearn)

# Read in the Data set
socialNetworkAds <- read.csv("Social_Network_Ads.csv")
head(socialNetworkAds)
socialNetworkAds <- socialNetworkAds[3:5]

ggplot() + geom_point(aes(x = Age, y = EstimatedSalary), data = socialNetworkAds)

# Split the data into test and train sets
set.seed(505)
splitData <- sample.split(socialNetworkAds$Purchased, SplitRatio = 0.75)
trainingData <- subset(socialNetworkAds, splitData == TRUE)
testingData <- subset(socialNetworkAds, splitData == FALSE)

# Feature Scaling
trainingData[-3] <- scale(trainingData[-3])
testingData[-3] <- scale(testingData[-3])

# Train K-nn model and Test
yPred <- knn(train = trainingData[, -3], test = testingData[, -3], cl = trainingData[, 3], k = 5, prob = TRUE)
table(testingData[, 3], yPred)

# Visualising the Training set results
set = trainingData
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
gridSet = expand.grid(X1, X2)
colnames(gridSet) = c('Age', 'EstimatedSalary')
yGrid = knn(train = trainingData[, -3], test = gridSet, cl = trainingData[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(yGrid), length(X1), length(X2)), add = TRUE)
points(gridSet, pch = '.', col = ifelse(yGrid == 1, 'coral', 'cornflowerblue'))
points(set, pch = 21, cex = 1.2, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
set = testingData
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
gridSet = expand.grid(X1, X2)
colnames(gridSet) = c('Age', 'EstimatedSalary')
yGrid = knn(train = trainingData[, -3], test = gridSet, cl = trainingData[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(yGrid), length(X1), length(X2)), add = TRUE)
points(gridSet, pch = '.', col = ifelse(yGrid == 1, 'coral', 'cornflowerblue'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))