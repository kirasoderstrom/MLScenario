train = read.csv("TrainData.csv", header = FALSE, stringsAsFactors = FALSE) #upload data to CSV
test = read.csv("TestData.csv", header = FALSE, stringsAsFactors = FALSE) #upload data to CSV

#off upload of the first input for each csv- atypical behavior- would like to look into this further
train[1,1]<-61.2 
test[1,1]<-64.2 

#separate test and training data from the classifications
trainX <-train[,1:64]
trainY <-train[,65]

testX <- test[,1:64]
testY <- test[,65]

#make sure the inputs are of numeric data class 
trainX = as.matrix(as.data.frame(lapply(trainX, as.numeric)))
testX = as.matrix(as.data.frame(lapply(testX, as.numeric)))

#breakUpTestCriteria by X,Y,W,Z

XcriteriaTrain <-ifelse(grepl("X", train[,65]), "X", "0") 
YcriteriaTrain <-ifelse(grepl("Y", train[,65]), "Y", "0")
WcriteriaTrain <-ifelse(grepl("W", train[,65]), "W", "0")
ZcriteriaTrain <-ifelse(grepl("Z", train[,65]), "Z", "0")

trainYy <-data.frame(X = XcriteriaTrain, Y = YcriteriaTrain, W = WcriteriaTrain, Z = ZcriteriaTrain)

XcriteriaTest <-ifelse(grepl("X", test[,65]), "X", "0")
YcriteriaTest <-ifelse(grepl("Y", test[,65]), "Y", "0")
WcriteriaTest <-ifelse(grepl("W", test[,65]), "W", "0")
ZcriteriaTest <-ifelse(grepl("Z", test[,65]), "Z", "0")

testYy <-data.frame(X = XcriteriaTest, Y = YcriteriaTest, W = WcriteriaTest, Z = ZcriteriaTest)

predictionYy <-data.frame(X = XcriteriaTrain, Y = YcriteriaTrain, W = WcriteriaTrain, Z = ZcriteriaTrain) #use this later on to determine overall success if the classification criteria is broken up 

#trainPCA based off of train data
train.pca <- prcomp(trainX, center = TRUE, scale. = TRUE)  
trainPCAS<-train.pca$x

#predict test PCAs based off of training pca algorithm 
testPCAS<-predict(train.pca, newdata=testX)

summary(train.pca)
plot(train.pca)

#15 covered a good amount based off of the cumulative proprotion of variance
#I would like to automatically set the weights based on some evaulation criteria so this part is automatic 
featureSetTrain<-trainPCAS[,1:15] 
featureSetTest<-testPCAS[,1:15]

install.packages("e1071") #svm package
library(e1071)

#Creating separate SVM models for each success criteria. Either grouping them all together or separating them out to combine later 
trainAll <- data.frame(Y = trainY, featureSetTrain) 
trainXv <- data.frame(Y = XcriteriaTrain, featureSetTrain) 
trainYv <- data.frame(Y = YcriteriaTrain, featureSetTrain) 
trainWv <- data.frame(Y = WcriteriaTrain, featureSetTrain) 
trainZv <- data.frame(Y = ZcriteriaTrain, featureSetTrain) 

#Identify tuning optimized metrics for overarching model
#use the obj$
obj <- tune.svm(Y ~ ., data = trainAll,   kernel = "radial")
obj$best.model

#Training model
model_svm <- svm(Y ~ ., trainAll, scale = TRUE, kernel="radial", gamma=.066666666, cost = 1) # metrics based on tuning results
model_svm_X <- svm(Y ~ ., trainXv)
model_svm_Y <- svm(Y ~ ., trainYv)
model_svm_W <- svm(Y ~ ., trainWv)
model_svm_Z <- svm(Y ~ ., trainZv)

#Use the predictions on the data
pred <- predict(model_svm, featureSetTest) #overall result without breaking independent classifiers out
predx <- predict(model_svm_X, featureSetTest)
predy <- predict(model_svm_Y, featureSetTest)
predw <- predict(model_svm_W, featureSetTest)
predz <- predict(model_svm_Z, featureSetTest)

##for overall result without breaking it down by independent classifiers (pred is the prediction without breaking it out)

RESULTS <-data.frame(test = pred, testY)
sum(RESULTS[,1]==RESULTS[,2])

RESULTSX <-data.frame(test = predx)
RESULTSY <-data.frame(test = predy)
RESULTSW <-data.frame(test = predw)
RESULTSZ <-data.frame(test = predz)

#confusion matrix printout
conf_matrix <- table(precitcion = pred, actual = testY)
print(conf_matrix)

#prediction set to compare to test set 
predictionYy <-data.frame(X = RESULTSX, Y = RESULTSY, W = RESULTSW, Z = RESULTSZ)

#total correct 
totalBooleanCorrect<-sum(predictionYy[,1:4]==testYy[,1:4])
#total values 
totalBooleanValues<-dim(predictionYy)[1]*dim(predictionYy)[2]
#percent success
totalBooleanCorrect/totalBooleanValues*100 #Without setting this should be about 83%

#view the combination quantity right out of 4 total classifers. Keeping in mind the binary selection toggle to be present or absent on each classification. 
vec <- vector()
i<-1
for(i in 1:(dim(predictionYy)[1])){
  vec[i]<-sum(predictionYy[i,1:4]==testYy[i,1:4])
}
vec <- sort(vec,decreasing = TRUE)

#confusion matrices printout per variable if broken up by classifer
conf_matrix_x <- table(precitcion = predictionYy[,1], actual = testYy[,1])
conf_matrix_y <- table(precitcion = predictionYy[,2], actual = testYy[,2])
conf_matrix_w <- table(precitcion = predictionYy[,3], actual = testYy[,3])
conf_matrix_z <- table(precitcion = predictionYy[,4], actual = testYy[,4])
print(conf_matrix_x)
print(conf_matrix_y)
print(conf_matrix_w)
print(conf_matrix_z)
