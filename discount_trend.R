#install.packages("e1071")
#install.packages("caret")
library(caret)
library(e1071)

#setwd("C:/My_Stuff/Work/R/DiscountTrends") #Change this to where you have placed the csv file
predata <- read.csv("Discount_data.csv", head = TRUE, sep = ",") #Load file
#Keep required columns (created, total, original, discount, deliveryCharge, paymentMode, state, style, and day)
data <- predata[, c(2, 3, 4, 5, 7, 10, 11, 12)] 
data <- data[! data$state %in% c("cancelled"),] #Remove all the cancelled orders
data <- data[! data$paymentMode %in% c("#N/A"),] #Remove all the #N/A fields from paymentMode
data$day <- weekdays(as.Date(data$created)) #Generate day column according to the created date
data <- data[1:748,] #Remove last row

accuracy <- numeric()

#Cross-fold validation
data <- data[sample(nrow(data)),] #shuffles dataset
folds <- cut(seq(1, nrow(data)), breaks = 10, labels = FALSE) #Create 10 equally sized folds

for(i in 1:10) {
	#Split into training and testing set
	testindex <- which(folds == i, arr.ind = TRUE)
	testset <- data[testindex,]
	trainset <- data[-testindex,]

	#Begin Machine learning
	svm.model <- svm(as.factor(day) ~ ., data = trainset, kernel='linear') #TODO: check for other features
	svm.pred <- predict(svm.model, testset[, -9]) #day is column 9
	table(pred = svm.pred, true = testset[,9]) #Confusion matrix
	summary(svm.model)

	#Retrieve accuracy
	confMat <- confusionMatrix(svm.pred, testset$day) #Fancy confusion matrix
	temp <- confMat$overall[['Accuracy']]
	accuracy <- append(accuracy,temp)
}

finalAccuracy <- round(mean(accuracy) * 100, digits = 2)