setwd("C:/My_Stuff/Work/R/DiscountTrends") #Change this to where you have placed the csv file
predata <- read.csv("Discount_data.csv", head = TRUE, sep = ",") #Load file
#Keep required columns (created, total, original, discount, deliveryCharge, paymentMode, state, style, and day)
data <- predata[, c(2, 3, 4, 5, 7, 10, 11, 12)] 
data <- data[! data$state %in% c("cancelled"),] #Remove all the cancelled orders
data <- data[! data$paymentMode %in% c("#N/A"),] #Remove all the #N/A fields from paymentMode
data$day <- weekdays(as.Date(data$created)) #Generate day column according to the created date
data <- data[1:748,] #Remove last row

#SVM
install.packages("e1071")
library(e1071)

#Split into training and testing set
index <- 1:nrow(data)
testindex <- sample(index, trunc(length(index)/3))
testset <- data[testindex,]
trainset <- data[-testindex,]

#Begin Machine learning
svm.model <- svm(as.factor(day) ~ ., data = trainset) #TODO: check for other features
svm.pred <- predict(svm.model, testset[, -9]) #day is column 9
table(pred = svm.pred, true = testset[,9]) #Confusion matrix
summary(svm.model)

#Retrieve accuracy
install.packages("caret")
library(caret)
confMat <- confusionMatrix(svm.pred, testset$day) #Fancy confusion matrix
accuracy <- confMat$overall[['Accuracy']]