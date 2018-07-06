
#install.packages("rminer")
library(rminer)
library("caret")
library(kernlab)
library("ggplot2")
library("e1071")
library(gridExtra)

tmp <- file.choose()
tmp
myData <- read.csv(tmp, header = TRUE, stringsAsFactors = TRUE)
colnames(myData)

#Classify supporters and detractors
unique(myData$NPS_Type)
NPS_subset <- myData[myData$NPS_Type != "Passive",]
NPS_subset$NPS_Type <- factor(NPS_subset$NPS_Type)
unique(NPS_subset$NPS_Type)

#Subsetting the columns for ammenities
NPS_subset <- NPS_subset[,c("Mini.Bar_PL","Pool.Indoor_PL","Pool.Outdoor_PL","Regency.Grand.Club_PL","Resort_PL","Restaurant_PL","Self.Parking_PL","Shuttle.Service_PL","Ski_PL","Spa.online.booking_PL","Spa_PL","Spa.services.in.fitness.center_PL","NPS_Type")]

#Converting to NA blank spaces
NPS_subset[NPS_subset==""]  <- "N"

#creating training and testing
#Shuffle dataset
randIndex <- sample(1:dim(NPS_subset)[1])

#calculate "cut point"
cutPoint2_3 <- floor(2 * dim(NPS_subset)[1]/3)
cutPoint2_3

#Training data
trainData <- NPS_subset[randIndex[1:cutPoint2_3], ]

#Testing data
testData <- NPS_subset[randIndex[(cutPoint2_3 + 1):dim(NPS_subset)[1]],]


#1)	Build a model (using the 'ksvm' function, trying to predict onzone). You can use all the possible attributes, or select the attributes that you think would be the most helpful.
svmOutput <- ksvm(NPS_Type ~ ., data = trainData, kernel = "rbfdot", kpar ="automatic", C = 50, cross = 3, prob.model = TRUE)
svmOutput


ksvmPred <- predict(svmOutput, testData)
str(ksvmPred)
compTable <- data.frame(testData[,13],ksvmPred)
colnames(compTable) <- c("test", "Pred")
#Compute the percentage of correct cases
perc_ksvm <- length(which(compTable$test==compTable$Pred))/dim(compTable)[1]
perc_ksvm

modelTree <- fit(NPS_Type ~ ., data = trainData,model="ctree")
plot(modelTree@object) # show model
