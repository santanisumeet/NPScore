library("caret")
#Linear model code

#Reading the dataset
tmp <- file.choose()
tmp
myData <- read.csv(tmp, header = TRUE, stringsAsFactors = TRUE)

#Deleting NA
myData <- na.omit(myData)
myData <- myData[complete.cases(myData),]

#Linear Models (General)

#Demographic information
#Using age, children and gener as independent variables against NPS_TYPE
#First we need to convert the columns to numeric types
#Age
unique(myData$Age_Range_H)
table(myData$Age_Range_H)
myData$Age_Range_H <- sapply(myData$Age_Range_H, as.numeric)
#Ignoring white spaces
myData <- myData[myData$Age_Range_H != 1,]
#"" -> 1
#18 - 25 -> 2
#26-35 -> 3
#36 - 45 -> 4 
#46 - 55 -> 5 
#56 - 65 -> 6
#66 - 75 -> 7
#76+ -> 8

#Gender
unique(myData$Gender_H)
table(myData$Gender_H)
myData$Gender_H <- sapply(myData$Gender_H, as.numeric)
#Ignoring white spaces and "Prefer not to answer"
myData <- myData[myData$Gender_H != 1,]
myData <- myData[myData$Gender_H != 4,]
#"" -> 1
#Female -> 2
#Male -> 3
#Prefer not to answe -> 4

#Childs
unique(myData$CHILDREN_NUM_C)
table(myData$CHILDREN_NUM_C)

#City
unique(myData$City_PL)
sort(table(myData$City_PL))
myData$City_PL <- sapply(myData$City_PL, as.numeric)

#State
unique(myData$State_PL)
sort(table(myData$State_PL))
myData$State_PL <- sapply(myData$State_PL, as.numeric)
#Ignoring white spaces
myData <- myData[myData$State_PL != 1,]

#Country
unique(myData$Country_PL)
sort(table(myData$Country_PL))
myData$Country_PL <- sapply(myData$Country_PL, as.numeric)

unique(myData$Currency_PL)
table(myData$Currency_PL)
myData$Currency_PL<-sapply(myData$Currency_PL, as.numeric)

#NPS_TYPE 
unique(myData$NPS_Type)
table(myData$NPS_Type)
myData$NPS_Type <- sapply(myData$NPS_Type, as.numeric)
#Detractors -> 1
#Passive -> 2
#Promoter -> 3
unique(myData$Overall_Sat_H)

#Testing the linear Demographic carachteristics
model <- lm(formula = Overall_Sat_H ~ CHILDREN_NUM_C + Gender_H + Age_Range_H + City_PL + Country_PL + Currency_PL + State_PL, data = myData)
summary(model)

plot(myData$City_PL + myData$State_PL +myData$Country_PL + myData$Currency_PL + myData$Gender_H + myData$CHILDREN_NUM_C + myData$Gender_H, myData$Overall_Sat_H, xlab = "Demographic variables", ylab = "Overall satisfaction", pch = 19, xlim = c(0,100), ylim = c(8,10))
abline(model, col = "red")
varImp(model)

#Testing the linear Demographic carachteristics vs NPS_Type
model <- lm(formula = NPS_Type ~ State_PL + CHILDREN_NUM_C + Gender_H + Age_Range_H + City_PL + Country_PL + Currency_PL, data = myData)
summary(model)

plot(myData$City_PL + myData$State_PL+ myData$Country_PL + myData$Currency_PL + myData$Gender_H + myData$CHILDREN_NUM_C + myData$Gender_H, myData$NPS_Type, xlab = "Demographic variables", ylab = "NPS Type", pch = 19, xlim = c(0,40), ylim = c(1,3))
abline(model, col = "red")
varImp(model)

#Testing the linear Demographic carachteristics vs Likelihood to recommend
model <- lm(formula = Likelihood_Recommend_H ~ State_PL + CHILDREN_NUM_C + Gender_H + Age_Range_H + City_PL + Country_PL + Currency_PL, data = myData)
summary(model)

plot(myData$City_PL + myData$State_PL + myData$Country_PL + myData$Currency_PL + myData$Gender_H + myData$CHILDREN_NUM_C + myData$Gender_H, myData$Likelihood_Recommend_H , xlab = "Demographic variables", ylab = "Likelihood to recommend", pch = 19, xlim = c(0,60), ylim = c(8,10))
abline(model, col = "red")
varImp(model)

#############################################################################################
#Using satisfaction of services for model
#Trying to create another using recomendations
model <- lm(formula = NPS_Type ~ Guest_Room_H+Tranquility_H+Condition_Hotel_H+Customer_SVC_H+Staff_Cared_H+Internet_Sat_H+ Check_In_H, data = myData)
summary(model)
plot(myData$Guest_Room_H+myData$Tranquility_H+myData$Condition_Hotel_H+myData$Customer_SVC_H+myData$Staff_Cared_H+myData$Internet_Sat_H+ myData$Check_In_H, myData$NPS_Type, xlab = "Satisfaction Scores", ylab = "NPS Type", pch = 19)
abline(model, col = "red")
varImp(model)

#Likelihood to recommend
model <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H+Tranquility_H+Condition_Hotel_H+Customer_SVC_H+Staff_Cared_H+Internet_Sat_H+ Check_In_H, data = myData)
summary(model)
plot(myData$Guest_Room_H+myData$Tranquility_H+myData$Condition_Hotel_H+myData$Customer_SVC_H+myData$Staff_Cared_H+myData$Internet_Sat_H+ myData$Check_In_H, myData$Likelihood_Recommend_H, xlab = "Satisfaction Scores", ylab = "Likelihood to recommend", pch = 19)
abline(model, col = "red")
varImp(model)

#Overall satisfaction
model <- lm(formula =  Overall_Sat_H  ~ Guest_Room_H+Tranquility_H+Condition_Hotel_H+Customer_SVC_H+Staff_Cared_H+Internet_Sat_H+ Check_In_H, data = myData)
summary(model)
plot(myData$Guest_Room_H+myData$Tranquility_H+myData$Condition_Hotel_H+myData$Customer_SVC_H+myData$Staff_Cared_H+myData$Internet_Sat_H+ myData$Check_In_H, myData$Overall_Sat_H, xlab = "Satisfaction Scores", ylab = "Overall satisfaction", pch = 19, xlim = c(5,20))
abline(model, col = "red")
varImp(model)


####################################################################################
#Using ammenities for lineal models
#Transform all the ammenities values (Y,N) to numeric
unique(myData$Mini.Bar_PL)
myData$Mini.Bar_PL <- sapply(myData$Mini.Bar_PL, as.numeric)

unique(myData$Pool.Indoor_PL)
myData$Pool.Indoor_PL <- sapply(myData$Pool.Indoor_PL, as.numeric)

unique(myData$Pool.Outdoor_PL)
myData$Pool.Outdoor_PL <- sapply(myData$Pool.Outdoor_PL, as.numeric)

unique(myData$Regency.Grand.Club_PL)
myData$Regency.Grand.Club_PL <- sapply(myData$Regency.Grand.Club_PL, as.numeric)

unique(myData$Resort_PL)
myData$Resort_PL <- sapply(myData$Resort_PL, as.numeric)

unique(myData$Restaurant_PL)
myData$Restaurant_PL <- sapply(myData$Restaurant_PL, as.numeric)

unique(myData$Self.Parking_PL)
myData$Self.Parking_PL <- sapply(myData$Self.Parking_PL, as.numeric)

unique(myData$Shuttle.Service_PL)
myData$Shuttle.Service_PL <- sapply(myData$Shuttle.Service_PL, as.numeric)

unique(myData$Ski_PL)
myData$Ski_PL <- sapply(myData$Ski_PL, as.numeric)

unique(myData$Spa_PL)
myData$Spa_PL <- sapply(myData$Spa_PL, as.numeric)

unique(myData$Spa.services.in.fitness.center_PL)
myData$Spa.services.in.fitness.center_PL <- sapply(myData$Spa.services.in.fitness.center_PL, as.numeric)

unique(myData$Spa.online.booking_PL)
myData$Spa.online.booking_PL <- sapply(myData$Spa.online.booking_PL, as.numeric)

#Likelihood to recommend as dependent variable
model <- lm(formula = Likelihood_Recommend_H ~ Mini.Bar_PL + Pool.Indoor_PL + Pool.Outdoor_PL + Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL + Ski_PL + Spa_PL + Spa.services.in.fitness.center_PL + Spa.online.booking_PL, data = myData)
summary(model)
plot(myData$Mini.Bar_PL + myData$Pool.Indoor_PL + myData$Pool.Outdoor_PL + myData$Regency.Grand.Club_PL + myData$Resort_PL + myData$Restaurant_PL + myData$Self.Parking_PL + myData$Shuttle.Service_PL + myData$Ski_PL + myData$Spa_PL + myData$Spa.services.in.fitness.center_PL + myData$Spa.online.booking_PL,myData$Likelihood_Recommend_H, xlab = "Ammenities count", ylab = "Likelihood to recommend", pch = 19)
abline(model, col = "red")
varImp(model)

#Overallsatisfaction as dependent variable
model <- lm(formula = Overall_Sat_H ~ Mini.Bar_PL + Pool.Indoor_PL + Pool.Outdoor_PL + Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL + Ski_PL + Spa_PL + Spa.services.in.fitness.center_PL + Spa.online.booking_PL, data = myData)
summary(model)
plot(myData$Mini.Bar_PL + myData$Pool.Indoor_PL + myData$Pool.Outdoor_PL + myData$Regency.Grand.Club_PL + myData$Resort_PL + myData$Restaurant_PL + myData$Self.Parking_PL + myData$Shuttle.Service_PL + myData$Ski_PL + myData$Spa_PL + myData$Spa.services.in.fitness.center_PL + myData$Spa.online.booking_PL,myData$Overall_Sat_H , xlab = "Ammenities", ylab = "Overall satisfaction", pch = 19, ylim = c(9,13), xlim = c(0,10))
abline(model, col = "red")
varImp(model)

#NPS_Type as dependent variable
model <- lm(formula = NPS_Type ~ Mini.Bar_PL + Pool.Indoor_PL + Pool.Outdoor_PL + Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL + Ski_PL + Spa_PL + Spa.services.in.fitness.center_PL + Spa.online.booking_PL, data = myData)
summary(model)
plot(myData$Mini.Bar_PL + myData$Pool.Indoor_PL + myData$Pool.Outdoor_PL + myData$Regency.Grand.Club_PL + myData$Resort_PL + myData$Restaurant_PL + myData$Self.Parking_PL + myData$Shuttle.Service_PL + myData$Ski_PL + myData$Spa_PL + myData$Spa.services.in.fitness.center_PL + myData$Spa.online.booking_PL,myData$NPS_Type , xlab = "Amenities", ylab = "NPS_TYPE", pch = 19, xlim = c(0,100), ylim = c(1, 3))
abline(model, col = "red")
varImp(model)

##########################################################################3
#Linear Models for Different states
myData <- read.csv(tmp, header = TRUE, stringsAsFactors = TRUE)
sort(table(myData$State_PL))
#Converting to numeric NPS_TYPE
unique(myData$NPS_Type)
table(myData$NPS_Type)
myData$NPS_Type <- sapply(myData$NPS_Type, as.numeric)

#Selecting the state
myData <- myData[myData$State_PL == "Florida",]

#Using satisfaction of services for model
#Trying to create another using recomendations
model <- lm(formula = NPS_Type ~ Guest_Room_H+Tranquility_H+Condition_Hotel_H+Customer_SVC_H+Staff_Cared_H+Internet_Sat_H+ Check_In_H, data = myData)
summary(model)
plot(myData$Guest_Room_H+myData$Tranquility_H+myData$Condition_Hotel_H+myData$Customer_SVC_H+myData$Staff_Cared_H+myData$Internet_Sat_H+ myData$Check_In_H, myData$NPS_Type, xlab = "Satisfaction Scores", ylab = "NPS Type", pch = 19)
abline(model, col = "red")
varImp(model)

#Likelihood to recommend
model <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H+Tranquility_H+Condition_Hotel_H+Customer_SVC_H+Staff_Cared_H+Internet_Sat_H+ Check_In_H, data = myData)
summary(model)
plot(myData$Guest_Room_H+myData$Tranquility_H+myData$Condition_Hotel_H+myData$Customer_SVC_H+myData$Staff_Cared_H+myData$Internet_Sat_H+ myData$Check_In_H, myData$Likelihood_Recommend_H, xlab = "Satisfaction Scores", ylab = "Likelihood to recommend", pch = 19)
abline(model, col = "red")
varImp(model)

#Overall satisfaction
model <- lm(formula =  Overall_Sat_H  ~ Guest_Room_H+Tranquility_H+Condition_Hotel_H+Customer_SVC_H+Staff_Cared_H+Internet_Sat_H+ Check_In_H, data = myData)
summary(model)
plot(myData$Guest_Room_H+myData$Tranquility_H+myData$Condition_Hotel_H+myData$Customer_SVC_H+myData$Staff_Cared_H+myData$Internet_Sat_H+ myData$Check_In_H, myData$Overall_Sat_H, xlab = "Satisfaction Scores", ylab = "Overall satisfaction", pch = 19, xlim = c(5,20))
abline(model, col = "red")
varImp(model)

####################################################################################
#Using ammenities for lineal models
#Transform all the ammenities values (Y,N) to numeric
myData$Mini.Bar_PL <- sapply(myData$Mini.Bar_PL, as.numeric)
myData$Pool.Indoor_PL <- sapply(myData$Pool.Indoor_PL, as.numeric)
myData$Pool.Outdoor_PL <- sapply(myData$Pool.Outdoor_PL, as.numeric)
myData$Regency.Grand.Club_PL <- sapply(myData$Regency.Grand.Club_PL, as.numeric)
myData$Resort_PL <- sapply(myData$Resort_PL, as.numeric)
myData$Restaurant_PL <- sapply(myData$Restaurant_PL, as.numeric)
myData$Self.Parking_PL <- sapply(myData$Self.Parking_PL, as.numeric)
myData$Shuttle.Service_PL <- sapply(myData$Shuttle.Service_PL, as.numeric)
myData$Ski_PL <- sapply(myData$Ski_PL, as.numeric)
myData$Spa_PL <- sapply(myData$Spa_PL, as.numeric)
myData$Spa.services.in.fitness.center_PL <- sapply(myData$Spa.services.in.fitness.center_PL, as.numeric)
myData$Spa.online.booking_PL <- sapply(myData$Spa.online.booking_PL, as.numeric)

#Likelihood to recommend as dependent variable
model <- lm(formula = Likelihood_Recommend_H ~ Mini.Bar_PL + Pool.Indoor_PL + Pool.Outdoor_PL + Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL + Ski_PL + Spa_PL + Spa.services.in.fitness.center_PL + Spa.online.booking_PL, data = myData)
summary(model)
plot(myData$Mini.Bar_PL + myData$Pool.Indoor_PL + myData$Pool.Outdoor_PL + myData$Regency.Grand.Club_PL + myData$Resort_PL + myData$Restaurant_PL + myData$Self.Parking_PL + myData$Shuttle.Service_PL + myData$Ski_PL + myData$Spa_PL + myData$Spa.services.in.fitness.center_PL + myData$Spa.online.booking_PL,myData$Likelihood_Recommend_H, xlab = "Ammenities count", ylab = "Likelihood to recommend", pch = 19)
abline(model, col = "red")
varImp(model)

#Overallsatisfaction as dependent variable
model <- lm(formula = Overall_Sat_H ~ Mini.Bar_PL + Pool.Indoor_PL + Pool.Outdoor_PL + Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL + Ski_PL + Spa_PL + Spa.services.in.fitness.center_PL + Spa.online.booking_PL, data = myData)
summary(model)
plot(myData$Mini.Bar_PL + myData$Pool.Indoor_PL + myData$Pool.Outdoor_PL + myData$Regency.Grand.Club_PL + myData$Resort_PL + myData$Restaurant_PL + myData$Self.Parking_PL + myData$Shuttle.Service_PL + myData$Ski_PL + myData$Spa_PL + myData$Spa.services.in.fitness.center_PL + myData$Spa.online.booking_PL,myData$Overall_Sat_H , xlab = "Ammenities", ylab = "Overall satisfaction", pch = 19)
abline(model, col = "red")
varImp(model)

#NPS_Type as dependent variable
model <- lm(formula = NPS_Type ~ Mini.Bar_PL + Pool.Indoor_PL + Pool.Outdoor_PL + Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL + Ski_PL + Spa_PL + Spa.services.in.fitness.center_PL + Spa.online.booking_PL, data = myData)
summary(model)
plot(myData$Mini.Bar_PL + myData$Pool.Indoor_PL + myData$Pool.Outdoor_PL + myData$Regency.Grand.Club_PL + myData$Resort_PL + myData$Restaurant_PL + myData$Self.Parking_PL + myData$Shuttle.Service_PL + myData$Ski_PL + myData$Spa_PL + myData$Spa.services.in.fitness.center_PL + myData$Spa.online.booking_PL,myData$NPS_Type , xlab = "Amenities", ylab = "NPS_TYPE", pch = 19, xlim = c(0,100), ylim = c(1, 3))
abline(model, col = "red")
varImp(model)
