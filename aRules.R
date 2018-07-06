#Creating arules
#loading library
library("arules")
library("arulesViz")
tmp <- file.choose()
tmp
myData <- read.csv(tmp, header = TRUE, stringsAsFactors = TRUE)


####################################################################################
#Creating subset for ammenities
myDataAmmenities <- myData[, c("Resort_PL", "Regency.Grand.Club_PL", "Pool.Outdoor_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL", "Spa.online.booking_PL" , "Spa.services.in.fitness.center_PL", "Pool.Indoor_PL", "Mini.Bar_PL", "Restaurant_PL" ,"NPS_Type")]
myDataAmmenities[myDataAmmenities==""] <- NA


#Promoters
ruleset <- apriori( myDataAmmenities, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Promoter"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.2]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(goodrules)

#Detractors
ruleset <- apriori( myDataAmmenities, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Detractor"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 2.5]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(goodrules)

#Passive
ruleset <- apriori( myDataAmmenities, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Passive"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.64]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(goodrules)

###############################################################################
#Using demographic information
myDataDemographics <- myData[, c("Gender_H","Age_Range_H" ,"Country_PL","NPS_Type")]
myDataDemographics[myDataDemographics==""] <- NA
myDataDemographics <- myDataDemographics[complete.cases(myDataDemographics),]

#Promoters
ruleset <- apriori( myDataDemographics, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Promoter"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.2]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(goodrules)

#Detractors
ruleset <- apriori( myDataDemographics, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Detractor"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.5]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(goodrules)

#Passive
ruleset <- apriori( myDataDemographics, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Passive"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.6]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(goodrules)

#

#Subsetting by state
myData <- myData[myData$State_PL == "California",]
####################################################################################
#Creating subset for ammenities
myDataAmmenities <- myData[, c("Resort_PL", "Regency.Grand.Club_PL", "Pool.Outdoor_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL", "Spa.online.booking_PL" , "Spa.services.in.fitness.center_PL", "Pool.Indoor_PL", "Mini.Bar_PL", "Restaurant_PL" ,"NPS_Type")]
myDataAmmenities[myDataAmmenities==""] <- NA


#Promoters
ruleset <- apriori( myDataAmmenities, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Promoter"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.4]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(head(goodrules))

#Detractors
ruleset <- apriori( myDataAmmenities, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Detractor"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.0]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(head(goodrules))

#Passive
ruleset <- apriori( myDataAmmenities, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Passive"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.64]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(goodrules)

###############################################################################
#Using demographic information
myDataDemographics <- myData[, c("Gender_H","Age_Range_H" ,"NPS_Type")]
myDataDemographics[myDataDemographics==""] <- NA
myDataDemographics <- myDataDemographics[complete.cases(myDataDemographics),]

#Promoters
ruleset <- apriori( myDataDemographics, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Promoter"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.2]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(goodrules)

#Detractors
ruleset <- apriori( myDataDemographics, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Detractor"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.5]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(goodrules)

#Passive
ruleset <- apriori( myDataDemographics, parameter = list(supp=0.001,conf = 0.05), appearance = list(default="lhs",rhs="NPS_Type=Passive"), control = list (verbose=F))
summary(ruleset)
inspect(head(ruleset))
itemFrequencyPlot(items(ruleset))
plot(ruleset)
#Best Rules
goodrules <- ruleset[quality(ruleset)$lift > 1.6]
goodrules <- sort(goodrules, by="lift", decreasing = TRUE)
summary(goodrules)
inspect(goodrules)

