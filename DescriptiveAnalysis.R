library("ggplot2")
library(ggmap)
library(maps)
library(mapdata)

#Descriptive Analysis
tmp <- file.choose()
myData <- read.csv(tmp, header = TRUE, stringsAsFactors = TRUE)

table(myData$Gender_H, myData$NPS_Type)

#Gender pie chart
gender <- as.data.frame(table(myData$Gender_H))
colnames(gender) <- c("Gender", "Freq")
gender <- gender[gender$Gender != "",]
#gender <- gender[gender$Var1 != "Prefer not to answer",]
bp<- ggplot(gender, aes(x="", y=Freq, fill=Gender))+
  geom_bar(width = 1, stat = "identity") 
bp
pie <- bp + coord_polar("y", start=0) + 
  ggtitle("Gender Distribution") +# for the main title
  scale_fill_manual(values=c("hotpink1", "skyblue", "olivedrab1"))
pie
#Gender map
Gender_maps <- myData[,c("Property.Latitude_PL","Property.Longitude_PL","Gender_H")]

#separating points by NPS_TYPE
unique(Gender_maps$Gender_H)
#Female
Female <- Gender_maps[Gender_maps$Gender_H == "Female",]
#Male
Male <- Gender_maps[Gender_maps$Gender_H == "Male",]

world <- map_data("world")
hyattMap <- 0
hyattMap <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey40", colour="grey90", alpha=1) + 
  coord_fixed(1.3)

FemaleMap <-hyattMap + geom_point(aes(x = Female$Property.Longitude_PL, y = Female$Property.Latitude_PL), color = "hotpink1", size = 1) + ggtitle("Females respondents")
MaleMap <-hyattMap + geom_point(aes(x = Male$Property.Longitude_PL, y = Male$Property.Latitude_PL), color = "skyblue", size = 1)  + ggtitle("Male respondents")

#NPS VS Gender
myData$count <- 1
agg.data <- aggregate(myData$count, by = list(gender = myData$Gender_H, NPS_type = myData$NPS_Type), FUN =  sum)

p1 <- ggplot(agg.data, aes(x=gender, y=x, fill=NPS_type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=x), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5) + 
  theme_minimal() +
  ggtitle("Number of NPS_Type by gender") + # for the main title
  xlab("Gender") + # for the x axis label
  ylab("Frequency") # for the y axis label

#Another plot
p2 <- ggplot(agg.data, aes(x=gender, y=x, fill=NPS_type)) +
  geom_bar(stat="identity")+theme_minimal()

#Number of children
myData <- transform(myData, similarityTag= ifelse(Similarity == 1, "Similar", ifelse(Similarity > 0.5, "some similar", "no similar") ))
unique(myData$CHILDREN_NUM_C)
myData <- myData[myData$CHILDREN_NUM_C != 5,]
myData <- myData[myData$CHILDREN_NUM_C != 6,]

agg.data <- aggregate(myData$count, by = list(children = myData$CHILDREN_NUM_C, NPS_type = myData$NPS_Type),FUN =  sum)

p1 <- ggplot(agg.data, aes(x=children, y=x, fill=NPS_type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=x), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5) + 
  theme_minimal() +
  ggtitle("Number of NPS_Type by Number of Children") + # for the main title
  xlab("Number of Children") + # for the x axis label
  ylab("Frequency") # for the y axis label



#Age
age <- as.data.frame(table(myData$Age_Range_H))
colnames(age) <- c("Age_range", "Freq")
age <- age[age$Age_range != "",]

bp<- ggplot(age, aes(x="", y=Freq, fill=Age_range))+
  geom_bar(width = 1, stat = "identity") 
bp
pie <- bp + coord_polar("y", start=0) + 
  ggtitle("Age Distribution") # for the main title
pie

agg.data <- aggregate(myData$count, by = list(age = myData$Age_Range_H, NPS_type = myData$NPS_Type),
                      FUN =  sum)

agg.data <- agg.data[agg.data$age != "",]
p1 <- ggplot(agg.data, aes(x=age, y=x, fill=NPS_type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=x), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5) + 
  theme_minimal() +
  ggtitle("Number of NPS_Type by Age Range") + # for the main title
  xlab("Age") + # for the x axis label
  ylab("Frequency") # for the y axis label


#Country
#myData <- myData[myData$Country_PL == "United States",]
myData <- myData[myData$State_PL == "Texas",]
USA <- as.data.frame(table(myData$NPS_Type))
colnames(USA) <- c("NPS_Type", "Freq")


bp<- ggplot(USA, aes(x="", y=Freq, fill=NPS_Type))+
  geom_bar(width = 1, stat = "identity") 
bp
pie <- bp + coord_polar("y", start=0) + 
  ggtitle("NPS_Type distribution in Texas") # for the main title
pie


#Ammenities
myData$Likelihood_Recommend_H
barplot(table(myData$Likelihood_Recommend_H))
ggplot(myData, aes(x=Spa.online.booking_PL, y=Likelihood_Recommend_H)) + geom_point()
# Change the point size, and shape
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)

library(ggplot2)
library(maps)

tmp <- file.choose()
tmp
myData <- read.csv(tmp, header = TRUE, stringsAsFactors = TRUE)

myData <- myData[myData$Country_PL == "United States", ]
myData <- myData[myData$Resort_PL == "N",]

#load us map data
all_states <- map_data("state")
ammenitiesMap <- myData[,c("Property.Latitude_PL","Property.Longitude_PL")]

hyattMap <- ggplot() + geom_polygon(data = all_states, aes(x=long, y = lat, group = group), fill="grey40", colour="grey90", alpha=1) + 
  coord_fixed(1.3)

MissingAmmenities <-hyattMap + geom_point(aes(x = ammenitiesMap$Property.Longitude_PL, y = ammenitiesMap$Property.Latitude_PL), color = "blue", size = 1) + ggtitle("Hotels with Restaurants, Spa, and  Resort")

#By state
#California
states <- subset(all_states, region %in% c( "california") )
myData <- myData[myData$State_PL == "California",]
ammenitiesMap <- myData[,c("Property.Latitude_PL","Property.Longitude_PL")]
hyattMap <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill="grey40", colour="grey90", alpha=1) + 
  coord_fixed(1.3)

California <-hyattMap + geom_point(aes(x = ammenitiesMap$Property.Longitude_PL, y = ammenitiesMap$Property.Latitude_PL), color = "red", size = 1) + ggtitle("Hotels with no Resorts in California")

#Florida
states <- subset(all_states, region %in% c( "florida") )
myData <- myData[myData$State_PL == "Florida",]
ammenitiesMap <- myData[,c("Property.Latitude_PL","Property.Longitude_PL")]
hyattMap <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill="grey40", colour="grey90", alpha=1) + 
  coord_fixed(1.3)

RestaurantMap <-hyattMap + geom_point(aes(x = ammenitiesMap$Property.Longitude_PL, y = ammenitiesMap$Property.Latitude_PL), color = "red", size = 1) + ggtitle("Hotels with no Resorts in Florida")


#Texas
states <- subset(all_states, region %in% c( "texas") )
myData <- myData[myData$State_PL == "Texas",]
ammenitiesMap <- myData[,c("Property.Latitude_PL","Property.Longitude_PL")]
hyattMap <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill="grey40", colour="grey90", alpha=1) + 
  coord_fixed(1.3)

RestaurantMap <-hyattMap + geom_point(aes(x = ammenitiesMap$Property.Longitude_PL, y = ammenitiesMap$Property.Latitude_PL), color = "red", size = 1) + ggtitle("Hotels with no Resorts in Texas")


#Hotels with low Guest room satisfaction score
myData <- read.csv(tmp, header = TRUE, stringsAsFactors = TRUE)
unique(myData$Guest_Room_H)
myData <- myData[myData$Country_PL == "United States", ]
myData <- myData[myData$Guest_Room_H < 6 ,]

#Texas
states <- subset(all_states, region %in% c( "texas") )
myData <- myData[myData$State_PL == "Texas",]
ammenitiesMap <- myData[,c("Property.Latitude_PL","Property.Longitude_PL")]
hyattMap <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill="grey40", colour="grey90", alpha=1) + 
  coord_fixed(1.3)

RestaurantMap <-hyattMap + geom_point(aes(x = ammenitiesMap$Property.Longitude_PL, y = ammenitiesMap$Property.Latitude_PL), color = "red", size = 1) + ggtitle("Hotels with no Resorts in Texas")

#California
states <- subset(all_states, region %in% c( "california") )
myData <- myData[myData$State_PL == "California",]
ammenitiesMap <- myData[,c("Property.Latitude_PL","Property.Longitude_PL")]
hyattMap <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill="grey40", colour="grey90", alpha=1) + 
  coord_fixed(1.3)

California <-hyattMap + geom_point(aes(x = ammenitiesMap$Property.Longitude_PL, y = ammenitiesMap$Property.Latitude_PL), color = "red", size = 1) + ggtitle("Hotels with no Resorts in California")

#Florida
states <- subset(all_states, region %in% c( "florida") )
myData <- myData[myData$State_PL == "Florida",]
ammenitiesMap <- myData[,c("Property.Latitude_PL","Property.Longitude_PL")]
hyattMap <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill="grey40", colour="grey90", alpha=1) + 
  coord_fixed(1.3)

RestaurantMap <-hyattMap + geom_point(aes(x = ammenitiesMap$Property.Longitude_PL, y = ammenitiesMap$Property.Latitude_PL), color = "red", size = 1) + ggtitle("Hotels with no Resorts in Florida")

