##Step 1 Load the Data

library(reshape2) 
library(ggmap)
library(ggplot2)

income <- read.csv("file:///C:/Users/kdoyl/OneDrive/Documents/IST687/MedianZIP.csv", stringsAsFactors = FALSE)

numberize <- function(inputVector){
  
  inputVector <- gsub(",", "", inputVector) # remove the commas
  inputVector <- gsub(" ", "", inputVector) # remove the spaces
  
  return(as.numeric(inputVector)) # convert to number and return
}

income$Median <- numberize(income$Median)
income$Mean <- numberize(income$Mean)
income$Pop <- numberize(income$Pop)

income$Mean <- ifelse(is.na(income$Mean), income$Median, income$Mean)

colnames(income) <- tolower(colnames(income))

install.packages("zipcode")
library(zipcode)
data("zipcode")

income$zip <- clean.zipcodes(income$zip) 
income = merge(income, zipcode, by.x = 'zip', by.y = 'zip')
income$state.name <- tolower(state.name[match(income$state, state.abb)]) 

income = income[!(income$state %in% c("HI", "AK")),]

##Step 2 Show income and population per state

state.income <- income[, c("state.name", "pop", "mean")] 

state.income$TotalIncome <- state.income$mean*state.income$pop

Population <- tapply(state.income$pop, state.income$state.name, sum)

Income <- tapply(state.income$TotalIncome, state.income$state.name, sum)

Simple <- data.frame(state.name=names(Income),Population, Income)
Simple$AvgIncome <- Income/Population
colnames(Simple)

Simple$state.abb <- tolower(state.abb[match(Simple$state.name, tolower(state.name))])

Simple <- Simple[, c(1, 4, 2, 3)]

us<- map_data("state")

dummyDF <- data.frame(state.name, stringsAsFactors = FALSE)
dummyDF$state<-tolower(dummyDF$state.name)
map.simple <- ggplot(dummyDF, aes(map_id = state))+geom_map(map=us, fill="white", color="black") + expand_limits(x=us$long, y=us$lat)+coord_map()



map.averageMedianIncome <- ggplot(Simple, aes(map_id = state.name))+geom_map(map=us, aes(fill=AvgIncome))+expand_limits(x=us$long, y=us$lat)+coord_map() + ggtitle("Average Median Income")
map.population <- ggplot(Simple, aes(map_id = state.name))+geom_map(map=us, aes(fill=Population))+expand_limits(x=us$long, y=us$lat)+coord_map() + ggtitle("Population")

#Step 3 Show the income per zipcode
head(income)
map.simple.black <- ggplot(dummyDF, aes(map_id = state))+geom_map(map=us, fill="black", color="white") + expand_limits(x=us$long, y=us$lat)+coord_map()
median.income.zip <- map.simple.black+geom_point(data=income, aes(x=income$longitude, y=income$latitude, color=income$median))

#Step 4 Show Zip Code Density
library(viridis)
ZipcodeDensity <- ggplot(income, aes(map_id = state.name)) +
  geom_map(map = us, fill='blue', alpha=.5) +
  expand_limits(x = us$long, y = us$lat) +
  coord_map() + 
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level..), alpha = .25,
                 geom = "polygon", data = income) + 
  scale_fill_viridis() + 
  ggtitle("Zipcode Density")

#Step 5 Zoom in to the region around NYC
library(ggmap)
zoomGeo <- geocode("New York, ny", source = "dsk")
zoomAmount <- 3
centerx <- zoomGeo$lon
centery <- zoomGeo$lat
ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount, centerx+zoomAmount)
map.simple.black.ny <- ggplot(dummyDF, aes(map_id = state))+geom_map(map=us, fill="black", color="white") + expand_limits(x=xlimit, y=ylimit)+coord_map()

zoomZipcodes <- income
zoomZipcodes <- zoomZipcodes[zoomZipcodes$longitude>xlimit[1],]
zoomZipcodes <- zoomZipcodes[zoomZipcodes$longitude<xlimit[2],]
zoomZipcodes <- zoomZipcodes[zoomZipcodes$latitude >ylimit[1],]
zoomZipcodes <- zoomZipcodes[zoomZipcodes$latitude <ylimit[2],]
median.income.zip.ny <- map.simple.black.ny+geom_point(data=zoomZipcodes, aes(x=zoomZipcodes$longitude, y=zoomZipcodes$latitude, color=zoomZipcodes$median))

ZipcodeDensityNY <- ggplot(zoomZipcodes, aes(map_id = state.name)) +
  geom_map(map = us, fill='blue', alpha=.5) +
  expand_limits(x = xlimit, y = ylimit) +
  coord_map() + 
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level..), alpha = .25,
                 geom = "polygon", data = zoomZipcodes) + 
  scale_fill_viridis() + 
  ggtitle("Zipcode Density")
