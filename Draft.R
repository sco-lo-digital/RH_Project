#install.packages("pacman")
library(pacman)
p_load(leaflet, ggplot2, Hmisc, arules, zipcode, dplyr, caret)
data("zipcode")
#Read in data
<<<<<<< HEAD
df <- read.csv("~/Data_Science/RH_Project/query_for_cust_tn.csv", stringsAsFactors=FALSE)
set.seed(3456)
trainIndex <- createDataPartition(df$TN_Flag, p = .8,
                                  list = FALSE,
                                  times = 1)
dfTrain <- df[trainIndex,] #Use this for exploration and modeling
dfTest <- df[-trainIndex,] #Use this for validation
#write.csv(dfTrain, file = "RH_Train_TN_Data")

prop.table(table(dfTest$TN_Flag)) # check for proportionality
prop.table(table(dfTrain$TN_Flag))

#Create teen only data set for exploration
teens <- dfTrain[dfTrain$TN_Flag==1,]
=======
df <- read.csv("query_for_cust_tn.csv", stringsAsFactors=FALSE)
#Create teen only data set
teens <- df[df$TN_Flag==1,]
>>>>>>> fffc45f77d8f5fb4e77d0cf296db205e6ce3dea5
#Clean zipcode
teens$Zip <- as.character(teens$Zip)
#Join lookup table of zipcodes to our data set
mapdf <- inner_join(zipcode, teens, by = c("zip"= "Zip"))

#Hypothesis: Teen Buyers are on the coasts

    #Create cluster map
p <- leaflet(data = mapdf)
p %>%  addTiles() %>% addMarkers(~mapdf$longitude, ~mapdf$latitude, clusterOptions = markerClusterOptions()
)


#Hypothesis: Designers are the ones buying Teen category
describe(teens$trade_buyer)

#Hypothesis: Teen buyers have children at least 7 years old
describe(teens$POC79Flag)
describe(teens$POC1012Flag) 

#Hypothesis: Teen buyers have fancy houses
describe(teens$Est_Home_Value)

describe(df$Est_Home_Value)
    #Adjust for extreme data points
teensGreaterZero <- teens[teens$val_TN>1000 & teens$Est_Home_Value>100000 & teens$Est_Home_Value<765000,]
    #Explore linear relationship between home value and value of teen purchase
linear.homes <- lm(val_TN~Est_Home_Value, data = teensGreaterZero)
summary(linear.homes)
    #Confirm meaningless result with plot
plot(teensGreaterZero$Est_Home_Value, teensGreaterZero$val_TN)

#Hypothesis: Private Label Credit Card users spend more
teens %>% group_by(as.factor(PLCC_holder)) %>%  summarise(mean(val_TN)) 
teens %>% group_by(as.factor(PLCC_holder)) %>%  summarise(mean(val_BC)) 
teens %>% group_by(as.factor(PLCC_holder)) %>%  summarise(mean(val_RH)) 

#Hypothesis: People with many items in BC also buy teen
#AssociateRules

#Hypothesis: People that buy a lot of BC items, also buy TN
linear.bc <- lm(val_TN~val_BC, data = teens)
summary(linear.bc)
plot(teens$val_TN,teens$val_BC)

linear.rh <- lm(val_TN~val_RH, data = teens)
summary(linear.rh)
plot(teens$val_TN,teens$val_RH)

cor(teens$val_TN,teens$val_RH)
cor(teens$val_TN,teens$val_BC)
cor(teens$item_TN,teens$item_BC)

linear.item.rh <- lm(item_TN~item_BC, data = teens)
#Hypothesis: Canadians love RH teen?
mean(teens$Foreign_Flag) #Barely Any Canadians

#Hypothesis: People that buy holiday items are more likely to buy Teen
cor(teens$valHol, teens$val_TN) #look at correlation of Holiday value with TN value

#Hypothesis: Customers that buy online are more likely to buy Teen?

table(teens$channelUsage) #How many in each channel
teens %>% group_by(as.factor(channelUsage)) %>%  summarise(sum(val_TN)) #Total value of TN product by channel
teens %>% group_by(as.factor(channelUsage)) %>%  summarise(mean(val_TN)) #Avg value of TN product by channel

#Hypothesis: US Buyers spend more on teen than Canadian buyers
teens %>% group_by(as.factor(Foreign_Flag)) %>%  summarise(mean(val_TN)) 
