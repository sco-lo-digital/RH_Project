#Tidy Data
#install.packages("pacman")
library(pacman)
p_load(leaflet, ggplot2, zipcode, plyr, dplyr, caret, geosphere, doMC)
#load zipcode data for joining lat long
data("zipcode")
#Read in data for the 70 stores
stores <- read.csv("~/Data_Science/RH_Project/GalleryLocation.csv", stringsAsFactors=FALSE)
#join the stores dataset to the zipcode dataset to add features of lat and long
storesJoined <- inner_join(zipcode, stores, by = c("zip"= "Zip"))
#Create new feature called Region - first 3 digits of zipcode
storesJoined$Region <- substr(storesJoined$zip,1, 3)
#Add Median Home sold Price from Zillow data
ZillowMedianSld <- read.csv("~/Data_Science/RH_Project/Zip_MedianSoldPrice_AllHomes.csv", stringsAsFactors=FALSE)
ZillowMedianSld <- ZillowMedianSld[,c("RegionName", "X2015.12")]
names(ZillowMedianSld) <- c("RegionName", "Zill_Med_Sld")
ZillowMedianSld$RegionName <- as.character(ZillowMedianSld$RegionName)
#Add Median Home sold Price per sqft from Zillow data
ZillowMedSqFt <- read.csv("~/Data_Science/RH_Project/Zip_MedianSoldPricePerSqft_AllHomes.csv", stringsAsFactors=FALSE)
ZillowMedSqFt <- ZillowMedSqFt[,c("RegionName", "X2015.12")]
names(ZillowMedSqFt) <- c("RegionName", "Zill_Med_SqFt")
ZillowMedSqFt$RegionName <- as.character(ZillowMedSqFt$RegionName)
#Read in catalog data
#BC catalog
BC15_Mailed <- read.table("~/Data_Science/RH_Project/BC15_Mailed.csv", quote="\"", comment.char="", stringsAsFactors=FALSE)
#TN catalog
TN15_Mailed <- read.table("~/Data_Science/RH_Project/TN15_Mailed.csv", quote="\"", comment.char="", stringsAsFactors=FALSE)
#Read in RH data
#temp<- read.csv("~/Data_Science/RH_Project/query_for_cust_tn.csv", stringsAsFactors=FALSE, strip.white=TRUE)

RH_DF <- read.csv("~/Data_Science/RH_Project/query_for_cust_tn.csv", stringsAsFactors=FALSE, strip.white=TRUE)
RH_DF$Zip <- as.character(RH_DF$Zip)
#check for unique HHIDs
#is_unique <- unique(dfTrain2$HHID)
#str(is_unique)

#Identify columns (covariates) to keep
keep <- c("HHID","Zip","Foreign_Flag","Mosaic_New",   
          "POC03Flag","POC46Flag","POC79Flag","POC1012Flag",
          "POC1315Flag","POC1618Flag","channelUsage","B_TextileVal",
          "B_TextileNum","B_FurntVal","B_FurntNum","B_LightVal",
          "B_LightNum", "B_WinVal","B_WinNum","B_AprlVal",
          "B_AprlNum","FurntVal", "FurntNum","OutFurntVal",
          "OutFurntNum","TextileVal","TextileNum","BathVal",
          "BathNum","LightVal","LightNum","AccessVal","AccessNum",       
          "item_RH","item_BC","val_BC", "val_RH","RH_flag","BC_Flag",
          "Est_Home_Value","HH_BuyerType","HH_BuyerType_BC","trade_buyer",
          "PLCC_holder","TradeArea","TA","TN_Flag")

RHfactors <- c("Foreign_Flag","Mosaic_New",   
               "POC03Flag","POC46Flag","POC79Flag","POC1012Flag",
               "POC1315Flag","POC1618Flag","channelUsage","RH_flag",
               "BC_Flag","HH_BuyerType","HH_BuyerType_BC","trade_buyer",
               "PLCC_holder","TradeArea","TA")

RH_DF <- RH_DF[,keep]

RH_DF <- inner_join(RH_DF, zipcode, by = c( "Zip"="zip"))
#Convert 0 vals in Est Hm Val to NAs for replacement/imputation
RH_DF$Est_Home_Value[RH_DF$Est_Home_Value == 0] <- NA
#factor
RH_DF[RHfactors] <- lapply(RH_DF[RHfactors], as.factor)
RH_DF$Foreign_Flag <- factor(RH_DF$Foreign_Flag, levels = c(0,1), labels = c("NtCNDN", "CNDN"))
RH_DF$TN_Flag <- factor(RH_DF$TN_Flag, levels = c(0,1), labels = c("NotTN", "TN"))
#Create Region feature
RH_DF$Region <- substr(RH_DF$Zip,1,3) %>% as.factor()

###### 
#Impute Est Home Value
# Create an imputation function for Est Hm values that are NA
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
#Impute at zip code 
RH_DF <- RH_DF %>% group_by(Zip) %>% mutate(ImpHmVal = impute.mean(Est_Home_Value))
RH_DF<- RH_DF %>% as.data.frame()
# Impute at region level 
RH_DF <- RH_DF %>% group_by(Region) %>% mutate(ImpHmVal1 = impute.mean(ImpHmVal))
RH_DF<- RH_DF %>% as.data.frame()
#Impute at State level for those remaining
RH_DF<- RH_DF %>% group_by(state) %>% mutate(ImpHmVal2 = impute.mean(ImpHmVal1))
RH_DF<- RH_DF %>% as.data.frame()



#drops <- c(c("ImpHmVal","ImpHmVal1", "Est_Home_Value"))
#RH_DF <- RH_DF[ , !(names(RH_DF) %in% drops)]



#Remove anything else
#rm <- is.na(RH_DF$ImpHmVal2)%>% which()
#RH_DF <- RH_DF[-rm,]
#Create boolean feature for a HHID's first 3 digits of zip code matching first 3 digits of store zip code
RH_DF$Store <- RH_DF$Region %in% storesJoined$Region %>% as.factor()

#Add median home vals to RH_DF
RH_DF <-  left_join(RH_DF, ZillowMedianSld, by = c("Zip"= "RegionName"))
RH_DF <-  left_join(RH_DF, ZillowMedSqFt, by = c("Zip"= "RegionName"))
######### 
#Impute Zillow Median Sold Price
#Impute at zip code 
RH_DF <- RH_DF %>% group_by(Zip) %>% mutate(ImpZillowMedianSld = impute.mean(Zill_Med_Sld))
RH_DF<- RH_DF %>% as.data.frame()
# Impute at region level 
RH_DF <- RH_DF %>% group_by(Region) %>% mutate(ImpZillowMedianSld1 = impute.mean(ImpZillowMedianSld))
RH_DF<- RH_DF %>% as.data.frame()
#Impute at State level for those remaining
RH_DF<- RH_DF %>% group_by(state) %>% mutate(ImpZillowMedianSld2 = impute.mean(ImpZillowMedianSld1))
RH_DF<- RH_DF %>% as.data.frame()
###############
# Impute Zillow Median sq ft
#Impute at zip code 
RH_DF <- RH_DF %>% group_by(Zip) %>% mutate(ImpZillowMedSqFt = impute.mean(Zill_Med_SqFt))
RH_DF<- RH_DF %>% as.data.frame()
# Impute at region level 
RH_DF <- RH_DF %>% group_by(Region) %>% mutate(ImpZillowMedSqFt1 = impute.mean(ImpZillowMedSqFt))
RH_DF<- RH_DF %>% as.data.frame()
#Impute at State level for those remaining
RH_DF<- RH_DF %>% group_by(state) %>% mutate(ImpZillowMedSqFt2 = impute.mean(ImpZillowMedSqFt1))
RH_DF<- RH_DF %>% as.data.frame()

######


#Split letter from Mosaic to create new feature
RH_DF$MosLetter <- substr(RH_DF$Mosaic_New, 1, 1) %>% as.factor()
#Catalog data
RH_DF$BC_Cat <- RH_DF$HHID %in% BC15_Mailed$V1 
RH_DF$TN_Cat <- RH_DF$HHID %in% TN15_Mailed$V1
#Engineer Average
RH_DF$RH_Avg <- RH_DF$val_RH / RH_DF$item_RH
#Account for missing values
RH_DF$RH_Avg <- ifelse(RH_DF$RH_Avg == "NaN", 0  ,RH_DF$RH_Avg) 
#Calc Avg Value
RH_DF$BC_Avg <- RH_DF$val_BC / RH_DF$item_BC
RH_DF$BC_Avg <- ifelse(RH_DF$BC_Avg == "NaN", 0  ,RH_DF$BC_Avg) 
RH_DF <- RH_DF[RH_DF$Foreign_Flag == "NtCNDN",] #Exclude Canadians due to missing zip codes 
RH_DF <- RH_DF[RH_DF$RH_flag == 1 | RH_DF$BC_Flag ==1, ] # assumption is, if there's no prior buying history, there's no prediction
#RH_DF <- RH_DF[,3:50]
#Clean up Variable name HHID for DataRobot consumption
names(RH_DF)[names(RH_DF)=="HHID"] <- "ID"
#set seed for training split
set.seed(3456)
#for i in 1:length(df){
#    distm (c(40.75074, -73.99653), c(40.75216, -73.97231), fun = distHaversine)}

trainIndex <- createDataPartition(RH_DF$TN_Flag, p = .8,
                                  list = FALSE,
                                  times = 1)
dfTrain <- RH_DF[trainIndex,] #Use this for exploration and modeling
dfTest <- RH_DF[-trainIndex,] #Use this for validation


#write.csv(dfTrain, file = "RH_Train_TN_Data3.csv")

#prop.table(table(dfTest$TN_Flag)) # check for proportionality - ok
#prop.table(table(dfTrain$TN_Flag))

#Create teen only data set for exploration
teens <- dfTrain[dfTrain$TN_Flag=="TN",]
table(BC = teens$BC_Cat, TN = teens$TN_Cat)
teens$Zip <- as.character(teens$Zip) #Clean zipcode

dfTrain %>% group_by(factor(BC_Flag))  %>% summarise(mean(Store))