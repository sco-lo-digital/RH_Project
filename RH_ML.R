#THESE TAKE A LONG TIME TO RUN
source("TidyData_RH.R")

#table(RH_DF$TN_Flag, RH_DF$POC1012Flag) %>% prop.table(margin = 2) 

set.seed(143)
down_train <- downSample(x = dfTrain2,
                         y = dfTrain2$TN_Flag)
down_train <- down_train[,-ncol(down_train)]
table(down_train$TN_Flag)
write.csv(dfTrain2, file = "dfTrain3.csv")

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)



myTuneGrid <- expand.grid(n.trees=seq(25,300,25), interaction.depth = 1:5, shrinkage = 0.1, n.minobsinnode = 100)

set.seed(143)
RH_gbm <- train(TN_Flag ~ ImpHmVal2 + RHPC1 + RHPC2 + RH_Avg + RHPC3 + RHPC4,
                     data = down_train,
                     method = "gbm",
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     trControl = ctrl
                    )
confusionMatrix(RH_gbm)

RH_glm <- train(TN_Flag ~ HH_BuyerType + channelUsage + state + BCPC1 + BCPC2 + BCPC3 + BCPC4 + BCPC5,
                     data = down_train,
                     method = "glm",
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     trControl = ctrl)
confusionMatrix(RH_glm)

RH_rpart <- train(TN_Flag ~ Est_Home_Value,
               data = down_train,
               method = "rpart",
               tuneLength = 20,
               metric = "ROC",
               preProc = c("center", "scale"),
               trControl = ctrl)

RH_nb <- train(TN_Flag ~ HH_BuyerType + channelUsage + state + BCPC1 + BCPC2 + BCPC3 + BCPC4 + BCPC5,
                     data = down_train,
                     method = "nb",
                     metric = "ROC",
                     trControl = ctrl)
confusionMatrix(RH_nb)
