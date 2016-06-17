library(ggplot2)
library(ggthemes)
xgb <- read.csv("~/RTMW/eXtreme Gradient Boosted Trees Classifier with Early Stopping (Validation) (BC_Avg) Model X-Ray  (Omitted Data Percent-  0%)  (Data is Capped) PD.csv", stringsAsFactors=FALSE)
ggplot(data = xgb, aes(x = feature_value, y = partial_dependence)) + 
  geom_line() + geom_point() + xlab("Average Baby&Child Purchase") +
  ylab("Partial Dependence") + scale_x_continuous(breaks = c(25,50,75,100,125,150,175,200)) + 
  ggtitle("Partial Dependence Plot", subtitle = "Marginal effect of a variable on the class probability")+
  theme(panel.background = element_blank())


L2 <- read.csv("~/Regularized Logistic Regression (L2) Feature Coefficients 32.0 % Sample Size.csv", stringsAsFactors=FALSE)

pos <- L2[L2$coefficient>0,]
pos <- pos[1:10,]
color <- c(NA,NA,NA,NA,NA,NA,"Engineered Feature",NA,NA,NA)
ggplot(data = pos, aes(x = reorder(feature_name, coefficient), y = coefficient, fill= color))+ 
  geom_bar(stat = "identity")+coord_flip() + labs(y='Standardized Coefficient',x='Variable')+
  ggtitle("Top 10 Positive Standardized Coefficients for Regularized Logistic Regression") +
  theme(legend.position="none",panel.background = element_blank())


negs <- L2[L2$coefficient<0,]
negs <- negs[1:10,]
color2 <- c(NA,NA,NA,NA,"Engineered Feature",NA,NA,NA,NA,NA)
ggplot(data = negs, aes(x = reorder(feature_name, -coefficient), y = coefficient, fill = color2))+ 
  geom_bar(stat = "identity")+coord_flip() + labs(y='Standardized Coefficient',x='Variable')+
  ggtitle("Top 10 Negative Standardized Coefficients for Regularized Logistic Regression")+
  theme(legend.position="none", panel.background = element_blank())
  
