library(VIM)
missing_cols <- colnames(RH_DF)[colSums(is.na(RH_DF))>0]
missing_dat <- RH_DF[, missing_cols]
aggr_plot <- aggr(missing_dat, col=c('grey88', 'lightcoral'), 
                  numbers = TRUE, sortVars = TRUE, prop = FALSE, labels = names(missing_dat), 
                  cex.axis = .5, gap = 3, ylab = c("Histogram of missing data", "Pattern"))



