## predictions on the test set 
test <- read.csv(file = "data/test.csv", check.names = FALSE, strip.white = TRUE)

## load models
load(file = "kaggle/NateAnalysis/m1.rda")
load(file = "kaggle/NateAnalysis/m2.rda")
load(file = "kaggle/NateAnalysis/stacked_rf_lrs_only.rda")


## feature generation
source(file = "kaggle/NateAnalysis/feature_generation_functions.R")

f1_test <- apply(test[,-c(1)] , MARGIN = 1, FUN = nates_normal_loglr_feat, 
                 mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
test$f1 <- f1_test

test_target_pred <- predict(m1, newdata = test, type = "response")

test_sub <- data.frame("id" = test$id, "target" = test_target_pred)

write.csv(x = test_sub, file = "kaggle/NateAnalysis/nate_sub1.csv", row.names = FALSE)

# kieg_pred <- read.csv("C://Users/Nate/Downloads/lasso_submission.csv")
# 
# plot(test_sub$target, kieg_pred$target)


## feature generation for model 2
## Model 2
lrs_test <- t(apply(X = test[,-c(1)], MARGIN = 1, FUN = nates_normal_loglr_feats2))
colnames(lrs_test) <- paste("lr", relevant_cols, sep = "_")

test_f2 <- cbind(test[,-1], lrs_test)

test_target_pred2 <- predict(m2, newdata = test_f2, type = "prob")

test_sub2 <- data.frame("id" = test$id, "target" = as.numeric(test_target_pred2[,2]))

write.csv(x = test_sub2, file = "kaggle/NateAnalysis/nate_sub2.csv", row.names = FALSE)


## Stacked RF with LR feature only for 23 relevant columns
lrs_test <- t(apply(X = test[,-c(1)], MARGIN = 1, FUN = nates_normal_loglr_feats2))
colnames(lrs_test) <- paste("lr", relevant_cols, sep = "_")

# test_f2 <- cbind(test[,-1], lrs_test)
## I think this is predicting probability of 0
test_target_pred3 <- 1 - predict(object = stacked_lrs_only, newdata = lrs_test, type = "prob")
test_sub3 <- data.frame("id" = test$id, "target" = as.numeric(test_target_pred3))
write.csv(x = test_sub3, file = "kaggle/NateAnalysis/nate_sub3.csv", row.names = FALSE)
