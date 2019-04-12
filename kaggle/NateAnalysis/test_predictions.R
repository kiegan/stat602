## predictions on the test set 
test <- read.csv(file = "data/test.csv", check.names = FALSE, strip.white = TRUE)

## load models
load(file = "kaggle/NateAnalysis/m1.rda")
load(file = "kaggle/NateAnalysis/m2.rda")


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

test_target_pred2 <- predict.cv.glmnet(m2, newx = as.matrix(x = test_f2), type = "response", s = "lambda.min")

test_sub2 <- data.frame("id" = test$id, "target" = as.numeric(test_target_pred2))

write.csv(x = test_sub2, file = "kaggle/NateAnalysis/nate_sub2.csv", row.names = FALSE)
