## script to train models
library(pROC)
source("kaggle/NateAnalysis/feature_generation_functions.R")
train <- read.csv("data/train.csv", check.names = FALSE, strip.white = TRUE)
f1 <- apply(X = train[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feat,
            mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
train_f <- data.frame("target" = train$target, "f1" = f1)
m1 <- glm(formula = target ~ f1, family = "binomial", data = train_f)
summary(m1)

save(m1, file = "kaggle/NateAnalysis/m1.rda")

# target_pred <- predict(object = m1, newdata = train_f, type = "response")
# 
# plot(roc(response = train_f$target, predictor = target_pred))
# roc(response = train_f$target, predictor = target_pred)
