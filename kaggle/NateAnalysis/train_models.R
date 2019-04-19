## script to train models
library(pROC)
library(glmnet)
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

## Model 2
lrs <- t(apply(X = train[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feats2))
colnames(lrs) <- paste("lr", relevant_cols, sep = "_")

train_f2 <- cbind(train[,-1], lrs)
m2 <- cv.glmnet(x = as.matrix(train_f2[,-1]), y = train_f2$target, family = "binomial", type.measure = "auc")
# summary(m2)

# predict.cv.glmnet(object = m2, newx = as.matrix(train_f2[,-1]), s = "lambda.min", type = "response")

save(m2, file = "kaggle/NateAnalysis/m2.rda")

## Models glm, knn, and xgboost using only lr features 
train$target <- as.factor(train$target)
levels(train$target) <- c("n","y")
fitControl <- trainControl(method = "cv", number = 10, 
                           classProbs = TRUE, 
                           search = "grid", index = createFolds(y = as.factor(train$target), k = 10, returnTrain = TRUE), 
                           savePredictions = TRUE)
caret_mods_lrs_only <-  caretEnsemble::caretList(y = as.factor(train$target), x = lrs, methodList = mods, 
                                       metric = "Accuracy", trControl = fitControl)
modelCor(x = resamples(caret_mods_lrs_only))

save(caret_mods_lrs_only,file =  "kaggle/NateAnalysis/caret_mods_lrs_only.rda")

## ensemble these models with generalized stacking
stacked_lrs_only <- caretEnsemble::caretStack(all.models = caret_mods_lrs_only,
                                              method = "rf",
                                              trControl = trainControl(method = "cv",
                                                                       number = 10, 
                                                                       classProbs = TRUE))
stacked_lrs_only
