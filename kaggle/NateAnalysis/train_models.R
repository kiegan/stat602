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
train_f2$target <- as.factor(train_f2$target)
levels(train_f2$target) <- c("n","y")
m2 <- cv.glmnet(x = as.matrix(train_f2[,-1]), y = train_f2$target, family = "binomial", type.measure = "auc")
summary(m2)
m2$glmnet.fit
coefficients(m2, lambda = "lambda.min")

# train_preds_m2 <- predict.cv.glmnet(object = m2, newx = as.matrix(train_f2[,-1]), s = "lambda.min", type = "response")
# table(1 * (train_preds_m2 > 0.5), train_f2$target)
# 35 / nrow(train_preds_m2)

# fitControl1 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, 
#                            classProbs = TRUE, 
#                            search = "grid", index = createFolds(y = train_f2$target, k = 5, returnTrain = TRUE), 
#                            savePredictions = TRUE)
# caret_mods <-  caretEnsemble::caretList(y = train_f2$target, x = train_f2[,-1], methodList = mods, 
#                                                  metric = "Accuracy", trControl = fitControl1)
# caret_mods
# 
# m2 <- caret::train(x = train_f2[,-1], y = train_f2$target, 
#                    method = "glmnet", metric = "Accuracy",
#                    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, search = "grid"),
#                    tuneGrid = expand.grid(alpha = seq(from = 0.05, to = 1, by = 0.05),
#                                           lambda = seq(from = 0.01, to = 0.1, by = 0.01)))
# m2
# m2$results
# 
# train_preds <- predict(object = m2, newdata = train_f2[,-1], type = "prob")[,2]
# roc(response = train$target, predictor = train_preds)

save(m2, file = "kaggle/NateAnalysis/m2.rda")

## try glm with just variables 33 and 65
m2.1 <- glm(formula = target ~ `33` + `65`, family = "binomial", data = train_f2)
save(m2.1, file = "kaggle/NateAnalysis/m2_1.rda")

## Models glmnet, knn, and xgboost using only lr features 
## xgbTree, glmnet, and knn seem to work reasonably well
set.seed(1308)
mods2 <- c("xgbTree","glmnet","knn")
train$target <- as.factor(train$target)
levels(train$target) <- c("n","y")
fitControl2 <- trainControl(method = "cv", number = 5,
                            index = createFolds(y = train$target, k = 5, returnTrain = TRUE),
                           classProbs = TRUE, 
                           search = "grid", 
                           savePredictions = "final")
caret_mods_lrs_only <-  caretEnsemble::caretList(y = as.factor(train$target), x = lrs, methodList = mods2, 
                                       metric = "Accuracy", trControl = fitControl2)

caret_mods_lrs_only
modelCor(x = resamples(caret_mods_lrs_only))

caret_mods_lrs_only$xgbTree$results
caret_mods_lrs_only$glmnet$results
caret_mods_lrs_only$knn$results



save(caret_mods_lrs_only,file =  "kaggle/NateAnalysis/caret_mods_lrs_only.rda")

## ensemble these models with generalized stacking
tg <- data.frame("mtry" = c(1,2,3))
set.seed(1308)
stacked_lrs_only <- caretEnsemble::caretStack(all.models = caret_mods_lrs_only,
                                              method = "rf",
                                              trControl = trainControl(method = "repeatedcv",
                                                                       number = 5,
                                                                       repeats = 50,
                                                                       classProbs = TRUE,
                                                                       index = createFolds(y = as.factor(train$target), k = 5, returnTrain = TRUE)),
                                              tuneGrid = tg)

stacked_lrs_only$error

train_preds <- predict(object = stacked_lrs_only, newdata = lrs, type = "prob")
roc(response = train$target, predictor = train_preds)

save(stacked_lrs_only, file = "kaggle/NateAnalysis/stacked_rf_lrs_only.rda")


## Model 4, boosted tree
lrs <- t(apply(X = train[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feats2))
colnames(lrs) <- paste("lr", relevant_cols, sep = "_")

train_f2 <- cbind(train[,-1], lrs)
train_f2$target <- as.factor(train_f2$target)
levels(train_f2$target) <- c("n","y")
# m2 <- cv.glmnet(x = as.matrix(train_f2[,-1]), y = train_f2$target, family = "binomial", type.measure = "auc")
# summary(m2)

# train_preds_m2 <- predict.cv.glmnet(object = m2, newx = as.matrix(train_f2[,-1]), s = "lambda.min", type = "response")
# table(1 * (train_preds_m2 > 0.5), train_f2$target)
# 35 / nrow(train_preds_m2)

m4 <- caret::train(x = train_f2[,-1], y = train_f2$target, 
                   method = "xgbTree", metric = "Accuracy",
                   trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, search = "grid"))
m4
m4$results

train_preds <- predict(object = m4, newdata = train_f2[,-1], type = "prob")[,2]
roc(response = train$target, predictor = train_preds)

save(m4, file = "kaggle/NateAnalysis/m4.rda")
