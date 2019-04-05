## Nate's feature generation functions

## FEATURE 1: LR of 300 dimensional multivariate normal with identity covariance matrix and 
##  specific means for each class
train <- read.csv(file = "data/train.csv", check.names = FALSE, strip.white = TRUE)
head(train)
train_c0 <- train[train$target == 0,]
train_c1 <- train[train$target == 1,]

## create likelihood ratio based on multivariate normal modeling where variables "33", "65" have unique means but 
##  remaining means are the same within a class

## actual feature generation not test code
mu_c0_33 <- mean(train_c0$`33`)
mu_c0_65 <- mean(train_c0$`65`)
mu_c1_33 <- mean(train_c1$`33`)
mu_c1_65 <- mean(train_c1$`65`)

mu_c0_remain <- mean(apply(X = train_c0[,-c(1,2, 34 + 2,66 + 2)], MARGIN = 2, FUN = mean))
mu_c1_remain <- mean(apply(X = train_c1[,-c(1,2, 34 + 2,66 + 2)], MARGIN = 2, FUN = mean))
mu_c0_remain
mu_c1_remain

mod_mu_c0 <- numeric()
mod_mu_c1 <- numeric()
for(i in 1:300)
{
  mod_mu_c0[i] <- ifelse(test = i == 34, yes = mu_c0_33, 
                         no = ifelse(test = i == 66, yes = mu_c0_65, no = mu_c0_remain))
  mod_mu_c1[i] <- ifelse(test = i == 34, yes = mu_c1_33, 
                         no = ifelse(test = i == 66, yes = mu_c1_65, no = mu_c1_remain))
}
mod_mu_c0
mod_mu_c1


nates_normal_loglr_feat <- function(x, mu0, mu1, prior_c0, prior_c1)
{
  x <- as.numeric(x)
  lr <- mvtnorm::dmvnorm(x = x , mean = mu1, sigma = diag(length(x)), log = TRUE) - 
    mvtnorm::dmvnorm(x = x , mean = mu0, sigma = diag(length(x)), log = TRUE) - 
    log(prior_c1/prior_c0)
  
  return(lr)
}

## temporarily create the means based on a subset of the data since my first submission was crap
##    just to test things
# ind0 <- sample.int(n = nrow(train_c0), replace = FALSE, size = 0.8 * nrow(train_c0))
# ind1 <- sample.int(n = nrow(train_c1), replace = FALSE, size = 0.8 * nrow(train_c1))
# 
# mu_c0_33 <- mean(train_c0[ind0,]$`33`)
# mu_c0_65 <- mean(train_c0[ind0,]$`65`)
# mu_c1_33 <- mean(train_c1[ind1,]$`33`)
# mu_c1_65 <- mean(train_c1[ind1,]$`65`)
# 
# mu_c0_remain <- mean(apply(X = train_c0[ind0,-c(1,2, 34 + 2,66 + 2)], MARGIN = 2, FUN = mean))
# mu_c1_remain <- mean(apply(X = train_c1[ind1,-c(1,2, 34 + 2,66 + 2)], MARGIN = 2, FUN = mean))
# mu_c0_remain
# mu_c1_remain
# 
# mod_mu_c0 <- numeric()
# mod_mu_c1 <- numeric()
# for(i in 1:300)
# {
#   mod_mu_c0[i] <- ifelse(test = i == 34, yes = mu_c0_33, 
#                           no = ifelse(test = i == 66, yes = mu_c0_65, no = mu_c0_remain))
#   mod_mu_c1[i] <- ifelse(test = i == 34, yes = mu_c1_33, 
#                          no = ifelse(test = i == 66, yes = mu_c1_65, no = mu_c1_remain))
# }
# mod_mu_c0
# mod_mu_c1
# 
# 
# nates_normal_loglr_feat <- function(x, mu0, mu1, prior_c0, prior_c1)
# {
#   x <- as.numeric(x)
#   lr <- mvtnorm::dmvnorm(x = x , mean = mu1, sigma = diag(length(x)), log = TRUE) - 
#     mvtnorm::dmvnorm(x = x , mean = mu0, sigma = diag(length(x)), log = TRUE) - 
#     log(prior_c1/prior_c0)
#   
#   return(lr)
# }
# 
# # nates_normal_loglr_feat(x = train_c0[1,-c(1,2)], mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# # 
# f1_c0 <- apply(X = train_c0[ind0,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feat,
#             mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# f1_c1 <- apply(X = train_c1[ind1,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feat,
#                mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# 
# hist(f1_c0)
# hist(f1_c1)
# 
# mean(f1_c0)
# mean(f1_c1)
# 
# f1 <- apply(X = rbind(train_c0[ind0, -c(1,2)], train_c1[ind1, -c(1,2)]), MARGIN = 1, FUN = nates_normal_loglr_feat, 
#             mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# 
# train_train = rbind(train_c0[ind0,], train_c1[ind1,])
# train_train$f1 <- f1
# 
# logreg <- glm(formula = target ~ f1, family = "binomial", data = train_train)
# summary(logreg)
# 
# train_test <- rbind(train_c0[-ind0,], train_c1[-ind1,])
# f1_train_test <- apply(X = train_test[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feat, mu0 = mod_mu_c0,
#                        mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# train_test$f1 <- f1_train_test
# 
# 
# predict(object = logreg, newdata = train_test, type = "response")
# predict(object = m1, newdata = train_test, type = "response")
# 
# roc(response = train_test$target, predictor = predict(object = logreg, newdata = train_test, type = "response")
# )
# 
# roc(response = train_test$target, predictor = predict(object = m1, newdata = train_test, type = "response")
# )
# 
# hist(predict(object = logreg, newdata = train_test, type = "response")[train_test$target == 0])
# hist(predict(object = logreg, newdata = train_test, type = "response")[train_test$target == 1])

# nates_normal_loglr_feat(x = train_c0[1,-c(1,2)], mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# 
# f1_c0 <- apply(X = train_c0[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feat,
#                mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# f1_c1 <- apply(X = train_c1[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feat,
#                mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# 
# hist(f1_c0)
# hist(f1_c1)
# 
# mean(f1_c0)
# mean(f1_c1)
# 
# f1 <- apply(X = train[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feat,
#             mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# train_f <- train
# train_f$f1 <- f1

# logreg <- glm(formula = target ~ f1, family = "binomial", data = train_f)
# summary(logreg)


