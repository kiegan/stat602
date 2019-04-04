## Nate's feature generation functions

## FEATURE 1: LR of 300 dimensional multivariate normal with identity covariance matrix and 
##  specific means for each class
train <- read.csv(file = "data/train.csv", check.names = FALSE, strip.white = TRUE)
head(train)
train_c0 <- train[train$target == 0,]
train_c1 <- train[train$target == 1,]

## create likelihood ratio based on multivariate normal modeling where variables "33", "65" have unique means but 
##  remaining means are the same within a class
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
  mod_mu_c0 [i] <- ifelse(test = i == 34, yes = mu_c0_33, 
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

# nates_normal_loglr_feat(x = train_c0[1,-c(1,2)], mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# 
# f1_c0 <- apply(X = train_c0[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feat, 
#             mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# f1_c1 <- apply(X = train_c1[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feat, 
#                mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)
# 
# hist(f1_c0)
# hist(f1_c1)
# 
# mean(f1_c0)
# mean(f1_c1)

f1 <- apply(X = train[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feat, 
            mu0 = mod_mu_c0, mu1 = mod_mu_c1, prior_c0 = 90/250, prior_c1 = 160/250)

# logreg <- glm(formula = train$target ~ f1, family = "binomial")
# summary(logreg)
