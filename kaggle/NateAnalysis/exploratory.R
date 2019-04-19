## Explore Kaggle Don't Overfit II data
library(caret)
library(ggplot2)
library(caretEnsemble)
train <- read.csv(file = "data/train.csv", check.names = FALSE, strip.white = TRUE)


head(train)

## 250 x 302
dim(train)
summary(train[,-c(1,2)])

## look at summary statistics
hist(apply(X = as.matrix(train[,-c(1:2)]), MARGIN = 2, FUN = min))
hist(apply(X = as.matrix(train[,-c(1,2)]), MARGIN = 2, FUN = max))
hist(apply(X = as.matrix(train[,-c(1,2)]), MARGIN = 2, FUN = mean)) ## looks suspiciously normal
hist(apply(X = as.matrix(train[,-c(1,2)]), MARGIN = 2, FUN = sd)) ## most columns have sd = 1

## class probabilities
table(train$target)

bsd <- sqrt((160 / 250) * (1 - 160/250) / 250)
ci_prior_p <- c(160/250 - 1.96 * bsd , 160/250 + 1.96 * bsd)
ci_prior_p

## look at summary statistics for class distributions individually
train_c0 <- train[train$target == 0,]
train_c1 <- train[train$target == 1,]

## class 0
hist(apply(X = as.matrix(train_c0[,-c(1:2)]), MARGIN = 2, FUN = min))
hist(apply(X = as.matrix(train_c0[,-c(1,2)]), MARGIN = 2, FUN = max))
hist(apply(X = as.matrix(train_c0[,-c(1,2)]), MARGIN = 2, FUN = mean)) ## looks suspiciously like iid normal
hist(apply(X = as.matrix(train_c0[,-c(1,2)]), MARGIN = 2, FUN = sd)) ## most columns have sd = 1


## class 1
hist(apply(X = as.matrix(train_c1[,-c(1:2)]), MARGIN = 2, FUN = min))
hist(apply(X = as.matrix(train_c1[,-c(1,2)]), MARGIN = 2, FUN = max))
hist(apply(X = as.matrix(train_c1[,-c(1,2)]), MARGIN = 2, FUN = mean)) ## looks suspiciously like iid normal
hist(apply(X = as.matrix(train_c1[,-c(1,2)]), MARGIN = 2, FUN = sd)) ## most columns have sd = 1

## compare means
mu_c1 <- apply(X = as.matrix(train_c1[,-c(1,2)]), MARGIN = 2, FUN = mean)
mu_c0 <- apply(X = as.matrix(train_c0[,-c(1,2)]), MARGIN = 2, FUN = mean)

hist(mu_c0) ## looks like one mean is suspiciously far to the left? 
which.min(mu_c0[-c(34,66)])
hist(train_c0$`33`)
hist(train_c0$`65`)
hist(train_c0$`24`)
mean(train_c0$`24`)

aha <- t.test(x = train_c0$`33`, mu = 0) ## column 33 has a nonzero mean
aha$p.value * 300

ks.test(x = train_c0$`33`, y = "pnorm", mean = mean(train_c0$`33`), sd = sd(train_c0$`33`))
shapiro.test(x = train_c0$`33`)
qqnorm(y = (train_c0$`33` - mean(train_c0$`33`)) / sd(train_c0$`33`) )
abline(a = 0, b = 1)

hist(mu_c1)
mean(mu_c0)
mean(mu_c1)

plot(mu_c0, mu_c1)

## compare the overall distributions of means
sd(mu_c0[-34] / sqrt(1/nrow(train_c0)))
sd(mu_c1 / sqrt(1/nrow(train_c1)))

## looks like the overall mean distributions are a little different location-wise
t.test(x = mu_c0[-c(34,66)] / sqrt(1/nrow(train_c0)), y = mu_c1[-c(34,66)] / sqrt(1/nrow(train_c1)), mu = 0, var.equal = TRUE) 
t.test(x = mu_c0[-c(34,66)] / sqrt(1/nrow(train_c0)), y = mu_c1[-c(34,66)] / sqrt(1/nrow(train_c1)), mu = 0, var.equal = TRUE, paired = TRUE) 

## there are a couple differences between means that exceed normal range
hist(mu_c0[-c(34,66)] - mu_c1[-c(34,66)])
which.min(mu_c0 - mu_c1) ## one of them is for "33" but the other is for "65"
mean(train_c0$`65`)
mu_c0[66]

mean(train_c1$`65`)
mu_c1[66]

aha2 <- t.test(x = train_c0$`33`, y = train_c1$`33`, mu = 0, paired = FALSE, var.equal = TRUE)
aha2$p.value * 300

aha3 <- t.test(x = train_c0$`65`, y = train_c1$`65`, mu = 0, paired = FALSE, var.equal = TRUE)
aha3$p.value * 300

qqnorm(y = (mu_c0[-c(34,66)] - mean(mu_c0[-c(34,66)])) / sqrt(var(mu_c0[-c(34,66)])))
abline(a = 0, b = 1)
ks.test(x = mu_c0[-c(34,66)], y = "pnorm", mean = 0, sd = sqrt(1/nrow(train_c0)))
shapiro.test(x = (mu_c0[-c(34,66)] - mean(mu_c0[-c(34,66)])) / sqrt(var(mu_c0[-c(34,66)])))
shapiro.test(x = (mu_c0[-c(34,66)] - mean(mu_c0[-c(34,66)])) / sqrt(1/nrow(train_c0)))


qqnorm(y = (mu_c1 - mean(mu_c1)) / sqrt(var(mu_c1)))
abline(a = 0, b = 1)
ks.test(x = mu_c1, y = "pnorm", mean = 0, sd = sqrt(1/nrow(train_c1)))
shapiro.test(x = (mu_c1 - mean(mu_c1)) / sqrt(var(mu_c1)))
shapiro.test(x = (mu_c1 - mean(mu_c1)) / sqrt(1/nrow(train_c1)))

## look at distribution of differences 
## theoretical variance of a difference is 1/90 + 1/160
diffs <- mu_c0[-c(34,66)] - mu_c1[-c(34,66)]
var(diffs)
1/90 + 1/160
var(diffs) / (1/90 + 1/160)
299 * var(diffs) / (1/90 + 1/160)

## sample variance of difference of means p-value
1 - pchisq(q = ((300 - 1)/(1/90 + 1/160)) * var(diffs) , df = 300 - 1)


## distribution of differences 
qqnorm(y = (diffs - mean(diffs)) / sqrt(var(diffs)))
abline(a = 0, b = 1)
ks.test(x = (diffs - mean(diffs)) / sqrt(var(diffs)), y = "pnorm", mean = 0, sd = 1)
shapiro.test(x = (diffs - mean(diffs)) / sqrt(var(diffs)))
shapiro.test(x = (diffs - mean(diffs)) / sqrt(1/90 + 1/160))



## do a bunch of t-tests comparing column means between classes
ttests <- list()
tstats <- numeric()
pvals <- numeric()
for(i in 1:ncol(train[,-c(1,2)]))
{
  ttests[[i]] <- t.test(x = train_c0[,i + 2], y = train_c1[,i + 2], 
                      mu = 0, 
                      paired = FALSE, var.equal = TRUE, conf.level = 0.95)
  tstats[i] <- ttests[[i]]$statistic
  pvals[i] <- ttests[[i]]$p.value
}

sort(pvals)
order(pvals)
sum(pvals * 300 < 0.05)

ttests[[25]]
ttests[[25]]$p.value * 300
ttests[[34]]
ttests[[66]]
hist(tstats)
hist(pvals)
ks.test(x = pvals, y = "punif", min = 0, max = 1) ## it looks like the data are very close to just iid N(0,1)


hist(pvals[-c(34,66)])
ks.test(x = pvals[-c(34,66)], y = "punif", min = 0, max = 1) ## it looks like the data are very close to just iid N(0,1)

## compare sds
sd_c1 <- apply(X = as.matrix(train_c1[,-c(1,2)]), MARGIN = 2, FUN = sd)
sd_c0 <- apply(X = as.matrix(train_c0[,-c(1,2)]), MARGIN = 2, FUN = sd)

plot(sd_c0, sd_c1)

## look at correlations
cor_c0 <- numeric()
cor_c1 <- numeric()
counter <- 0
for(i in 1:(ncol(train[,-c(1,2)]) - 1))
{
  print(i)
  for(j in (i + 1):ncol(train[,-c(1,2)]))
  {
    counter <- counter + 1
    cor_c0[counter] <- cor(x = train_c0[,i + 2], y = train_c0[,j + 2])
    cor_c1[counter] <- cor(x = train_c1[,i + 2], y = train_c1[,j + 2])
    
  }
}

hist(cor_c0)
hist(cor_c1)
mean(cor_c0)
mean(cor_c1)


plot(cor_c0, cor_c1)


## look at class conditional distance distributions
d_c0 <- numeric()
d_c1 <- numeric()
counter <- 0
for(i in 1:(nrow(train_c0) - 1))
{ print(i)
  for(j in (i + 1):nrow(train_c0))
  {
    counter <- counter + 1
    d_c0[counter] <- sum( (train_c0[i,-c(1,2)] - train_c0[j,-c(1,2)])^2 )
  }
}
hist(d_c0)
mean(d_c0)

counter <- 0
for(i in 1:(nrow(train_c1) - 1))
{ print(i)
  for(j in (i + 1):nrow(train_c1))
  {
    counter <- counter + 1
    d_c1[counter] <- sum( (train_c1[i,-c(1,2)] - train_c1[j,-c(1,2)])^2 )
  }
}
hist(d_c1)
mean(d_c1)


## do likelihood ratio test for whether all variables in a class have the same mean
## it seems like *maybe* means are slightly different, but it's not easy to tell which...
library(mvtnorm)
lrstat0.1 <- -2 * (sum( apply(X = train_c0[,-c(1,2, 34 + 2, 66 + 2)], MARGIN = c(1,2), FUN = dnorm, log = TRUE, mean = mean(mu_c0[-c(34,66)]), sd = sqrt(var(as.numeric(as.matrix(train_c0[,-c(1,2,34,66)])))
) ) ) - 
                    sum( apply(X = train_c0[,-c(1,2, 34 + 2, 66 + 2)] - matrix(rep(x = mu_c0[-c(34,66)], times = nrow(train_c0)), nrow = nrow(train_c0), byrow = TRUE), MARGIN = 1, FUN = dmvnorm, sigma = var(as.numeric(as.matrix(train_c0[,-c(1,2,34,66)]))
) * diag(ncol(train_c0) - 2 - 2), log = TRUE) )) 
lrstat0.1
1 - pchisq(q = lrstat0.1, df = 298 - 1)

lrstat1.1 <- -2 * (sum( apply(X = train_c1[,-c(1,2, 34 + 2, 66 + 2)], MARGIN = c(1,2), FUN = dnorm, log = TRUE, mean = mean(mu_c1[-c(34,66)]), sd = sqrt(var(as.numeric(as.matrix(train_c1[,-c(1,2,34,66)])))
) ) ) - 
                     sum( apply(X = train_c1[,-c(1,2, 34 + 2, 66 + 2)] - matrix(rep(x = mu_c1[-c(34,66)], times = nrow(train_c1)), nrow = nrow(train_c1), byrow = TRUE), MARGIN = 1, FUN = dmvnorm, sigma = var(as.numeric(as.matrix(train_c1[,-c(1,2,34,66)])))
 * diag(ncol(train_c1) - 2 - 2), log = TRUE) )) 
lrstat1.1
1 - pchisq(q = lrstat1.1, df = 288 - 1)

## What if column means were generated from a normal with separate means for the two classes 
var(as.numeric(as.matrix(train_c0[,-c(1,2,34,66)])))
var(as.numeric(as.matrix(train_c1[,-c(1,2,34,66)])))

## create likelihood ratio based on multivariate normal modeling where variables "33", "65" have unique means but 
##  remaining means are the same within a class
mu_c0_33 <- mean(train_c0$`33`)
mu_c0_65 <- mean(train_c0$`65`)
mu_c1_33 <- mean(train_c1$`33`)
mu_c1_65 <- mean(train_c1$`65`)

mu_c0_remain <- mean(apply(X = train_c0[,-c(34 + 2,66 + 2)], MARGIN = 2, FUN = mean))
mu_c1_remain <- mean(apply(X = train_c1[,-c(34 + 2,66 + 2)], MARGIN = 2, FUN = mean))
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

## do likelihood ratio on columns left in by lasso (lambda min) on second model
mu_vec_lasso1_c1 <- numeric()
mu_vec_lasso1_c0 <- numeric()
sd_vec_lasso1_c1 <- numeric()
sd_vec_lasso1_c0 <- numeric()

counter <- 0
ones_left <- c(30,43,73,80,82,90,91,114,117,133,168,189,194,199,217,221,226,252,258,295,298)
for(i in c(30,43,73,80,82,90,91,114,117,133,168,189,194,199,217,221,226,252,258,295,298))
{
  counter <- counter + 1
  mu_vec_lasso1_c0[counter] <- mean(train_c0[,i])
  mu_vec_lasso1_c1[counter] <- mean(train_c0[,i])
  sd_vec_lasso1_c0[counter] <- sd(train_c0[,i])
  sd_vec_lasso1_c1[counter] <- sd(train_c1[,i])
}

lrstat0.2 <- -2 * (sum( apply(X = train_c0[,ones_left], MARGIN = c(1,2), FUN = dnorm, log = TRUE, mean = mean(mu_c0[ones_left + 1]), sd = sqrt(var(as.numeric(as.matrix(train_c0[,ones_left + 2 + 1])))
) ) ) - 
  sum( apply(X = train_c0[,ones_left + 2 + 1] - matrix(data = rep(x = mu_c0[ones_left + 1], times = nrow(train_c0)), nrow = nrow(train_c0), byrow = TRUE), MARGIN = 1, FUN = dmvnorm, sigma = var(as.numeric(as.matrix(train_c0[,ones_left + 2 + 1])))
             * diag(length(ones_left)), log = TRUE) )) 
lrstat0.2
1 - pchisq(q = lrstat0.2, df = (length(ones_left) - 1))

lrstat1.2 <- -2 * (sum( apply(X = train_c1[,ones_left + 3], MARGIN = c(1,2), FUN = dnorm, log = TRUE, mean = mean(mu_c1[ones_left + 1]), sd = sqrt(var(as.numeric(as.matrix(train_c1[,ones_left + 3])))
) ) ) - 
  sum( apply(X = train_c1[,ones_left + 3] - matrix(rep(x = mu_c1[ones_left + 1], times = nrow(train_c1)), nrow = nrow(train_c1), byrow = TRUE), MARGIN = 1, FUN = dmvnorm, sigma = var(as.numeric(as.matrix(train_c1[,ones_left + 3])))
             * diag(length(ones_left)), log = TRUE) )) 
lrstat1.2
1 - pchisq(q = lrstat1.2, df = (length(ones_left) - 1))

## Make note of columns (column names) that we want to model with various means
relevant_cols <- c(33,65, ones_left)
save(relevant_cols, file = "kaggle/NateAnalysis/interesting_columns.rda")

hist(cor(x = train_c0[,3 + relevant_cols]))
hist(cor(x = train_c1[,3 + relevant_cols]))


## correlations of relevant columns
## summary: still thinking there are probably no correlations... if there are any they may be in class 0
relevant_col_cors_c0 <- numeric()
relevant_col_cors_c1 <- numeric()
cor_test_c0 <- numeric()
cor_test_c1 <- numeric()
counter <- 0
for(i in relevant_cols[-length(relevant_cols)])
{
  for(j in relevant_cols[(which(relevant_cols == i) + 1):length(relevant_cols)])
  {
    counter <- counter + 1
    relevant_col_cors_c0[counter] <- cor(train_c0[,i + 3], train_c0[,j + 3]) 
    relevant_col_cors_c1[counter] <- cor(train_c1[,i + 3], train_c1[,j + 3]) 
    cor_test_c0[counter] <- 2 * (1 - pt(q = abs(relevant_col_cors_c0[counter]) * sqrt(nrow(train_c0) - 2) / sqrt(1 - relevant_col_cors_c0[counter]^2), df = nrow(train_c0) - 2))
    cor_test_c1[counter] <- 2 * (1 - pt(q = abs(relevant_col_cors_c1[counter]) * sqrt(nrow(train_c1) - 2) / sqrt(1 - relevant_col_cors_c1[counter]^2), df = nrow(train_c1) - 2))
  }
}
hist(cor_test_c0)
hist(cor_test_c1)
ks.test(x = cor_test_c0, y = "punif")
ks.test(x = cor_test_c1, y = "punif")
relevant_col_cors_c0[order(cor_test_c0)][1:10]
relevant_col_cors_c1[order(cor_test_c1)][1:10]

plot(relevant_col_cors_c0, relevant_col_cors_c1)
hist(relevant_col_cors_c0 - relevant_col_cors_c1)


## normality tests for all relevant columns (yes everything looks normal)
norm_tests_c0 <- numeric()
norm_tests_c1 <- numeric()
for(i in 1:length(relevant_cols)){
  norm_tests_c0[i] <- shapiro.test(x = train_c0[,relevant_cols[i] + 3])$p.value
  norm_tests_c1[i] <- shapiro.test(x = train_c1[,relevant_cols[i] + 3])$p.value
}
hist(norm_tests_c0)
hist(norm_tests_c1)

ks.test(norm_tests_c0, y = "punif")
ks.test(norm_tests_c1, y = "punif")

## normality test for all columns (yes everything looks normal)
# norm_tests_c0 <- numeric()
# norm_tests_c1 <- numeric()
# for(i in 1:300){
#   norm_tests_c0[i] <- shapiro.test(x = train_c0[,i + 2])$p.value
#   norm_tests_c1[i] <- shapiro.test(x = train_c1[,i + 2])$p.value
# }
# hist(norm_tests_c0)
# hist(norm_tests_c1)
# 
# ks.test(norm_tests_c0, y = "punif")
# ks.test(norm_tests_c1, y = "punif")


## look at distances between LR features 
lrs <- t(apply(X = train[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feats2))
colnames(lrs) <- paste("lr", relevant_cols, sep = "_")

dist_lrs <- numeric()
cls <- numeric()
cnt <- 0
for(i in 1:(nrow(lrs) - 1))
{
  for(j in (i + 1):nrow(lrs))
  {
    cnt <- cnt + 1
    cls[cnt] <- 1*(train[i,]$target == train[j,]$target)
    dist_lrs[cnt] <- sum( (lrs[i,] - lrs[j,])^2 )
  }
}

par(mfrow = c(3,1))
hist(dist_lrs)
hist(dist_lrs[cls == 1])
hist(dist_lrs[cls == 0])


## look at how models do that only consider the lr features of interesting variables (relevant_cols)

## create a feature for predictions from m2 
lrs <- t(apply(X = train[,-c(1,2)], MARGIN = 1, FUN = nates_normal_loglr_feats2))
colnames(lrs) <- paste("lr", relevant_cols, sep = "_")

train_f2 <- cbind(train[,-1], lrs)
load(file = "kaggle/NateAnalysis/m2.rda")
fpredsm2 <- predict.cv.glmnet(object = m2, newx = as.matrix(train_f2[,-1]), s = "lambda.min", type = "response")

train$target <- as.factor(train$target)
levels(train$target) <- c("n","y")
# mods <- c("knn", "svmRadial", "rpart", "nnet","xgbTree")
# mods <- c("knn", "rpart", "nnet","xgbTree", "glmnet")
mods <- c("knn","xgbTree", "glmnet")

fitControl <- trainControl(method = "cv", number = 10, p = 0.8, 
                           classProbs = TRUE, 
                           search = "grid", index = createFolds(y = as.factor(train$target), k = 10))
train_mods <- caretEnsemble::caretList(y = as.factor(train$target), x = lrs, methodList = mods, 
                                       metric = "ROC", trControl = fitControl)
train_mods

xyplot(resamples(train_mods))
modelCor(resamples(train_mods))


