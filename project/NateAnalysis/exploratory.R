## Explore Kaggle Don't Overfit II data
library(caret)
library(ggplot2)
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

plot(mu_c0, mu_c1)

## do a bunch of t-tests
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

ttests[[1]]
hist(tstats)
hist(pvals)
ks.test(x = pvals, y = "punif", min = 0, max = 1) ## it looks like the data are very close to just iid N(0,1)


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

counter <- 0
for(i in 1:(nrow(train_c1) - 1))
{ print(i)
  for(j in (i + 1):nrow(train_c1))
  {
    counter <- counter + 1
    d_c1[counter] <- sum( (train_c1[i,-c(1,2)] - train_c1[j,-c(1,2)])^2 )
  }
}
