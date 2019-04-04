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

hist(mu_c0) ## looks like one mean is suspiciously far to the left? 
which.min(mu_c0)
hist(train_c0$`33`)

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
t.test(x = mu_c0[-34] / sqrt(1/nrow(train_c0)), y = mu_c1 / sqrt(1/nrow(train_c1)), mu = 0, var.equal = TRUE) 
t.test(x = mu_c0[-34] / sqrt(1/nrow(train_c0)), y = mu_c1[-34] / sqrt(1/nrow(train_c1)), mu = 0, var.equal = TRUE, paired = TRUE) 

## there are a couple differences between means that exceed normal range
hist(mu_c0 - mu_c1)
which.min(mu_c0[-34] - mu_c1[-34]) ## one of them is for "33" but the other is for "65"
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
library(mvtnorm)
lrstat0.1 <- -2 * (sum( apply(X = train_c0[,-c(1,2, 34 + 2)], MARGIN = c(1,2), FUN = dnorm, log = TRUE, mean = mean(mu_c0[-34]), sd = 1 ) ) - 
                    sum( apply(X = train_c0[,-c(1,2, 34 + 2)] - matrix(rep(x = mu_c0[-34], times = nrow(train_c0)), nrow = nrow(train_c0), byrow = TRUE), MARGIN = 2, FUN = dmvnorm, sigma = diag(nrow(train_c0)), log = TRUE) )) 
lrstat0.1
1 - pchisq(q = lrstat0.1, df = 299 - 1)

lrstat1.1 <- -2 * (sum( apply(X = train_c1[,-c(1,2)], MARGIN = c(1,2), FUN = dnorm, log = TRUE, mean = mean(mu_c1), sd = 1 ) ) - 
                     sum( apply(X = train_c1[,-c(1,2)] - matrix(rep(x = mu_c1, times = nrow(train_c1)), nrow = nrow(train_c1), byrow = TRUE), MARGIN = 2, FUN = dmvnorm, sigma = diag(nrow(train_c1)), log = TRUE) )) 
lrstat1.1
1 - pchisq(q = lrstat1.1, df = 300 - 1)
