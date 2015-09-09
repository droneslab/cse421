###################################
# Simple linear regression - leverage
options(width=4)
set.seed(3)
x <- seq(0,1,by=0.01)
n <- length(x)
beta <- 1
sigma <- 0.25

x[n] <- 10
y <- x * beta + rnorm(n,sd=sigma)

plot(x,y,cex=0.3)
betaHat <- sum(y*x) / sum(x^2)
abline(0,betaHat,col="red")

# prediction at the high leverage point
abs(betaHat*x[n] - y[n])
mean(abs(betaHat*x[-n] - y[-n]))

# leverage with corruption
x <- seq(0,1,by=0.01)
x[n] <- 10
y <- x * beta + rnorm(n,sd=sigma)
y[n] <- 9

plot(x,y,cex=0.3,ylim=c(-1,10.5))
abline(0,1)
1 /(1 - pnorm(4))

betaHat <- sum(y*x) / sum(x^2)
abline(0,betaHat,col="red")

betaHatMinus <- sum(y[-n]*x[-n]) / sum(x[-n]^2)
abline(0,betaHatMinus,col="blue")

# simulation over many iterations
N <- 10000
betaHat <- rep(NA, N)
betaHatMinus <- rep(NA, N)
x <- seq(0,1,by=0.01)
x[n] <- 10

for (j in 1:N) {
  y <- x * beta + rnorm(n,sd=sigma)
  y[n] <- x[n] - 1
  betaHat[j] <- sum(y*x) / sum(x^2)
  betaHatMinus[j] <- sum(y[-n]*x[-n]) / sum(x[-n]^2)
}

par(mfrow=c(1,2))
hist(betaHat)
abline(v=median(betaHat), col="red", lwd=3)
hist(betaHatMinus)
abline(v=median(betaHatMinus), col="red", lwd=3)

# repeat for a different point
N <- 100
betaHat <- rep(NA, N)
betaHatMinus <- rep(NA, N)
x <- seq(0,1,by=0.01)

for (j in 1:N) {
  y <- x * beta + rnorm(n,sd=sigma)
  y[n] <- y[n] - 1
  betaHat[j] <- sum(y*x) / sum(x^2)
  betaHatMinus[j] <- sum(y[-n]*x[-n]) / sum(x[-n]^2)
}

par(mfrow=c(1,2))
hist(betaHat)
abline(v=median(betaHat), col="red", lwd=3)
hist(betaHatMinus)
abline(v=median(betaHatMinus), col="red", lwd=3)

# calc leverage measure of each point
set.seed(2)
x <- seq(0,1,by=0.01)
x[n] <- 10
y <- x * beta + rnorm(n,sd=sigma)

results <- rep(NA, n)
for (k in 1:n) {
  ynew <- y
  ynew[k] <- x[k] - 1

  betaHat <- sum(ynew*x) / sum(x^2)
  betaHatMinus <- sum(ynew[-k]*x[-k]) / sum(x[-k]^2)
  results[k] <- betaHatMinus - betaHat
}

plot(results)
plot(x,results)

###################################
# Hypothesis tests
set.seed(1)
x <- seq(0.05,1,by=0.05)
n <- length(x)
beta <- 0.1
sigma <- 0.2

y <- x * beta + rnorm(n,sd=sigma)
plot(x,y)

betaHat <- sum(y*x) / sum(x^2)

# z test
b <- 0
zTest <- (betaHat - b) /
           sqrt(sigma^2 / sum(x^2))
zTestP <- (1 - pnorm(zTest))*2
zTest
zTestP

(1 - pnorm(1.96))*2 # just a test

# t test
b <- 0
s <- sqrt(sum((y - x*betaHat)^2) / (n-1))
se <- sqrt(s^2 / sum(x^2))
tTest <- (betaHat - b) / se
tTestP <- (1 - pt(tTest, df=n-1))*2
tTest
tTestP

# f test
fTest <- tTest^2
fTestP <- (1 - pf(fTest, df1=1, df2=n-1))
fTestP
fTest

# check them with lm()
out <- lm(y ~ x - 1)
summary(out)

betaHat
se
s

tTest
tTestP

fTest
fTestP

# simulate confidence intervals
N <- 10000
ci <- matrix(NA, ncol=2, nrow=N)
t_alpha <- qt(1-0.01/2, df=n-1)

for (j in 1:N) {
  y <- x * beta + rnorm(n,sd=sigma)
  betaHat <- sum(y*x) / sum(x^2)
  s <- sqrt(sum((y - x*betaHat)^2) / (n-1))
  se <- sqrt(s^2 / sum(x^2))
  ci[j,1] <- betaHat - t_alpha * se
  ci[j,2] <- betaHat + t_alpha * se
}

mean(ci[,1] <= beta & beta <= ci[,2])

###################################
# Hypothesis tests; with intercept
set.seed(1)
x <- seq(0.05,1,by=0.05)
n <- length(x)
alpha <- 0.05
beta <- 0.1
sigma <- 0.1

y <- alpha + x * beta + rnorm(n,sd=sigma)

out <- lm(y ~ x)
summary(out)

confint(out, level=0.9)


###################################
# Galton's height data
urlBase <- "http://euler.stat.yale.edu/~tba3/stat612/lectures/lec03/data/"
h <- read.csv(paste0(urlBase, "galton_heights.csv"))
head(h)

summary(lm(Height ~ Father - 1, data=h))

summary(lm(h$Height ~ h$Father - 1))



summary(lm(Height ~ Father - 1, data=h,
          subset=(Gender == "M")))

mean(h$Father)
mean(h$Mother)
mean(h$Height[h$Gender == "M"])
mean(h$Height[h$Gender == "F"])

mean(h$Father) / mean(h$Mother)
mean(h$Height[h$Gender == "M"]) /
  mean(h$Height[h$Gender == "F"])

h$Height2 <- h$Height
h$Height2[h$Gender == "F"] <-
  h$Height2[h$Gender == "F"] * 1.08
h$MidHeight <- (h$Father + h$Mother*1.08) / 2

summary(lm(Height2 ~ MidHeight - 1, data=h))
summary(lm(Height2 ~ MidHeight, data=h))


plot(h$MidHeight, h$Height2, pch=19, cex=0.3,
      xlab="Parents Avg. Height",
      ylab="Children's Height")
abline(lm(Height2 ~ MidHeight, data=h),
        col="red", lwd=2)

summary(lm(MidHeight ~ Height2, data=h))
abline(-44.47115/0.35750, 1/0.35750,
         col="red", lwd=2)

summary(lm(Height2 ~ Father, data=h))
summary(lm(Father ~ Height2, data=h))








