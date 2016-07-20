# Load dataset for the problem set; calculate hours and delays
x <- readRDS("airline2007_pset04.Rds")
x$hour <- as.numeric(format(x$dTime + x$dOffset - x$depDelay, "%H", tz="GMT"))

x$late <- as.numeric(x$arrDelay > 60*60)

# Problem 1
outOls <- lm(late ~ dest - 1, data=x)
outLog <- glm(late ~ dest - 1, data=x)

predOls <- predict(outOls, type="response")
predLog <- predict(outLog, type="response")
max(abs(predOls - predLog))

# Problem 2
outOls <- lm(late ~ hour, data=x)
outLog <- glm(late ~ hour, data=x, family=binomial)

predOls <- predict(outOls, type="response")
predLog <- predict(outLog, type="response")
max(abs(predOls - predLog))

predHourOls <- tapply(predOls, x$hour, mean)
predHourLog <- tapply(predLog, x$hour, mean)

plot(0:23, predHourOls, type="l", col="red")
lines(0:23, predHourLog, type="l", col="blue")

# Problem 3
outLog <- glm(late ~ hour, data=x, family=binomial)
outPrb <- glm(late ~ hour, data=x, family=binomial(link="probit"))
outCll <- glm(late ~ hour, data=x, family=binomial(link="cloglog"))
outCau <- glm(late ~ hour, data=x, family=binomial(link="cauchit"))
outLLo <- glm(late ~ hour, data=x, family=binomial(link="log"))

predLog <- predict(outLog, type="response")
predPrb <- predict(outPrb, type="response")
predCll <- predict(outCll, type="response")
predCau <- predict(outCau, type="response")
predLLo <- predict(outLLo, type="response")

predHourLog <- tapply(predLog, x$hour, mean)
predHourPrb <- tapply(predPrb, x$hour, mean)
predHourCll <- tapply(predCll, x$hour, mean)
predHourCau <- tapply(predCau, x$hour, mean)
predHourLLo <- tapply(predLLo, x$hour, mean)

plot(0:23, predHourLog, type="l", col="red")
lines(0:23, predHourPrb, type="l", col="blue")
lines(0:23, predHourCll, type="l", col="green")
lines(0:23, predHourCau, type="l", col="salmon")
lines(0:23, predHourLLo, type="l", col="black")

# Problem 4
outOls <-  lm(veryLate ~ hour + schedTotTime + dest, data=x, subset=(group == "I"))
outLog <- glm(veryLate ~ hour + schedTotTime + dest, data=x, subset=(group == "I"), family=binomial)

predOls <- predict(outOls, newdata=x[x$group == "II",], type="response")
predLog <- predict(outLog, newdata=x[x$group == "II",], type="response")

res <- x[x$group == "II","late"]
mean( (predOls - res)^2 )
mean( (predLog - res)^2 )

qOls <- quantile(predOls, seq(0,1,0.01))
qLog <- quantile(predLog, seq(0,1,0.01))

cOls <- cut(predOls, qOls, labels=FALSE, include.lowest=TRUE)
cLog <- cut(predLog, qLog, labels=FALSE, include.lowest=TRUE)

tabOls <- tapply(res, cOls, mean)
tabLog <- tapply(res, cLog, mean)

# Problem 5
n <- 1000
p <- 100
beta <- rnorm(p)
X <- matrix(runif(n*p), ncol=p)

# Mean squared error
N <- 1000
res <- matrix(NA,nrow=N,ncol=6)
for (i in 1:N) {
  y <- X %*% beta + rnorm(n,sd=5)
  betaHat <- coef(lm(y ~ X - 1))
  betaTilde9 <- betaHat * 0.9
  betaTilde8 <- betaHat * 0.8
  betaTilde5 <- betaHat * 0.5
  betaTilde2 <- betaHat * 0.2
  betaTilde1 <- betaHat * 0.1
  res[i,1] <- sum((betaHat - beta)^2)
  res[i,2] <- sum((betaTilde9 - beta)^2)
  res[i,3] <- sum((betaTilde8 - beta)^2)
  res[i,4] <- sum((betaTilde5 - beta)^2)
  res[i,5] <- sum((betaTilde2 - beta)^2)
  res[i,6] <- sum((betaTilde1 - beta)^2)
}
apply(res,2,mean)

# Problem 6

N <- 1000
res <- matrix(NA,nrow=N,ncol=6)
for (i in 1:N) {
  y <- X %*% beta + rcauchy(n)
  betaHat <- coef(lm(y ~ X - 1))
  betaTilde9 <- betaHat * 0.9
  betaTilde8 <- betaHat * 0.8
  betaTilde5 <- betaHat * 0.5
  betaTilde2 <- betaHat * 0.2
  betaTilde1 <- betaHat * 0.1
  res[i,1] <- sum((betaHat - beta)^2)
  res[i,2] <- sum((betaTilde9 - beta)^2)
  res[i,3] <- sum((betaTilde8 - beta)^2)
  res[i,4] <- sum((betaTilde5 - beta)^2)
  res[i,5] <- sum((betaTilde2 - beta)^2)
  res[i,6] <- sum((betaTilde1 - beta)^2)
}
apply(res,2,mean)

n <- 1000
p <- 10
beta <- rnorm(p)
X <- matrix(runif(n*p), ncol=p)

N <- 1000
res <- matrix(NA,nrow=N,ncol=6)
for (i in 1:N) {
  y <- X %*% beta + rnorm(n)
  betaHat <- coef(lm(y ~ X - 1))
  betaTilde9 <- betaHat * 0.9
  betaTilde8 <- betaHat * 0.8
  betaTilde5 <- betaHat * 0.5
  betaTilde2 <- betaHat * 0.2
  betaTilde1 <- betaHat * 0.1
  res[i,1] <- sum((betaHat - beta)^2)
  res[i,2] <- sum((betaTilde9 - beta)^2)
  res[i,3] <- sum((betaTilde8 - beta)^2)
  res[i,4] <- sum((betaTilde5 - beta)^2)
  res[i,5] <- sum((betaTilde2 - beta)^2)
  res[i,6] <- sum((betaTilde1 - beta)^2)
}
apply(res,2,mean)

n <- 1000
p <- 10
beta <- rnorm(p)
X <- matrix(runif(n*p), ncol=p)

N <- 1000
res <- matrix(NA,nrow=N,ncol=6)
for (i in 1:N) {
  y <- X %*% beta + rnorm(n)
  betaHat <- coef(lm(y ~ X - 1))
  betaTilde9 <- betaHat * 0.9
  betaTilde8 <- betaHat * 0.8
  betaTilde5 <- betaHat * 0.5
  betaTilde2 <- betaHat * 0.2
  betaTilde1 <- betaHat * 0.1
  res[i,1] <- sum((betaHat - beta)^2)
  res[i,2] <- sum((betaTilde9 - beta)^2)
  res[i,3] <- sum((betaTilde8 - beta)^2)
  res[i,4] <- sum((betaTilde5 - beta)^2)
  res[i,5] <- sum((betaTilde2 - beta)^2)
  res[i,6] <- sum((betaTilde1 - beta)^2)
}
apply(res,2,mean)


