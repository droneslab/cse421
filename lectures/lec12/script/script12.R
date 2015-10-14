# Simulate a probit model
n <- 1000
p <- 10
beta <- rnorm(p)
X <- matrix(runif(n*p),ncol=p)

z <- X %*% beta + rnorm(n,sd=1)
y <- as.numeric(z > 0)

# ?
lm(y ~ X - 1)
glm(y ~ X - 1)

out <- glm(y ~ X - 1, family=binomial(link="probit"))
out

plot(beta, coef(out))
abline(0,1, col="red")

plot(out$fitted.values, out$residuals)
plot(out$fitted.values, out$residuals, col=(y == 1) + 1)
plot(out$fitted.values, y-out$fitted.values, col=(y == 1) + 1)

pred <- predict(out)
range(pred)
plot(pred, z)
abline(0,1, col="red", lwd=2)
cor(pred, z)

pred <- predict(out, type="response")
range(pred)
hist(pred,breaks=20)

# Confusion matrix & error rate
tab <- table(pred > 0.5, y)
sum(diag(tab)) / sum(tab)

# sensetivity/specificity
alpha <- 0.2
predClass <- as.numeric(pred >= alpha)
sens <- sum(predClass == 1 & y == 1) / sum(y == 1)
spec <- sum(predClass == 0 & y == 0) / sum(y == 0)

sens
spec

# a curve
alpha <- quantile(pred, seq(0,1,by=0.01))
N <- length(alpha)

sens <- rep(NA,N)
spec <- rep(NA,N)
for (i in 1:N) {
  predClass <- as.numeric(pred >= alpha[i])
  sens[i] <- sum(predClass == 1 & y == 1) / sum(y == 1)
  spec[i] <- sum(predClass == 0 & y == 0) / sum(y == 0)
}

plot(spec, sens, xlab="specificity", ylab="sensitivity",
     type="l")

# ROC curve
plot(1- spec, sens, xlab="false positive rate", ylab="true positive rate", type="l")







