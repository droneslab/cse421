# Code solutions from Problem Set #03 - There is not
#   a lot of commentary here, but hopefully enough to
#   fill in any gaps left after Wed's lecture. If you
#   have any further questions please let Jason or I
#   know.


### Part I - General linear model

## 2. "Download the cleaned ..."
x <- readRDS("airline2007_pset03.Rds")

groupI <- x[x$group == "I",]
groupII <- x[x$group == "II",]
groupIII <- x[x$group == "III",]

# Calculate form of V:
outI <- lm(arrDelay ~ depDelay + dest - 1, data = groupI)
varTable <- tapply(outI$resid, groupI$dest, var)
index <- match(groupII$dest, names(varTable))
weights <- 1/sqrt(as.numeric(varTable)[index])

# Compute betaOLS and betaGLS
ols <- lm(arrDelay ~ depDelay + dest - 1, data = groupII)
gls <- lm(arrDelay ~ depDelay + dest - 1, data = groupII, weights=weights)

ols$coefficients - gls$coefficients

# To do prediction, need variance of the new terms
index <- match(groupIII$dest, names(varTable))
predVar <- as.numeric(varTable)[index]

predOLS <- predict(ols, newdata=groupIII, interval="prediction")
predGLS <- predict(gls, newdata=groupIII, interval="prediction", pred.var=predVar)

res <- groupIII$arrDelay
mean(predOLS[,2] < res & res < predOLS[,3])
mean(predGLS[,2] < res & res < predGLS[,3])

sort(tapply(predOLS[,2] < res & res < predOLS[,3], groupIII$dest, mean))
sort(tapply(predGLS[,2] < res & res < predGLS[,3], groupIII$dest, mean))

# 3. "Take all of the flights..."
x <- readRDS("airline2007_pset03.Rds")
date <- format(x$aTime + x$aOffset, "%Y-%m-%d", tz="GMT")
x$time <- as.numeric(x$aTime + x$aOffset)

w <- x[date == "2007-01-11" & x$dest == "ORD",]
w <- w[order(w$time),]

out <- lm(arrDelay ~ depDelay, data=w)
plot(w$time, resid(out))

res <- resid(out)
sampleCov <- mean(res[-1] * res[-length(res)])
phiHat <- -1 * log(sampleCov / summary(out)$sigma^2)
phiHat

w <- x[date == "2007-02-13" & x$dest == "ORD",]
w <- w[order(w$time),]

mf <- model.frame(arrDelay ~ depDelay, data=w)
mr <- model.response(mf)
mm <- model.matrix(mf, data=w)

V <- matrix(0, ncol=nrow(w), nrow=nrow(w))
V <- exp(- phiHat * abs(row(V) - col(V)))
C <- chol(V)

df <- data.frame(y=C %*% mr, C%*% mm)
names(df) <- c("arrDelay", "inter", "depDelay")

ols <- lm(arrDelay ~ depDelay, data = w)
gls <- lm(arrDelay ~ . - 1, data=df)

ols$coefficients - gls$coefficients

w$inter <- 1
predOLS <- predict(ols, newdata=w, interval="prediction")
predGLS <- predict(gls, newdata=w, interval="prediction")

res <- w$arrDelay
mean(predOLS[,2] < res & res < predOLS[,3])
mean(predGLS[,2] < res & res < predGLS[,3])

plot(predOLS[,2] < res & res < predOLS[,3])
plot(predGLS[,2] < res & res < predGLS[,3])

mean(predOLS[,3] - predOLS[,2])


############################################################
### Part II - Breaking classical linear model assumptions

## Normality
n <- 1000
p <- 5
X <- matrix(rnorm(n*p), ncol=p)
beta <- c(1,1,1,0,0)


funs <- list(normal = rnorm,
             runif = function(n) runif(n, -2, 2),
             t=function(n) rt(n, 2),
             discrete = function(n) sample(c(-1,1),n, replace=TRUE),
             cauchy = rcauchy)

N <- 10000
results <- rep(NA, N)
for (j in 1:length(funs)) {
  for (i in 1:N) {
    y <- X %*% beta + rnorm(n)
    ci <- confint(lm(y ~ X - 1))[1,]
    results[i] <- ci[1] < 1 & ci[2] > 1
  }
  cat(names(funs)[j], ": ", mean(results), "\n", sep="")
}

## Spherical errors
n <- 1000
p <- 5
X <- matrix(rnorm(n*p), ncol=p)
beta <- c(1,1,1,0,0)

N <- 2500
for (phi in c(0.5,0.9,1)) {
  results <- rep(NA, N)
  for (i in 1:N) {
    delta <- rnorm(n)
    epsilon <- rep(0, n)
    for (j in 2:n) {
      epsilon[j] <- phi * epsilon[j-1] + delta[j]
    }
    y <- X %*% beta + epsilon
    ci <- confint(lm(y ~ X - 1))[1,]
    results[i] <- ci[1] < 1 & ci[2] > 1
  }
  cat("phi:", phi, " sucess rate: ", mean(results), "\n", sep="")
}

## Exogeneity
n <- 1000

N <- 2500
for (beta in c(0.5,0.9,1)) {
  results <- rep(NA, N)
  for (i in 1:N) {
    epsilson <- rnorm(n)
    y <- rep(0, n)
    for (j in 2:n) {
      y[j + 1] <- y[j]*beta + epsilson[j]
    }
    ci <- confint(lm(y[-1] ~ y[-n] - 1))[1,]
    results[i] <- ci[1] < beta & ci[2] > beta
  }
  cat("beta:", beta, " sucess rate: ", mean(results), "\n", sep="")
}






