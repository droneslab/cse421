###################################
# Simple linear regression - leverage
x <- seq(0,1,by=0.01)
n <- length(x)
beta <- 1
sigma <- 0.25

x[n] <- 10
y <- x * beta + rnorm(n,sd=sigma)

plot(x,y,cex=0.3)
betaHat <- sum(y*x) / sum(x^2)
abline(0,betaHat,col="red")

# leverage with corruption
x[n] <- 10
y <- x * beta + rnorm(n,sd=sigma)
k <- n
y[k] <- y[k] - 1

plot(x,y,cex=0.3)
betaHat <- sum(y*x) / sum(x^2)
abline(0,betaHat,col="red")
betaHatMinus <- sum(y[-k]*x[-k]) / sum(x[-k]^2)
abline(0,betaHatMinus,col="blue")

# calc leverage measure of each point
x[n] <- 10
y <- x * beta + rnorm(n,sd=sigma)

results <- rep(NA, n)
for (k in 1:n) {
  ynew <- y
  ynew[k] <- ynew[k] - 1

  betaHat <- sum(ynew*x) / sum(x^2)
  betaHatMinus <- sum(ynew[-k]*x[-k]) / sum(x[-k]^2)
  results[k] <- betaHatMinus - betaHat
}

plot(results)
plot(x,results)

###################################
# Galton's height data
urlBase <- "http://euler.stat.yale.edu/~tba3/stat612/lectures/lec02/data/"
h <- read.csv(paste0(urlBase, "galton_heights.csv"))




