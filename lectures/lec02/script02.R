###################################
# Figure normal_density.pdf
pdf("img/normal_density.pdf", height=7.5, width=9)
SD <- c(0.25, 0.5, 1, 2)
xset <- seq(-4,4,by=0.001)
plot(0,0,col="white", xlim=range(xset), ylim=c(0,2), xlab="position", ylab="density")
for (i in 1:length(SD)) {
  lines(xset, dnorm(xset, sd=SD[i]))
}
text(0, dnorm(0, sd=SD[1])+0.07, expression(paste(sigma," = ", 0.25)))
text(0, dnorm(0, sd=SD[2])+0.07, expression(paste(sigma," = ", 0.5)))
text(0, dnorm(0, sd=SD[3])+0.07, expression(paste(sigma," = ", 1)))
text(0, dnorm(0, sd=SD[4])+0.07, expression(paste(sigma," = ", 2)))
dev.off()


###################################
# Simulation of simple regression
x <- seq(0,1,by=0.01)
n <- length(x)
beta <- 1
sigma <- 0.25

# get one sample of y and plot it with
# the ols estimator
y <- x * beta + rnorm(n,sd=sigma)
plot(x,y,pch=19)
betaHat <- sum(y*x) / sum(x^2)
abline(0,betaHat,col="red")

# run 1e3 samples
N <- 1000
betaHat <- rep(NA, N)
for (i in 1:N) {
  y <- x * beta + rnorm(n,sd=sigma)
  betaHat[i] <- sum(y*x) / sum(x^2)
}

# check that results hold for large N
hist(betaHat,breaks=100)
mean(betaHat) - beta
var(betaHat) - sigma^2 / sum(x^2)

# leverage
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
# Galton's pea
p <- read.csv("http://euler.stat.yale.edu/~tba3/stat612/lectures/lec02/data/galton_peas.csv")
p <- read.csv("~/files/stat612/lectures/lec02/data/galton_peas.csv")
table(p[,1], round(p[,2]), dnn=c("parent", "child"))

betaHat <- sum((p$child)*(p$parent)) / sum((p$parent)^2)

betaHat <- sum((p$parent - mean(p$parent))*(p$child - mean(p$child))) / sum((p$child - mean(p$child))^2)
alphaHat <- mean(p$parent) - mean(p$child) * betaHat

out <- lm(child ~ parent, data=p)
out

out <- lm(child ~ parent - 1, data=p)
out

# Galton's height data
h <- read.csv("http://euler.stat.yale.edu/~tba3/stat612/lectures/lec02/data/galton_heights.csv")
h <- read.csv("~/files/stat612/lectures/lec02/data/galton_heights.csv")

x <- h$Height
y <- (h$Father + h$Mother) / 2

sum( (y - mean(y))*(x - mean(x))) / sum((x - mean(x))^2)




