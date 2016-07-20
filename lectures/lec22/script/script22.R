library(genlasso)
library(glmgen)
library(glmnet)

##########
n <- 1000
s <- 5
mu <- rep(0,n+1)
mu[breaks <- sort(sample(1:n, s-1))] <- rnorm(s-1,sd=(s-1):1)
mu <- cumsum(mu) / (s-1)

y <- rnorm(n, mean=mu, sd=0.09)

X <- matrix(0, ncol=n, nrow=n)
X[row(X) >= col(X)] <- 1

out <- glmnet(X, y, standardize=FALSE, intercept=FALSE)



##########
n <- 1000
s <- 10
breaks <- c(0,sort(sample(1:n, s-1)),n+1)
mu <- c(0,rnorm(s-1,sd=1),0)
approxfun(breaks,mu)

mu <- approxfun(breaks,mu)(1:n)
y <- rnorm(n, mean=mu, sd=0.09)

X <- matrix(0, ncol=n, nrow=n)
X <- row(X) - col(X) + 1
X[X < 0] <- 0

out <- glmnet(X, y, standardize=FALSE, intercept=FALSE)
lines(cumsum(cumsum(out$beta[,60])))


##########
n <- 1000
s <- 40

X <- rnorm(n)
y <- 2 + 4 * X + rnorm(n, sd=2)
index <- sample(1:n,s)
y[index] <- mean(y) + rnorm(s, sd=15)
plot(X, y)

Z <- cbind(X, diag(n))
out <- glmnet(Z, y, standardize=FALSE, intercept=FALSE, lambda.min.ratio=0)
these <- (which(out$beta[,40] != 0)[-1] - 1)
points(X[these],y[these],col="red",pch=19,cex=0.7)
2


###########
library(genlasso)
set.seed(3)
n <- 1000
s <- 10
breaks <- c(0,sort(sample(1:n, s-1)),n+1)
mu <- c(0,rnorm(s-1,sd=1),0)
approxfun(breaks,mu)

mu <- approxfun(breaks,mu)(1:n)
y <- rnorm(n, mean=mu, sd=0.12)
out <- trendfilter(y,ord=2)
pred <- predict(out)$fit


system("rm ani/*")
jpeg("ani/foo%04d.jpg", 1920, 1080, quality=100)

par(mar=c(2,2,10,5))
for(i in 1:ncol(pred)) {
  plot(y, pch=19, cex=0.5, col=rgb(0,0,0,0.5), cex.axis=3, xlab="", ylab="", mgp = c(3, 3, 0),
       lwd=4, axes=FALSE)
  box()
  title(main=sprintf("df=%03d",out$df[i]), cex.main=10)
  lines(pred[,i], col="red", lwd=3)
}

dev.off()


system("rm trendfilter_1.mp4")
system("ffmpeg -r 24 -i ani/foo%04d.jpg -r 24 trendfilter_1.mp4")

system("rm trendfilter_2.mp4")
system("ffmpeg -r 24 -i ani/foo%04d.jpg -r 24 trendfilter_2.mp4")











