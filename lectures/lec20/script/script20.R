library(glmnet)

# Independent design matrix (dense model)
set.seed(1)
d <- 25
bpoints <- c(0,sort(runif(d-2)),1)
bvals <- cumsum(rnorm(d))
f <- approxfun(bpoints, bvals)
plot(f)

n <- 10000
x <- sort(runif(n))
eps <- rnorm(n,sd=0.75)
y <- f(x) + eps

plot(f,ylim=range(y))
points(x,y)
lines(x,f(x),col="red")

X <- poly(x,degree=10)
round(t(X) %*% X,4)

coff <- log(2*max(abs(t(X) %*% eps)) / sqrt(n))
out <- glmnet(X,y)

plot(out, xvar="lambda")
abline(v=coff,lty="dashed",col='red')

# Independent design matrix (sparse model)
set.seed(1)
d <- 25
s <- 5
n <- 1000
x <- sort(runif(n))
X <- poly(x,degree=d)
beta <- rep(0, d)
beta[1:s] <- runif(s) * 20
beta

eps <- rnorm(n,sd=0.1)
y <- X %*% beta + eps
plot(x,y)

coff <- log(2*max(abs(t(X) %*% eps)) / sqrt(n))
out <- glmnet(X,y)

plot(out, xvar="lambda")
abline(v=coff,lty="dashed",col='red')

# Empirical process part
nvals <- round(exp(seq(log(1e2),log(1e5),length.out=25)))

k <- 200
output <- vector("list", length(nvals))
for (i in 1:length(nvals)) {
  z <- matrix(rnorm(nvals[i]*k),ncol=k)
  output[[i]] <- apply(abs(z), 2, max)
}

dat <- cbind(rep(nvals, each=k),unlist(output))

plot(dat[,1], dat[,2], pch=19, cex=0.5, xlab="sample size", ylab="max(|z|)")
points(nvals, sapply(output,mean), cex=3, pch=4L,col="red")
lines(dat[,1], sqrt(2 * log(2*dat[,1])), col="blue", lwd=2)













