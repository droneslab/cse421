#### Lasso
set.seed(1)
n <- 1000
p <- 5
X <- poly(seq(0,1,length.out=n),degree=p)
t(X) %*% X
beta <- c(1,0,1,0,0)
y <- X %*% beta + rnorm(n,sd=0.3)

Xty <- t(X) %*% y

lambda <- seq(0,max(abs(Xty))*2,length.out=1e5)

j <- 1
beta <- Xty[j] - lambda * sign(Xty[j])
beta[lambda > abs(Xty[j])] <- 0

pdf("img/fig03.pdf", height=5, width=8)
plot(lambda, beta, type='l', xlab="lambda", ylab="beta_1")
dev.off()

beta <- matrix(0,nrow=length(lambda),ncol=p)
for (j in 1:p) {
  beta[,j] <- Xty[j] - lambda * sign(Xty[j])
  beta[lambda > abs(Xty[j]),j] <- 0
}

pdf("img/fig04.pdf", height=5, width=8)
plot(0,0,col="white",xlim=range(lambda),
      ylim=range(beta)*1.2, xlab="lambda", ylab="beta")
for (j in 1:p) lines(lambda, beta[,j])
text(-0.02,Xty,1:5)
dev.off()


pdf("img/fig05.pdf", height=5, width=8)
plot(0,0,col="white",xlim=range(lambda),
      ylim=range(beta)*1.2, xlab="lambda", ylab="beta")
for (j in 1:p) lines(lambda, beta[,j])
text(-0.02,Xty,1:5)
abline(v=0.5,col="blue",lty="dashed")
this <- which.min(abs(lambda - 0.5))
points(lambda[this], beta[this,1], pch=19, cex=1, col="blue")
points(lambda[this], beta[this,3], pch=19, cex=1, col="blue")
dev.off()

library(lars)
out <- lars(X,y,normalize=FALSE,intercept=FALSE)
out

pdf("img/fig06.pdf", height=5, width=8)
plot(out)
dev.off()


out <- lars(X,y,normalize=FALSE,intercept=FALSE,trace=TRUE)

n <- 1000
p <- 25
X <- matrix(rnorm(p*n),ncol=p)
X <- X*0.8 + X[,1]*0.2
t(X) %*% X
beta <- sample(0:1,p,replace=TRUE,prob=c(9,1))
y <- X %*% beta + rnorm(n,sd=0.3)

library(lars)
out <- lars(X,y,normalize=FALSE,intercept=FALSE,trace=TRUE)

pdf("img/fig07.pdf", height=5, width=8)
plot(out)
dev.off()



