X  <- matrix(c(10^9, -1, -1, 10^(-5)), 2, 2)
beta <- c(1,1)
y <- X %*% beta

XtX <- t(X) %*% X
Xty <- t(X) %*% y
XtXinv <- solve(XtX, tol=0)

betaHat <- qr.solve(XtX, Xty, tol=0)

options(digits=22)
n <- 1000
p <- 25
alpha <- 1e-5
X <- matrix(runif(n*p), ncol=p)
X <- alpha * X + matrix(rnorm(n), nrow=n, ncol=p)
beta <- runif(p)
y <- X %*% beta + rnorm(n,sd=0.5)

XtX <- t(X) %*% X
Xty <- t(X) %*% y

U <- chol(XtX)
betaChol <- backsolve(U, forwardsolve(t(U),Xty))

QR <- qr(X)
Q <- qr.Q(QR)
R <- qr.R(QR)
dim(Q)
dim(R)

betaQR <- backsolve(R, t(Q) %*% y)

sqrt(sum(abs(betaQR - betaChol)^2)) / mean(abs(betaQR))
sqrt(sum(abs(X %*% betaQR - X%*% betaChol)^2))


betaHat <- qr.solve(t(X) %*% X, t(X) %*% y, tol=0)
coef(lm(y ~ X-1)) - betaHat



