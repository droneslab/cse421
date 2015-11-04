####################
# Columbia images
library(jpeg)
pm <- read.csv("photoMetaData.csv", as.is=TRUE)

system(paste0("open columbiaImages/",pm$name[94]))
table(pm$category)

pm <- pm[pm$category != "artificial",]
pm <- pm[pm$category != "natural",]
n <- nrow(pm)

y <- rep(0, nrow(pm))
y[grep("indoor", pm$category)] <- 1

set.seed(1)
tFlag <- as.numeric(runif(n) > 0.5)

X <- matrix(0,nrow=n,ncol=3)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  X[j,] <- apply(img, 3, median)
  print(j)
}

y1 <- y[tFlag == 1]
y2 <- y[tFlag == 0]
X1 <- X[tFlag == 1,]
X2 <- X[tFlag == 0,]

betaHat <- qr.solve(crossprod(X1),crossprod(X1,y1))

y2Hat <- X2 %*% betaHat
table(y2Hat > 0.5, y2)

mean((y2Hat > 0.5) == y2)
mean(0 == y2)


X <- matrix(0,nrow=n,ncol=3)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  X[j,] <- apply(img, 3, median)
  print(j)
}



X <- matrix(rnorm(10*100),ncol=10)
P <- X %*% solve(t(X) %*% X) %*% t(X)
U <- svd(X)$u
U %*% t(U) - P

U <- svd(X,nu=100)$u
U %*% I %*% t(U) - P

beta <- runif(10)
eps <- rnorm(100)
y <- X %*% beta + eps

SVD <- svd(X)
Xplus <- SVD$v %*% diag(1/SVD$d) %*% t(SVD$u)

y2 <- X %*% (beta + Xplus %*% eps)
coef(lm(y ~ X - 1))
coef(lm(y2 ~ X - 1))







