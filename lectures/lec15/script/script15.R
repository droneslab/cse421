####################
# Columbia images
library(jpeg)
img <- readJPEG("columbiaImages/DSC_1785.jpg")
system("open columbiaImages/DSC_1785.jpg")

X <- matrix(img,ncol=3)
cor(X)

index <- sample(nrow(X),5e4)
par(mfrow=c(1,2))
plot(X[index,1], X[index,2], pch=19, cex=0.5,
      col="#ff00ff08",
      xlab="red", ylab="green")
plot(X[index,3], X[index,2], pch=19, cex=0.5,
      col="#ff00ff08",
      xlab="blue", ylab="green")

y <- as.numeric(row(img[,,1]))
y <- (y - mean(y)) / sd(y)

summary(lm(y ~ X - 1))

library(MASS)
lm.ridge(y ~ X - 1, lambda=10)
lm.ridge(y ~ X - 1, lambda=100)

##################
SVD <- svd(X)
SVD$v
T <- SVD$u %*% diag(SVD$d)

# Notice, components are uncorrelated
t(T) %*% T

# Plot first two components
index <- sample(nrow(X),5e4)
plot(T[index,1],T[index,2], xlab="PC1", ylab="PC2")

# Fit PCR for k equal to 1, 2, and 3
trainSet <- (runif(nrow(T)) > 0.99)
m1 <- lm(y ~ T[,1] - 1)$coef
m2 <- lm(y ~ T[,1:2] - 1)$coef
m3 <- lm(y ~ T[,1:3] - 1)$coef

# notice the relationship between the coefs; why?
m1
m2
m3

# Now, refit just on the training set
trainSet <- (runif(nrow(T)) > 0.99)
m1 <- lm(y ~ T[,1] - 1, subset=trainSet)$coef
m2 <- lm(y ~ T[,1:2] - 1, subset=trainSet)$coef
m3 <- lm(y ~ T[,1:3] - 1, subset=trainSet)$coef

# Fit data on the testing set
pred1 <- T[!trainSet,1,drop=FALSE] %*% m1
pred2 <- T[!trainSet,1:2,drop=FALSE] %*% m2
pred3 <- T[!trainSet,1:3,drop=FALSE] %*% m3

# Notice how much variance is decreased in PCR
sd(pred1)
sd(pred2)
sd(pred3)

# How do the predictions stack up?
mean((pred1 - y[!trainSet])^2)
mean((pred2 - y[!trainSet])^2)
mean((pred3 - y[!trainSet])^2)




