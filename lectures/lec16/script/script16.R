### Fixed PCA example:

# Load data
library(jpeg)
img <- readJPEG("columbiaImages/DSC_1785.jpg")
system("open columbiaImages/DSC_1785.jpg")

X <- matrix(img,ncol=3)
y <- as.numeric(row(img[,,1]))
y <- (y - mean(y)) / sd(y)

# PCA
SVD <- svd(X)
SVD$v
T <- SVD$u %*% diag(SVD$d)

# FIXED: I had cor() before, which is not diagonal
#   because columns do not have zero mean
round(t(T) %*% T,6)

# Fit PCR for k equal to 1, 2, and 3
m1 <- lm(y ~ T[,1] - 1)$coef
m2 <- lm(y ~ T[,1:2] - 1)$coef
m3 <- lm(y ~ T[,1:3] - 1)$coef

# FIXED: I had previously decomposed on all of X but
#   fit the data only on training set;
# notice the relationship between the coefs; why?
m1
m2
m3

# Now, refit just on the training set
trainSet <- (runif(nrow(T)) > 0.99)
m1 <- lm(y ~ T[,1] - 1, subset=trainSet)$coef
m2 <- lm(y ~ T[,1:2] - 1, subset=trainSet)$coef
m3 <- lm(y ~ T[,1:3] - 1, subset=trainSet)$coef

# *these* are not the same
m1
m2
m3

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

#####################
n <- 1000
p <- 10
X <- matrix(rnorm(n*p),ncol=p)
beta <- runif(p)
y <- as.numeric(X%*%beta + rnorm(n) > 0)

# Fit a model; what do you think the iter is?
out <- glm(y ~ X - 1, family=binomial())
out$iter

# Show that the residuals are orthogonal to col space of X
t(X) %*% (y - predict(out, type="response"))

# Fit GLM IRWLS
beta <- rep(0,p)

# Manually iterate this:
prob <- 1 / (1 + exp(-X %*% beta))
D <- diag(as.numeric(prob*(1-prob)))
z <- X %*% beta + (y - prob) / diag(D)
beta <- qr.solve(crossprod(X, D %*% X), crossprod(X, D %*% z))
beta

coef(out) - beta

# Notice how quickly this converges...
beta <- rep(0,p)
for (k in 1:10) {
  prob <- 1 / (1 + exp(-X %*% beta))
  D <- diag(as.numeric(prob*(1-prob)))
  z <- X %*% beta + (y - prob) / diag(D)
  beta <- qr.solve(crossprod(X, D %*% X), crossprod(X, D %*% z))
  print(sum((beta - coef(out))^2))
}


##################### Photograph examples
library(jpeg)
pm <- read.csv("photoMetaData.csv")
pm <- pm[pm$category %in% c("artificial","natural"),]
n <- nrow(pm)

y <- as.numeric(pm$category == "natural")

X <- matrix(NA, ncol=3, nrow=n)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  X[j,] <- apply(img,3,mean)
  print(sprintf("%03d / %03d", j, n))
}

out <- glm(y ~ X, family=binomial)
summary(out)

pred <- predict(out, type="response")
y[order(pred)]

### ROC curve (see lecture 12)
roc <- function(y, pred) {
  alpha <- quantile(pred, seq(0,1,by=0.01))
  N <- length(alpha)

  sens <- rep(NA,N)
  spec <- rep(NA,N)
  for (i in 1:N) {
    predClass <- as.numeric(pred >= alpha[i])
    sens[i] <- sum(predClass == 1 & y == 1) / sum(y == 1)
    spec[i] <- sum(predClass == 0 & y == 0) / sum(y == 0)
  }
  return(list(fpr=1- spec, tpr=sens))
}

r <- roc(y, pred)
plot(r$fpr, r$tpr, xlab="false positive rate", ylab="true positive rate", type="l")
abline(0,1,lty="dashed")

auc <- function(r) {
  sum((r$fpr) * diff(c(0,r$tpr)))
}

auc(r)

### A bit more grainularity
img <- readJPEG(paste0("columbiaImages/",pm$name[100]))
system(paste0("open columbiaImages/",pm$name[100]))

hist(img[,,1],breaks=seq(0,1,by=0.05))

# You may have seen something like this on a fancy digital camera
hist(img[,,1],breaks=seq(0,1,by=0.05), col="#ff000099",border=NA)
hist(img[,,2],breaks=seq(0,1,by=0.05), col="#00ff0099",border=NA,add=TRUE)
hist(img[,,3],breaks=seq(0,1,by=0.05), col="#0000ff99",border=NA,add=TRUE)

# We can save the output of the historam and use it as
# features in the model
h <- hist(img[,,1],breaks=seq(0,1,by=0.05), plot=FALSE)
h$density

X <- matrix(NA, ncol=3*20, nrow=n)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  h <- apply(img,3,hist,breaks=seq(0,1,by=0.05), plot=FALSE)
  h <- Map(`getElement`, h, "density")
  X[j,] <- unlist(h)
  print(sprintf("%03d / %03d", j, n))
}

out2 <- glm(y ~ X, family=binomial)
summary(out2)

pred2 <- predict(out2, type="response")

r2 <- roc(y, pred2)
plot(r2$fpr, r2$tpr, xlab="false positive rate",
     ylab="true positive rate", type="l", col="blue")
lines(r$fpr, r$tpr)

auc(r)
auc(r2)
