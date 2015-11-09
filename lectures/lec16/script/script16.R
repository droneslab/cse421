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
t(T) %*% T

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

#####################










