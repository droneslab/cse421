########
# prelims from last time (need to run before today's code)
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
auc <- function(r) {
  sum((r$fpr) * diff(c(0,r$tpr)))
}

library(jpeg)
pm <- read.csv("photoMetaData.csv")
n <- nrow(pm)

set.seed(1)
trainFlag <- (runif(n) > 0.5)
y <- as.numeric(pm$category == "outdoor-day")

########
X <- matrix(NA, ncol=3*101, nrow=n)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  h <- apply(img,3,quantile,probs=seq(0,1,by=0.01))
  X[j,] <- as.numeric(h)
  print(sprintf("%03d / %03d", j, n))
}
#saveRDS(X, "centile_rgb.Rds")

# Now, going to run two versions of the Lasso
library(glmnet)
X20 <- readRDS("quantile_rgb.Rds")
outLasso20 <- glmnet(X20[trainFlag,], y[trainFlag], family="binomial",
                    alpha=1)
X100 <- readRDS("centile_rgb.Rds")
outLasso100 <- glmnet(X100[trainFlag,], y[trainFlag], family="binomial",
                      alpha=1)

# Find AUC values for all of the curves
aucValsLasso20 <- rep(NA, length(outLasso20$lambda))
for (j in 1:length(outLasso20$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,X20) %*% coef(outLasso20)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsLasso20[j] <- auc(r)
}
aucValsLasso100 <- rep(NA, length(outLasso100$lambda))
for (j in 1:length(outLasso100$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,X100) %*% coef(outLasso100)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsLasso100[j] <- auc(r)
}

# Plot these over one another
plot(log(outLasso20$lambda)[-1], aucValsLasso20[-1], type="l",
      xlab="log lambda", ylab="AUC", main="lasso",
      ylim=range(c(aucValsLasso20[-1],aucValsLasso100[-1])))
lines(log(outLasso100$lambda)[-1], aucValsLasso100[-1], col="navy", lwd=2)

# Add glm to plot
out20 <- glm(y ~ X20, family=binomial, subset=trainFlag)
pred <- 1 / (1 + exp(-1 * cbind(1,X20) %*% coef(out20)))
glmAuc20 <- auc(roc(y[trainFlag == 0], pred[trainFlag == 0]))

out100 <- glm(y ~ X100, family=binomial, subset=trainFlag)
pred <- 1 / (1 + exp(-1 * cbind(1,X100) %*% coef(out100)))
glmAuc100 <- auc(roc(y[trainFlag == 0], pred[trainFlag == 0]))

abline(h=glmAuc20, lty="dashed", lwd=2)
abline(h=glmAuc100, col="navy", lty="dashed", lwd=2)

#######
# Let's try this with ridge regression
X20 <- readRDS("quantile_rgb.Rds")
outRidge20 <- glmnet(X20[trainFlag,], y[trainFlag], family="binomial",
                     alpha=0, lambda.min.ratio=1e-6)
X100 <- readRDS("centile_rgb.Rds")
outRidge100 <- glmnet(X100[trainFlag,], y[trainFlag], family="binomial",
                     alpha=0, lambda.min.ratio=1e-6)

# Find AUC values for all of the curves
aucValsRidge20 <- rep(NA, length(outRidge20$lambda))
for (j in 1:length(outRidge20$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,X20) %*% coef(outRidge20)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsRidge20[j] <- auc(r)
}
aucValsRidge100 <- rep(NA, length(outRidge100$lambda))
for (j in 1:length(outRidge100$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,X100) %*% coef(outRidge100)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsRidge100[j] <- auc(r)
}

plot(log(outRidge20$lambda)[-1], aucValsRidge20[-1], type="l",
      xlab="log lambda", ylab="AUC", main="ridge",
      ylim=range(c(aucValsRidge20[-1],aucValsRidge100[-1])))
lines(log(outRidge100$lambda)[-1], aucValsRidge100[-1], col="navy", lwd=2)
abline(h=glmAuc20, lty="dashed", lwd=2)
abline(h=glmAuc100, col="navy", lty="dashed", lwd=2)

#######
# A note on refitting

aucValsRefit20 <- rep(0, length(outLasso20$lambda))
for (j in 2:length(outLasso20$lambda)) {
  index <- which(coef(outLasso20)[-1,j] != 0)
  out <- glm(y[trainFlag] ~ X20[trainFlag,index], family="binomial")
  pred <- 1 / (1 + exp(-1 * cbind(1,X20[,index]) %*% coef(out)))
  aucValsRefit20[j] <- auc(roc(y[!trainFlag], pred[!trainFlag]))
}

plot(log(outLasso20$lambda)[-1], aucValsLasso20[-1], type="l",
      xlab="log lambda", ylab="AUC", main="lasso",
      ylim=range(c(aucValsRidge20[-1],aucValsRidge100[-1])))

lines(log(outLasso20$lambda)[-1], aucValsRefit20[-1], col="salmon", lwd=2)

#######
# The elastic net

X20 <- readRDS("quantile_rgb.Rds")
outEnet20 <- glmnet(X20[trainFlag,], y[trainFlag], family="binomial",
                     alpha=0.3, lambda.min.ratio=1e-6)
X100 <- readRDS("centile_rgb.Rds")
outEnet100 <- glmnet(X100[trainFlag,], y[trainFlag], family="binomial",
                     alpha=0.3, lambda.min.ratio=1e-6)

# Find AUC values for all of the curves
aucValsEnet20 <- rep(NA, length(outEnet20$lambda))
for (j in 1:length(outEnet20$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,X20) %*% coef(outEnet20)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsEnet20[j] <- auc(r)
}
aucValsEnet100 <- rep(NA, length(outEnet100$lambda))
for (j in 1:length(outEnet100$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,X100) %*% coef(outEnet100)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsEnet100[j] <- auc(r)
}

plot(log(outEnet20$lambda)[-1], aucValsEnet20[-1], type="l",
      xlab="log lambda", ylab="AUC", main="lasso",
      ylim=range(c(aucValsEnet20[-1],aucValsEnet100[-1])))
lines(log(outEnet100$lambda)[-1], aucValsEnet100[-1], col="navy", lwd=2)

# Plot all three
plot(log(outEnet100$lambda)[-1], aucValsEnet100[-1], type="l",
      xlab="log lambda", ylab="AUC", main="lasso",
      ylim=range(c(aucValsEnet100[-1],aucValsLasso100[-1], aucValsRidge100[-1])),
      col="red")
lines(log(outLasso100$lambda)[-1], aucValsLasso100[-1], col="navy", lwd=2)
lines(log(outRidge100$lambda)[-1], aucValsRidge100[-1], col="orange", lwd=2)

# A better plot
plot(aucValsEnet100[-1], type="l",
      xlab="step", ylab="AUC", main="lasso",
      ylim=range(c(aucValsEnet100[-1],aucValsLasso100[-1], aucValsRidge100[-1])),
      col="red")
lines(aucValsLasso100[-1], col="navy", lwd=2)
lines(aucValsRidge100[-1], col="orange", lwd=2)

#######
# Now, how would we find the optimal value of lambda?

# Find AUC values for all of the curves
aucValsLassoTrain <- rep(NA, length(outLasso100$lambda))
for (j in 1:length(outLasso100$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,X100) %*% coef(outLasso100)[,j]))
  r <- roc(y[trainFlag == 1], pred[trainFlag == 1])
  aucValsLassoTrain[j] <- auc(r)
}
aucValsLassoTest <-aucValsLasso100

plot(log(outLasso100$lambda)[-1], aucValsLassoTrain[-1], type="l",
      xlab="log lambda", ylab="AUC", main="lasso",
      ylim=range(c(aucValsLassoTrain[-1],aucValsLassoTest[-1])), col="red")
lines(log(outLasso100$lambda)[-1], aucValsLassoTest[-1], col="navy", lwd=2)

# Cross validation
outLasso <- cv.glmnet(X100[trainFlag,], y[trainFlag], family="binomial",
                      alpha=0, nfolds=10, lambda.min.ratio=1e-6)
plot(outLasso)
outLasso$lambda.min
outLasso$lambda.1se

# auc?
outLasso <- cv.glmnet(X100[trainFlag,], y[trainFlag], family="binomial", alpha=0,
                      nfolds=10, lambda.min.ratio=1e-6, type.measure="auc")
plot(outLasso)
outLasso$lambda.min
outLasso$lambda.1se


betaMin <- coef(outLasso, outLasso$lambda.min)
beta1se <- coef(outLasso, outLasso$lambda.1se)
sqrt(apply(betaMin^2,2,sum))
sqrt(apply(beta1se^2,2,sum))

predMin <- predict(outLasso, X100, outLasso$lambda.min, type="response")
pred1se <- predict(outLasso, X100, outLasso$lambda.1se, type="response")
cor(predMin,pred1se)
mean((predMin > 0.5) == (pred1se > 0.5))
plot(predMin, pred1se)

########
# How about a new set of features?

# Texture!
system(paste0("open columbiaImages/",pm$name[100]))

img <- readJPEG(paste0("columbiaImages/",pm$name[100]))
img <- (img[,,1] + img[,,2] + img[,,3]) / 3

delta <- abs(img[-1,-1] - img[-nrow(img),-ncol(img)])
delta <- delta / max(delta)
writeJPEG(1-abs(img[-1,-1] - img[-nrow(img),-ncol(img)]) + 0.5,
           "~/Desktop/texture.jpg")

# Grab texture features as quantiles
X <- matrix(NA, ncol=21, nrow=n)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  img <- (img[,,1] + img[,,2] + img[,,3]) / 3
  h <- quantile(abs(img[-1,-1] - img[-nrow(img),-ncol(img)]),seq(0,1,by=0.05))
  X[j,] <- as.numeric(h)
  print(sprintf("%03d / %03d", j, n))
}
#saveRDS(X, "quantile_texture.Rds")

# Calculate AUC for color, texture, and both combined
Xtext <- readRDS("quantile_texture.Rds")
outText <- glmnet(Xtext[trainFlag,], y[trainFlag], family="binomial", alpha=0.8)
Xcol <- readRDS("centile_rgb.Rds")
outCol <- glmnet(Xcol[trainFlag,], y[trainFlag], family="binomial", alpha=0.8)
Xboth <- cbind(readRDS("quantile_texture.Rds"),readRDS("centile_rgb.Rds"))
outBoth <- glmnet(Xboth[trainFlag,], y[trainFlag], family="binomial", alpha=0.8)

aucValsText <- rep(NA, length(outText$lambda))
for (j in 1:length(outText$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,Xtext) %*% coef(outText)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsText[j] <- auc(r)
}
aucValsCol <- rep(NA, length(outCol$lambda))
for (j in 1:length(outCol$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,Xcol) %*% coef(outCol)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsCol[j] <- auc(r)
}
aucValsBoth <- rep(NA, length(outBoth$lambda))
for (j in 1:length(outBoth$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,Xboth) %*% coef(outBoth)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsBoth[j] <- auc(r)
}

# Plot all three

plot(log(outText$lambda)[-1], aucValsText[-1], type="l",
      xlab="log lambda", ylab="AUC", main="lasso",
      ylim=range(c(aucValsText[-1],aucValsCol[-1], aucValsBoth[-1])), col="red")
lines(log(outCol$lambda)[-1], aucValsCol[-1], col="navy", lwd=2)
lines(log(outBoth$lambda)[-1], aucValsBoth[-1], col="orange", lwd=2)
abline(h=glmAuc20, lty="dashed", lwd=2)

