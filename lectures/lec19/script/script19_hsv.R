

########

# a new coordinate system
X <- matrix(NA, ncol=3*21, nrow=n)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  hsv <- t(rgb2hsv(as.numeric(img[,,1]),as.numeric(img[,,2]),as.numeric(img[,,3]),1))
  h <- apply(hsv,2,quantile,probs=seq(0,1,by=0.05))
  X[j,] <- as.numeric(h)
  print(sprintf("%03d / %03d", j, n))
}
#saveRDS(X, "quantile_hsv.Rds")
X <- readRDS("quantile_hsv.Rds")

##
out <- glm(y ~ X, family=binomial, subset=trainFlag == 1)
pred <- 1 / (1 + exp(-1 * cbind(1,X) %*% coef(out)))
glmAuc # old
auc(roc(y[trainFlag == 0], pred[trainFlag == 0])) # new

# ridge
outRidge <- glmnet(X[trainFlag == 1,], y[trainFlag == 1], family="binomial", alpha=0)

aucValsRidge <- rep(NA, length(outLasso$lambda))
for (j in 1:length(outRidge$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,X) %*% coef(outRidge)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsRidge[j] <- auc(r)
}

plot(log(outRidge$lambda)[-1], aucValsRidge[-1], type="l",
      xlab="log lambda", ylab="AUC", main="ridge",
      ylim=range(c(aucValsRidge[-1],glmAuc)))
abline(h=glmAuc,col="salmon")
text(log(outRidge$lambda)[50],glmAuc-0.003,"glm value",col="salmon")

# lasso
library(glmnet)
outLasso <- glmnet(X[trainFlag,], y[trainFlag], family="binomial", alpha=1)

aucValsLasso <- rep(NA, length(outLasso$lambda))
for (j in 1:length(outLasso$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,X) %*% coef(outLasso)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsLasso[j] <- auc(r)
}

plot(log(outLasso$lambda)[-1], aucValsLasso[-1], type="l",
      xlab="log lambda", ylab="AUC", main="lasso",
      ylim=range(c(aucValsLasso[-1],glmAuc)))
abline(h=glmAuc,col="salmon")
text(log(outLasso$lambda)[50],glmAuc-0.0012,"glm value",col="salmon")

########

# another new coordinate system

X <- matrix(NA, ncol=10 + 2*21, nrow=n)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  hsv <- t(rgb2hsv(as.numeric(img[,,1]),as.numeric(img[,,2]),as.numeric(img[,,3]),1))
  h <- apply(hsv,2,quantile,probs=seq(0,1,by=0.05))
  X[j,-c(1:10)] <- as.numeric(h[,2:3])
  X[j,1:10] <- hist(hsv[,1],breaks=seq(0,1,length.out=11),plot=FALSE)$density[1:10]
  print(sprintf("%03d / %03d", j, n))
}
#saveRDS(X, "quantile_hist_hsv.Rds")
X <- readRDS("quantile_hist_hsv.Rds")

# lasso
outLasso <- glmnet(X[trainFlag,], y[trainFlag], family="binomial", alpha=0.6)

aucValsLasso <- rep(NA, length(outLasso$lambda))
for (j in 1:length(outLasso$lambda)) {
  pred <- 1 / (1 + exp(-1 * cbind(1,X) %*% coef(outLasso)[,j]))
  r <- roc(y[trainFlag == 0], pred[trainFlag == 0])
  aucValsLasso[j] <- auc(r)
}

plot(log(outLasso$lambda)[-1], aucValsLasso[-1], type="l",
      xlab="log lambda", ylab="AUC", main="lasso",
      ylim=range(c(aucValsLasso[-1],glmAuc)))
abline(h=glmAuc,col="salmon")
text(log(outLasso$lambda)[50],glmAuc-0.0012,"glm value",col="salmon")




