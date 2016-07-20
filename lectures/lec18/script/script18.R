### Photograph examples

## Read in the library and metadata
library(jpeg)
pm <- read.csv("photoMetaData.csv")
n <- nrow(pm)

trainFlag <- (runif(n) > 0.5)
y <- as.numeric(pm$category == "outdoor-day")

X <- matrix(NA, ncol=3, nrow=n)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  X[j,] <- apply(img,3,median)
  print(sprintf("%03d / %03d", j, n))
}
#saveRDS(X, "median_rgb.Rds")
X <- readRDS("median_rgb.Rds")

# build a glm model on these median values
out <- glm(y ~ X, family=binomial, subset=trainFlag)
out$iter
summary(out)

# How well did we do?
pred <- 1 / (1 + exp(-1 * cbind(1,X) %*% coef(out)))
y[order(pred)]
y[!trainFlag][order(pred[!trainFlag])]

mean((as.numeric(pred > 0.5) == y)[trainFlag])
mean((as.numeric(pred > 0.5) == y)[!trainFlag])

## ROC curve (see lecture 12)
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

r <- roc(y[!trainFlag], pred[!trainFlag])
plot(r$fpr, r$tpr, xlab="false positive rate", ylab="true positive rate", type="l")
abline(0,1,lty="dashed")

# auc
auc <- function(r) {
  sum((r$fpr) * diff(c(0,r$tpr)))
}
glmAuc <- auc(r)
glmAuc

## ridge regression?
library(glmnet)
outRidge <- glmnet(X[trainFlag,], y[trainFlag], family="binomial",
                    alpha=0)

aucValsRidge <- rep(NA, length(outRidge$lambda))
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

## lasso regression?
outLasso <- glmnet(X[trainFlag,], y[trainFlag], family="binomial",
                    alpha=1)

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

# What are worst and best examples?
pred <- 1 / (1 + exp(-1 * cbind(1,X) %*% coef(out)))
temp <- cbind(pm[order(pred),],y[order(pred)])

# outdoor image that looks like it is not
id <- which(temp$y == 1)[1]
system(paste0("open columbiaImages/",temp$name[id]))

# non-outdoor image that looks like it is not
id <- max(which(temp$y == 0))
system(paste0("open columbiaImages/",temp$name[id]))

# "most" outdoor-like image
id <- max(which(temp$y == 1))
system(paste0("open columbiaImages/",temp$name[id]))

########

img <- readJPEG(paste0("columbiaImages/",pm$name[500]))
system(paste0("open columbiaImages/",pm$name[500]))

hist(img[,,1],breaks=seq(0,1,by=0.05))

# You may have seen something like this on a fancy digital camera
hist(img[,,1],breaks=seq(0,1,by=0.05), col="#ff000099",border=NA)
hist(img[,,2],breaks=seq(0,1,by=0.05), col="#00ff0099",border=NA,add=TRUE)
hist(img[,,3],breaks=seq(0,1,by=0.05), col="#0000ff99",border=NA,add=TRUE)

########

X <- matrix(NA, ncol=3*21, nrow=n)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  h <- apply(img,3,quantile,probs=seq(0,1,by=0.05))
  X[j,] <- as.numeric(h)
  print(sprintf("%03d / %03d", j, n))
}
saveRDS(X, "quantile_rgb.Rds")
#X <- readRDS("quantile_rgb.Rds")

out <- glm(y ~ X, family=binomial, subset=trainFlag)
pred <- 1 / (1 + exp(-1 * cbind(1,X) %*% coef(out)))
glmAuc # old
auc(roc(y[!trainFlag], pred[!trainFlag])) # new

glmAuc <- auc(roc(y[trainFlag == 0], pred[trainFlag == 0]))

# ridge regression now?
outRidge <- glmnet(X[trainFlag == 1,], y[trainFlag == 1],
                    family="binomial", alpha=0)

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

## lasso regression?
outLasso <- glmnet(X[trainFlag,], y[trainFlag], family="binomial",
                   alpha=1)

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

# visualizing this all?

SVD <- svd(X)
U <- SVD$u

plot(U[,1], U[,2], col=y+1, pch=19, cex=0.5)
plot(U[,2], U[,3], col=y+1, pch=19, cex=0.5)

U[,1] <- U[,1] - min(U[,1])
U[,1] <- U[,1] / max(U[,1])
U[,2] <- U[,2] - min(U[,2])
U[,2] <- U[,2] / max(U[,2])

set.seed(1)
par(mar=c(0,0,0,0))
rho <- 0.1
plot(0,0,type="n", xlim=c(0,1+rho), ylim=c(0,1+rho))

files <- paste0("columbiaImages/",pm$name)
for (j in sample(1:length(files))) {
  z <- readJPEG(files[j])

  rat <- dim(z)[1] / dim(z)[2]
  delta_x <- ifelse(rat > 1, 1, rat) * rho
  delta_y <- ifelse(rat > 1, rat, 1) * rho
  rasterImage(z, xleft=U[j,1], ybottom=U[j,2],
                 xright=U[j,1]+delta_x, ytop=U[j,2]+delta_y)
  rect(U[j,1],U[j,2],U[j,1]+delta_x,U[j,2]+delta_y,lwd=2)
}