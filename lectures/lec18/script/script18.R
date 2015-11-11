##################### Photograph examples
library(jpeg)
pm <- read.csv("photoMetaData.csv")
pm <- pm[pm$category %in% c("artificial","natural"),]
n <- nrow(pm)

trainFlag <- (runif(n) > 0.5)
y <- as.numeric(pm$category == "natural")

X <- matrix(NA, ncol=3, nrow=n)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  X[j,] <- apply(img,3,mean)
  print(sprintf("%03d / %03d", j, n))
}

out <- glm(y ~ X, family=binomial, subset=trainFlag)
summary(out)

pred <- 1 / (1 + exp(-1 * cbind(1,X) %*% coef(out)))
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

r <- roc(y[!trainFlag], pred[!trainFlag])
plot(r$fpr, r$tpr, xlab="false positive rate", ylab="true positive rate", type="l")
abline(0,1,lty="dashed")

auc <- function(r) {
  sum((r$fpr) * diff(c(0,r$tpr)))
}

auc(r)

# What are worst and best examples?
temp <- cbind(pm[order(pred),],y[order(pred)])

id <- which(temp$y == 1)[1]
system(paste0("open columbiaImages/",temp$name[id]))

id <- rev(which(temp$y == 0))[1]
system(paste0("open columbiaImages/",temp$name[id]))

id <- which(temp$y == 0)[1]
system(paste0("open columbiaImages/",temp$name[id]))

id <- rev(which(temp$y == 1))[1]
system(paste0("open columbiaImages/",temp$name[id]))

#############################
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

X2 <- matrix(NA, ncol=3*20, nrow=n)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  h <- apply(img,3,hist,breaks=seq(0,1,by=0.05), plot=FALSE)
  h <- Map(`getElement`, h, "density")
  X2[j,] <- unlist(h)
  print(sprintf("%03d / %03d", j, n))
}

# Why are there NAs?
out2 <- glm(y ~ X2, family=binomial, subset=trainFlag)
summary(out2)

X3 <- X2[,c(1:9,11:19,21:29)]

out3 <- glm(y ~ X3, family=binomial, subset=trainFlag)
summary(out3)

pred3 <- 1 / (1 + exp(-1 * cbind(1,X3) %*% coef(out3)))
y[order(pred3)]

r2 <- roc(y, pred2)
plot(r2$fpr, r2$tpr, xlab="false positive rate",
     ylab="true positive rate", type="l", col="blue")
lines(r$fpr, r$tpr)

auc(r)
auc(r2)

######## ridge regression?
library(glmnet)

out <- glmnet(X[trainFlag,], y[trainFlag], family="binomial", alpha=0)
ridgeP <- predict(out,newx=X2[!trainFlag,], type="response")
dim(ridgeP)

r3 <- roc(y[!trainFlag], ridgeP[,2])
plot(r2$fpr, r2$tpr, xlab="false positive rate",
     ylab="true positive rate", type="l", col="blue")
lines(r$fpr, r$tpr)
lines(r3$fpr, r3$tpr, col="salmon")




