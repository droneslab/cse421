# Code assumes you have saved the relevant data locally;
# it is generally too big to keep fetching over HTTP
setwd("~/files/class_data/")
dir()

# The cleaned version:
x <- readRDS("airline2007_clean.Rds")
rownames(x) <- NULL
dim(x)
str(x)

# For now, going to take two 5% samples to speed up the
# plots and exploration
index <- sample(1:20,nrow(x),replace=TRUE)
w <- x[index == 1,]
x <- x[index == 2,]
x$hour <- as.numeric(format(x$aTime + x$aOffset, "%H", tz="GMT"))
w$hour <- as.numeric(format(w$aTime + w$aOffset, "%H", tz="GMT"))




round(quantile(x$arrDelay, seq(0,1,0.01)) / 3600)

bigDelay <- (x$arrDelay > 3600 * 2)
bigDepDelay <- (x$depDelay > 3600 * 2)
mean(bigDelay)
sum(bigDelay)

out <- lm(bigDelay ~ x$depDelay)
plot(out$fitted, out$resid, pch=".",
     xlab="Py", ylab="My")
abline(h=0,col="red", lty="dashed")

out <- lm(bigDelay ~ x$depDelay + bigDepDelay)
plot(out$fitted, out$resid, pch=".",
     xlab="Py", ylab="My")
abline(h=0,col="red", lty="dashed")