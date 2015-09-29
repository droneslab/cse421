# Code assumes you have saved the relevant data locally;
# it is generally too big to keep fetching over HTTP
setwd("~/files/class_data/")
dir()
x <- readRDS("airline2007_clean.Rds")
rownames(x) <- NULL
dim(x)
str(x)

# Calculate the day of the year of arrival for
# everything in the dataset
day <- as.numeric(format(x$aTime + x$aOffset, "%j", tz="GMT"))
hist(day, breaks=0:365, col="black")

# Also calculate local hour of arrival for every flight
hour <- as.numeric(format(x$aTime + x$aOffset, "%H", tz="GMT"))

# Start by just taking a single day of data:
z <- x[day == 45 & hour == 12,]
dim(z)

# Look at leverage
out <- lm(arrDelay ~ depDelay, data=z)
out
summary(out)

mm <- model.frame(arrDelay ~ depDelay, data=z)
X <- model.matrix(mm, data=z)
lev <- diag(X %*% solve(t(X) %*% X) %*% t(X))
hist(lev)
sum(lev)

plot(z$depDelay, lev, pch=19, cex=0.3, log="y")
abline(v=mean(z$depDelay),col="red")

newx <- data.frame(depDelay=seq(0,36000,length.out=1e3))

# Confidence region
pred <- predict(out, newx, interval="confidence")
head(pred)

plot(newx$depDelay, pred[,"fit"], type='l')
points(z$depDelay, z$arrDelay, pch=19, cex=0.2, col=rgb(0,0,0,0.1))
lines(newx$depDelay, pred[,"fit"], col="red")
lines(newx$depDelay, pred[,"lwr"], col="blue")
lines(newx$depDelay, pred[,"upr"], col="blue")

# Prediction region
pred2 <- predict(out, newx, interval="prediction")
head(pred2)

plot(newx$depDelay, pred[,"fit"], type='l')
points(z$depDelay, z$arrDelay, pch=19, cex=0.2, col=rgb(0,0,0,0.1))
lines(newx$depDelay, pred[,"fit"], col="red")
lines(newx$depDelay, pred[,"lwr"], col="blue")
lines(newx$depDelay, pred[,"upr"], col="blue")
lines(newx$depDelay, pred2[,"lwr"], col="green")
lines(newx$depDelay, pred2[,"upr"], col="green")

############
# For now, going to take a 5% sample to speed up the
# plots and exploration; and then split into two
# groups
x <- x[sample(1:nrow(x),nrow(x)*0.05),]
index <- sample(0:1,nrow(x),replace=TRUE)
newx <- x[index == 0,]
oldx <- x[index != 0,]

# Calculate the linear model with airport effects
out <- lm(arrDelay ~ depDelay + dest - 1, data=oldx)
out
summary(out)

# Construct a prediction region:
pred <- predict(out, newx, interval="prediction")

successFlag <- (pred[,2] < newx$arrDelay & pred[,3] > newx$arrDelay)
mean(successFlag)

sort(tapply(successFlag, newx$dest, mean))

#############
varhat <- tapply(out$resid, oldx$dest, var)
index <- match(newx$dest, names(varhat))
pred.var <- varhat[index]

pred <- predict(out, newx, interval="prediction", pred.var=pred.var)

successFlag <- (pred[,2] < newx$arrDelay & pred[,3] > newx$arrDelay)
mean(successFlag)
sort(tapply(successFlag, newx$dest, mean))


##########
tab <- round(sort(tapply(bigDelay, x$dest, mean)),2)
tab

tab <- sort(tab,decreasing=TRUE)
index <- match(names(tab), air$iata)

library(maps)
map("usa")
map("state",add=TRUE,lwd=0.2)
cols <- rgb(0,0,1,sdDest / max(sdDest))
points(air$long[index], air$lat[index],
        pch=19, col=heat_hcl(100,alpha=0.7))

out2 <- lm(bigDelay ~ x$depDelay + bigDepDelay + x$dest - 1)

anova(out,out2)
summary(out)$r.squared
summary(out2)$r.squared
plot(out$resid,out2$res, pch=".")


###########
upsetScore <- (x$arrDelay - 15*60) / (3600*2 - 15*60)
upsetScore[x$arrDelay < 15*60] <- 0
upsetScore[x$arrDelay > 3600*2] <- 1

out <- lm(upsetScore ~ x$depDelay)
summary(out)

tab <- round(sort(tapply(upsetScore, x$dest, mean)),2)
out2 <- lm(upsetScore ~ x$depDelay + x$dest)

hour <- as.numeric(format(x$aTime + x$aOffset, "%H", tz="GMT"))
tapply(upsetScore, hour, mean)
plot(tapply(upsetScore, hour, mean),type="l")

out3 <- lm(upsetScore ~ x$depDelay + factor(hour))
plot(out$resid, out3$resid, pch=".")






