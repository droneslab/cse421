# Code assumes you have saved the relevant data locally;
# it is generally too big to keep fetching over HTTP
setwd("~/files/class_data/")
dir()

# A sample of the raw data
x <- read.csv(bzfile("2007.csv.bz2"), as.is=TRUE, nrow=1000)
str(x)

# The cleaned version:
x <- readRDS("airline2007_clean.Rds")
rownames(x) <- NULL
dim(x)
str(x)

# For now, going to take a 5% sample to speed up the
# plots and exploration
x <- x[sample(1:nrow(x),nrow(x)*0.05),]

# Calculate the base linear model
out <- lm(arrDelay ~ depDelay, data=x)
out
summary(out)

# plot the residuals
plot(out$fitted, out$resid, pch=".")

plot(out$fitted/3600, out$resid/3600, pch=".",
     xlab="Py", ylab="My")
abline(h=0,col="red", lty="dashed")

plot(out$fitted/3600, out$resid/3600, pch=19,
     xlab="Py", ylab="My", cex=0.5,
     col=rgb(0,0,0,0.01))

# Let's explore relationship to other covariates
tapply(out$resid, x$dest, mean)
sort(round(tapply(out$resid, x$dest, mean) / 60))
sort(round(tapply(out$resid, x$dest, sd) / 60))

# make sense of this data:
sdDest <- sort(round(tapply(out$resid, x$dest, sd) / 60),
                decreasing=TRUE)
air <- read.csv("airports.csv", as.is=TRUE)
index <- match(names(sdDest), air$iata)

cat(sprintf("%40s - %d", air$airport[index], sdDest),
     sep="\n")

library(maps)
map("usa")
map("state",add=TRUE,lwd=0.2)
cols <- rgb(0,0,1,sdDest / max(sdDest))
points(air$long[index], air$lat[index],
        pch=19, cex=1.5, col=heat_hcl(100,alpha=0.7))

# Now, try to fit the linear model using airport as
# a covariate
out2 <- lm(arrDelay ~ depDelay + dest - 1, data=x)
out2
summary(out2)

out2 <- lm(arrDelay ~ depDelay + dest - 1, data=x)
plot(out2$fitted/3600, out2$resid/3600, pch=".",
     xlab="Py", ylab="My")
abline(h=0,col="red", lty="dashed")

# How do the two fits align?
anova(out,out2)
summary(out)$r.squared
summary(out2)$r.squared
plot(out$resid,out2$res, pch=".")
