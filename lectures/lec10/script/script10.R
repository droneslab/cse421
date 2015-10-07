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
rownames(x) <- NULL
rownames(w) <- NULL

##################################
# If we do a model of arrDelay ~ depDelay + carrier, what
# does model.matrix look like?
mf <- model.frame(arrDelay ~ depDelay + carrier, data=x)
mm <- model.matrix(mf, data=x)

head(mm)
table(x$carrier)

mf <- model.frame(arrDelay ~ depDelay*carrier, data=x)
mm <- model.matrix(mf, data=x)
head(mm)

###################################
# How could we do this by hand?
carrierFactor <- factor(x$carrier)
con <- contr.treatment(levels(carrierFactor))
con

con <- contr.treatment(levels(carrierFactor), contrasts=FALSE)
con

con <- contr.sum(levels(carrierFactor))
con

t(con) %*% con

con <- contr.helmert(levels(carrierFactor))
con

t(con) %*% con

# How to apply?
mf <- model.frame(arrDelay ~ depDelay + carrier, data=x)
mm <- model.matrix(mf, data=x, contrasts = list(carrier="contr.helmert"))
head(mm)

out <- lm(arrDelay ~ depDelay + carrier, data=x,
           contrasts = list(carrier="contr.helmert"))
summary(out)

###############################
# Sparsity
con <- contr.treatment(levels(carrierFactor), sparse=TRUE)
con

class(con)
t(con)

sessionInfo()

library(Matrix)
t(con)

p <- 10
m <- matrix(sample(0:1,p^2,prob=c(0.9,0.1),replace=TRUE),p,p)
m
Matrix(m)

p <- 10
m <- matrix(sample(0:1,p^2,prob=c(0.2,0.8),replace=TRUE),p,p)
m
Matrix(m)
Matrix(m,sparse=TRUE)

m <- matrix(sample(0:1,p^2,prob=c(0.9,0.1),replace=TRUE),p,p)
M <- Matrix(m)
object.size(m)
object.size(M)

for (p in c(10,100,1000)) {
  m <- matrix(sample(0:1,p^2,prob=c(0.9,0.1),replace=TRUE),p,p)
  M <- Matrix(m)
  cat("p:", p, "\n")
  cat("m:", object.size(m), "\n")
  cat("M:", object.size(M), "\n\n")
}

Mnew <- M %*% m
class(Mnew)
class(as.matrix(Mnew))

for (p in c(10,100,1000)) {
  m <- diag(p)
  M <- Matrix(m)
  cat("p:", p, "\n")
  cat("m:", object.size(m), "\n")
  cat("M:", object.size(M), "\n\n")
}

# Look how fast this is!
M <- Diagonal(1e7)
class(M)

# Try in another window: m <- diag(1e7)

mf <- model.frame(arrDelay ~ depDelay + carrier, data=x)
mm <- sparse.model.matrix(mf, data=x)
head(mm)

###############################
# multcomp (automatic F tests)
library(multcomp)
out <- lm(arrDelay ~ depDelay + carrier, data=x)
summary(out)

D <- matrix(0, ncol=length(coef(out)), nrow=3)
D[1,11] <- D[2,17] <- D[3,18] <- 1
d <- c(0,0,0)
summary(glht(out, linfct=D, rhs=d), test=Ftest())

###############################
# Heirarchical models
out <- lm(arrDelay ~ depDelay + dest - 1, data=w)
summary(out)

destSet <- sort(unique(w$dest))
predDF <- data.frame(depDelay = 0, dest = destSet)
pred <- predict(out, predDF)
hist(pred)

# Heirarchy
index <- match(x$dest, destSet)
x$predDest <- pred[index]

out1 <- lm(arrDelay ~ depDelay + dest - 1, data=x)
out2 <- lm(arrDelay ~ depDelay + predDest, data=x)

mean(out1$resid^2) / mean(out2$resid^2)

# Why so useful?
# Now, use carrier and arrival airport

outDest <- lm(arrDelay ~ depDelay + dest - 1, data=w)
outOrigin <- lm(arrDelay ~ depDelay + origin - 1, data=w)
outCarrier <- lm(arrDelay ~ depDelay + carrier - 1, data=w)
outHour <- lm(arrDelay ~ depDelay + factor(hour) - 1, data=w)

destSet <- sort(unique(w$dest))
originSet <- sort(unique(w$origin))
carrierSet <- sort(unique(w$carrier))
hourSet <- sort(unique(w$hour))

predDestDF <- data.frame(depDelay = 0, dest = destSet)
predOriginDF <- data.frame(depDelay = 0, origin = originSet)
predCarrierDF <- data.frame(depDelay = 0, carrier = carrierSet)
predHourDF <- data.frame(depDelay = 0, hour = hourSet)

predDest <- predict(outDest, predDestDF)
predOrigin <- predict(outOrigin, predOriginDF)
predCarrier <- predict(outCarrier, predCarrierDF)
predHour <- predict(outHour, predHourDF)

index <- match(x$dest, destSet)
x$predDest <- predDest[index]
index <- match(x$origin, originSet)
x$predOrigin <- predOrigin[index]
index <- match(x$carrier, carrierSet)
x$predCarrier <- predCarrier[index]
index <- match(x$hour, hourSet)
x$predHour <- predHour[index]


out1 <- lm(arrDelay ~ depDelay + dest + origin + carrier + factor(hour), data=x)
out2 <- lm(arrDelay ~ depDelay + predDest + predOrigin + predCarrier + predHour, data=x)

mean(out1$resid^2) / mean(out2$resid^2)

p1 <- predict(out1, interval="confidence")
p2 <- predict(out2, interval="confidence")

round(quantile(p1[,"upr"] - p1[,"lwr"]) / 60, 1)
round(quantile(p2[,"upr"] - p2[,"lwr"]) / 60, 1)

pred <- predict(out2)
bins <- cut(pred, quantile(pred, seq(0,1,0.1)), labels=FALSE)
tapply(pred, bins, range)

tapply(pred, bins, range)
tapply(pred, bins, range)




