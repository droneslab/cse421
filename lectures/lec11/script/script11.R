### A clever solution to I.2 on problem set 3
x <- readRDS("airline2007_pset03.Rds")

groupI <- x[x$group == "I",]
groupII <- x[x$group == "II",]
groupIII <- x[x$group == "III",]

# Calculate form of V:
outI <- lm(arrDelay ~ depDelay + dest - 1, data = groupI)
varTable <- tapply(outI$resid, groupI$dest, var)
index <- match(groupII$dest, names(varTable))
weights <- 1/sqrt(as.numeric(varTable)[index])

# Compute betaOLS and betaGLS
ols <- lm(arrDelay ~ depDelay + dest - 1, data = groupII)
gls <- lm(arrDelay ~ depDelay + dest - 1, data = groupII, weights=weights)

ols$coefficients - gls$coefficients

# To do prediction, need variance of the new terms
index <- match(groupIII$dest, names(varTable))
predVar <- as.numeric(varTable)[index]

predOLS <- predict(ols, newdata=groupIII, interval="prediction")
predGLS <- predict(gls, newdata=groupIII, interval="prediction", pred.var=predVar)

res <- groupIII$arrDelay
mean(predOLS[,2] < res & res < predOLS[,3])
mean(predGLS[,2] < res & res < predGLS[,3])

sort(tapply(predOLS[,2] < res & res < predOLS[,3], groupIII$dest, mean))
sort(tapply(predGLS[,2] < res & res < predGLS[,3], groupIII$dest, mean))
