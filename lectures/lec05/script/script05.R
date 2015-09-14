###################################
# Galton's height data
urlBase <- "http://euler.stat.yale.edu/~tba3/stat612/lectures/lec03/data/"
urlBase <- "~/files/stat612/lectures/lec03/data/"
h <- read.csv(paste0(urlBase, "galton_heights.csv"))
head(h)

summary(lm(Height ~ Father - 1, data=h))
summary(lm(h$Height ~ h$Father - 1))


summary(lm(Height ~ Father - 1, data=h,
          subset=(Gender == "M")))

mean(h$Father)
mean(h$Mother)
mean(h$Height[h$Gender == "M"])
mean(h$Height[h$Gender == "F"])

mean(h$Father) / mean(h$Mother)
mean(h$Height[h$Gender == "M"]) /
  mean(h$Height[h$Gender == "F"])

h$Height2 <- h$Height
h$Height2[h$Gender == "F"] <-
  h$Height2[h$Gender == "F"] * 1.08
h$MidHeight <- (h$Father + h$Mother*1.08) / 2

summary(lm(Height2 ~ MidHeight - 1, data=h))
summary(lm(Height2 ~ MidHeight, data=h))


plot(h$MidHeight, h$Height2, pch=19, cex=0.3,
      xlab="Parents Avg. Height",
      ylab="Children's Height")
abline(lm(Height2 ~ MidHeight, data=h),
        col="red", lwd=2)

summary(lm(MidHeight ~ Height2, data=h))
abline(-44.47115/0.35750, 1/0.35750,
         col="red", lwd=2)

summary(lm(Height2 ~ Father, data=h))
summary(lm(Father ~ Height2, data=h))


###################################
# Matricies in R
n <- 100
p <- 4
X <- matrix(runif(n*p), ncol=p)
beta <- c(1,2,0,0)
sigma <- 0.5

epsilon <- rnorm(n, sd=sigma)
y <- X %*% beta + epsilon

summary(lm(y ~ X))
coef(lm(y ~ X))

XtXinv <- solve(t(X) %*% X)
betaHat <- XtXinv %*% t(X) %*% y
betaHat # doesn't match the summary, because of the intercept

coef(lm(y ~ X - 1)) # does match

Xnew <- cbind(1,X)
XtXinv <- solve(t(Xnew) %*% Xnew)
betaHat <- XtXinv %*% t(Xnew) %*% y
betaHat # matches original

###################################
# Model Matricies

mf <- model.frame(y ~ X)
head(mf)
class(mf)

mm <- model.matrix(mf)
head(mm)
class(mm)

mr <- model.response(mf)
head(mr)
class(mr)

betaHat <- solve(t(mm) %*% mm) %*% t(mm) %*% mr

# these match, and have the same names!
betaHat
coef(lm(y ~ X))

betaHat - coef(lm(y ~ X)) # well, not exactly the same...

