####################
# Now, F-test
set.seed(1)
n <- 1000
p <- 2
X <- matrix(rnorm(n*p), ncol=p)
beta <- c(1,0)
epsilon <- rnorm(n,sd=3)

alpha <- 0.9
X[,2] <- alpha * X[,1] + (1-alpha) * X[,2]
y <- X %*% beta + epsilon

Z <- cbind(X[,1] + X[,2], X[,1] - X[,2])
out1 <- lm(y ~ X)
out2 <- lm(y ~ Z)
out3 <- lm(y ~ Z[,1])
out4 <- lm(y ~ 1)

anova(out4,out3,out2,out1)

plot(0,0,col="white",ylim=c(-5,8),xlim=c(-5,8),
      xlab="coef 1", ylab="coef 2")
points(1,0,pch=19,cex=1,col="red")
mixtools::ellipse(coef(out1)[2:3],
     vcov(out1)[2:3,2:3], alpha=0.05)

# Iterate over several random samples
plot(0,0,col="white",ylim=c(-5,8),xlim=c(-5,8),
      xlab="coef 1", ylab="coef 2")
points(1,0,pch=19,cex=1,col="red")

for (i in 1:5) {
  epsilon <- rnorm(n,sd=3)
  y <- X %*% beta + epsilon
  out1 <- lm(y ~ X)
  mixtools::ellipse(coef(out1)[2:3],
      vcov(out1)[2:3,2:3])
}
points(1,0,pch=19,cex=1,col="red")








###################################
# Galton's height data
#urlBase <- "http://euler.stat.yale.edu/~tba3/stat612/lectures/lec03/data/"
urlBase <- "~/files/stat612/lectures/lec03/data/"
h <- read.csv(paste0(urlBase, "galton_heights.csv"))

################# Now, model frames again!
mf <- model.frame(Height ~ Father + Gender, data=h)
mm <- model.matrix.lm(mf)
mr <- model.response(mf)

head(mm)

mf <- model.frame(Height ~ Father:Gender, data=h)
mm <- model.matrix.lm(mf)
mr <- model.response(mf)

head(mm)

mf <- model.frame(Height ~ Father*Gender, data=h)
mm <- model.matrix.lm(mf)
mr <- model.response(mf)

head(mm)

## Redo model frame!
mf <- model.frame(Height ~ Father + Gender, data=h)
mm <- model.matrix.lm(mf)
mr <- model.response(mf)

head(mm)
colnames(mm)[1] <- "GenderF"

mm[,1] <- 1 - mm[,3]
head(mm)

solve(t(mm) %*% mm) %*% t(mm) %*% mr








