set.seed(12)
n <- 100
gender <- sample(c(rep("F", n/2), rep("M", n/2)))
height <- rnorm(n, mean=161, sd=6) + (gender == "M")*10
bmi <- runif(n,18,28) + (gender == "M")*2
weight <- (height / 100)^2 * bmi

height <- round(height)
weight <- round(weight)

PCH <- rep(19, n)
PCH[gender == "M"] <- 22
plot(height,weight,pch=PCH, xlab="height (cm)", ylab="weight (kg)")
legend("topleft", c("females","males"), pch=c(19,22))

df <- data.frame(weight=weight, height=height, gender=gender)

summary(out1 <- lm(weight ~ height, data=df))
summary(out2 <- lm(weight ~ height + gender, data=df))
summary(out3 <- lm(weight ~ height:gender, data=df))
summary(out4 <- lm(weight ~ height*gender, data=df))


out <- lm(weight ~ height*gender)
plot(out$fitted, out$resid)
abline(h=0, lty="dashed")

########################################
pdf("fig01.pdf", height=6, width=8)
par(mfrow=c(2,2))
x <- runif(100)
y <- x + x*rnorm(100)
out <- lm(y ~ x)
plot(out$fitted, out$resid, main="(a)", xlab="Py (fitted values)", ylab="My (residuals)")
abline(h=0, lty="dashed")

x <- runif(100, -1, 1)
y <- x^2 + rnorm(100,sd=0.1)
out <- lm(y ~ x)
plot(out$fitted, out$resid, main="(b)", xlab="Py (fitted values)", ylab="My (residuals)")
abline(h=0, lty="dashed")

set.seed(1)
x <- runif(100, -1, 1)
y <- x + rcauchy(100)
out <- lm(y ~ x)
plot(out$fitted, out$resid, main="(c)", xlab="Py (fitted values)", ylab="My (residuals)")
abline(h=0, lty="dashed")

set.seed(1)
x <- runif(100)
x[1] <- 5
y <- x + rnorm(100)
out <- lm(y ~ x)
plot(out$fitted, out$resid, main="(d)", xlab="Py (fitted values)", ylab="My (residuals)")
abline(h=0, lty="dashed")
dev.off()








