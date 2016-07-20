x <- runif(25)*4
y <- 0.2 + 0.5 * x + rnorm(25, sd=0.5)

pdf("img/fig01.pdf", height=5, width=8)
par(mar=c(1,1,1,1))
plot(0,0,col="white", xlim=c(-1,4), ylim=c(-1,4), axes=FALSE, xlab="", ylab="")
abline(v=0)
abline(h=0)
text(4,-0.2,"x")
text(-0.15,4,"y")
points(x,y,pch=19,cex=0.8)
dev.off()

pdf("img/fig02.pdf", height=5, width=8)
par(mar=c(1,1,1,1))
plot(0,0,col="white", xlim=c(-1,4), ylim=c(-1,4), axes=FALSE, xlab="", ylab="")
abline(v=0)
abline(h=0)
text(4,-0.2,"x")
text(-0.15,4,"y")
points(x,y,pch=19,cex=0.8)
abline(lm(y~x),col="blue",lwd=2)
dev.off()

pdf("img/fig03.pdf", height=5, width=8)
par(mar=c(1,1,1,1))
plot(0,0,col="white", xlim=c(-1,4), ylim=c(-1,4), axes=FALSE, xlab="", ylab="")
abline(v=0)
abline(h=0)
text(4,-0.2,"x")
text(-0.15,4,"y")
points(x,y,pch=19,cex=0.8)
abline(lm(y~x),col="blue",lwd=2)
resid <- lm(y~x)$resid
segments(x,y,x,y-resid, col="red")
dev.off()

set.seed(0)
x <- seq(0,1,by=0.001)
n <- length(x)
z <- sort(c(0,runif(10),1))
ymean <- approxfun(z, rnorm(length(z)))(x)
y <- ymean + rnorm(n,sd=0.5)

pdf("img/fig04.pdf", height=5, width=8)
par(mar=c(1,1,1,1))
plot(0,0,col="white", xlim=c(-1/4,1), ylim=c(-4,4), axes=FALSE, xlab="", ylab="")
abline(v=0)
abline(h=0)
points(x,y,pch=19,cex=0.8)
dev.off()

pdf("img/fig05.pdf", height=5, width=8)
par(mar=c(1,1,1,1))
plot(0,0,col="white", xlim=c(-1/4,1), ylim=c(-4,4), axes=FALSE, xlab="", ylab="")
abline(v=0)
abline(h=0)
points(x,y,pch=19,cex=0.8)
lines(x,ymean,col="cyan",lwd=2)
dev.off()

for (i in 1:5) {
  p <- c(1,2,3,4,20)[i]
  pdf(sprintf("img/fig%02d.pdf",i+5), height=5, width=8)
  par(mar=c(1,1,1,1))
  plot(0,0,col="white", xlim=c(-1/4,1), ylim=c(-4,4), axes=FALSE, xlab="", ylab="")
  abline(v=0)
  abline(h=0)
  points(x,y,pch=19,cex=0.8)
  lines(x,ymean,col="cyan",lwd=2)
  X <- matrix(0, ncol=p*2, nrow=n)
  for (j in 1:p) {
    X[,j] = sin(j * x * (2*pi))
    X[,j+p] = cos(j * x * (2*pi))
  }
  out <- predict(lm(y ~ ., data=data.frame(y=y, X)))
  lines(x,out,col="red",lwd=4)
  dev.off()
}


