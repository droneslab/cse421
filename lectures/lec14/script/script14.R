# Figures
vals <- seq(0,1,0.001)
pdf("img/fig01.pdf", height=4, width=8)
plot(vals, (vals-0.5)^2, type="l", xlab="parameter p", ylab="risk")
dev.off()
pdf("img/fig02.pdf", height=4, width=8)
plot(vals, vals - vals^2, type="l", xlab="parameter p", ylab="risk")
dev.off()
pdf("img/fig03.pdf", height=4, width=8)
plot(vals, rep(1/16,length(vals)), type="l", xlab="parameter p", ylab="risk")
dev.off()

##### SVD simulations
A <- matrix(1:6,ncol=3)
A

SVD <- svd(A, nu=2, nv=3)
Sigma <- cbind(diag(SVD$d),0)
U <- SVD$u
V <- SVD$v
A - U %*% Sigma %*% t(V)

N <- 1e4
p <- 3
unitBall <- matrix(runif(N * p, -1, 1), nrow=3)
unitBall <- unitBall[,apply(unitBall^2, 2, sum) < 1]
unitBall[,1:5]

projUnitBall <- t(A %*% unitBall)
head(projUnitBall)
plot(projUnitBall,pch=".")

pdf("img/fig04.pdf", height=7, width=7)
plot(projUnitBall,pch=".")
dev.off()

A %*% V

v1 <- (A %*% V)[,1]
v2 <- (A %*% V)[,2]
arrows(0,0,v1[1],v1[2],col="red",lwd=2)
arrows(0,0,v2[1],v2[2],col="green",lwd=2)

pdf("img/fig05.pdf", height=7, width=7)
plot(projUnitBall,pch=".")
v1 <- (A %*% V)[,1]
v2 <- (A %*% V)[,2]
arrows(0,0,v1[1],v1[2],col="red",lwd=2)
arrows(0,0,v2[1],v2[2],col="green",lwd=2)
dev.off()

####################
# Columbia images
library(jpeg)
pm <- read.csv("photoMetaData.csv")

table(pm$location)
table(pm$photographer)
table(pm$category)

system(paste0("open columbiaImages/",pm$name[300]))

img <- readJPEG(paste0("columbiaImages/",pm$name[300]))
class(img)
dim(img)

options(digits=4)



