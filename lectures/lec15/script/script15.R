####################
# Columbia images
library(jpeg)
pm <- read.csv("photoMetaData.csv", as.is=TRUE)

system(paste0("open columbiaImages/",pm$name[94]))
table(pm$category)

pm <- pm[pm$category != "artificial",]
pm <- pm[pm$category != "natural",]
n <- nrow(pm)

y <- rep(0, nrow(pm))
y[grep("indoor", pm$category)] <- 1

set.seed(1)
tFlag <- as.numeric(runif(n) > 0.5)

X <- matrix(0,nrow=n,ncol=3)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  X[j,] <- apply(img, 3, median)
  print(j)
}

y1 <- y[tFlag == 1]
y2 <- y[tFlag == 0]
X1 <- X[tFlag == 1,]
X2 <- X[tFlag == 0,]

betaHat <- qr.solve(crossprod(X1),crossprod(X1,y1))

y2Hat <- X2 %*% betaHat
table(y2Hat > 0.5, y2)

mean((y2Hat > 0.5) == y2)
mean(0 == y2)


X <- matrix(0,nrow=n,ncol=3)
for (j in 1:n) {
  img <- readJPEG(paste0("columbiaImages/",pm$name[j]))
  X[j,] <- apply(img, 3, median)
  print(j)
}




