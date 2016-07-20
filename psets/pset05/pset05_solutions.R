############
# Code for Part I

###########
# 2.

X <- matrix(c(10^9, -1, -1, 10^(-5)), 2, 2)
beta <- c(1,1)
y <- X %*% beta

SVD <- svd(X)
betaHatSVD <- SVD$v %*% diag(1/SVD$d) %*% t(SVD$u) %*% y
betaHatSVD
#      [,1]
# [1,]    1
# [2,]    1

# Can see that this returns the 'correct' solution; reason is
# that it works on the raw matrix X rather than the Gram matrix
# XtX. This avoids the problem of squaring the condition number
# from squaring the matrix X, and mimics QR solution.



############
# Code for Part III

############
# 1.
img <- jpeg::readJPEG("columbiaImages/DSC_1739.jpg")
matBW <- (img[,,1] + img[,,2] + img[,,3]) / 3

SVD <- svd(matBW)

# (a) log of singular values
hist(log(SVD$d))

# (b) new matrix with just 60 singular values
newSigma <- SVD$d
newSigma[61:length(newSigma)] <- 0
newMat <- SVD$u %*% diag(newSigma) %*% t(SVD$v)
jpeg::writeJPEG(newMat, "output_III_1_b.jpg")

# (c) the image has been reconstructed fairly well; all
#      major features remain, though clearly there is some
#      degradation in the overall image quality

############
# 2.

# (a) images
newSigma <- SVD$d

newSigma[51:length(newSigma)] <- 0
newMat <- SVD$u %*% diag(newSigma) %*% t(SVD$v)
jpeg::writeJPEG(newMat, "output_III_2_b_50.jpg")

newSigma[41:length(newSigma)] <- 0
newMat <- SVD$u %*% diag(newSigma) %*% t(SVD$v)
jpeg::writeJPEG(newMat, "output_III_2_b_40.jpg")

newSigma[31:length(newSigma)] <- 0
newMat <- SVD$u %*% diag(newSigma) %*% t(SVD$v)
jpeg::writeJPEG(newMat, "output_III_2_b_30.jpg")

newSigma[21:length(newSigma)] <- 0
newMat <- SVD$u %*% diag(newSigma) %*% t(SVD$v)
jpeg::writeJPEG(newMat, "output_III_2_b_20.jpg")

newSigma[11:length(newSigma)] <- 0
newMat <- SVD$u %*% diag(newSigma) %*% t(SVD$v)
jpeg::writeJPEG(newMat, "output_III_2_b_10.jpg")

############
# 3.

# (a) scatter plot
img <- jpeg::readJPEG("columbiaImages/DSC_1734.jpg")
mat <- matrix(img, ncol=3)
plot(mat[,1],mat[,2],pch=".")
plot(mat[,2],mat[,3],pch=".")

# (b) pca
SVD <- svd(mat)
T <- SVD$u %*% diag(SVD$d)

# scale 0->1
T <- t((t(T) - apply(T,2,min)) / (apply(T,2,max) - apply(T,2,min)))
apply(T,2,range) # check it

# notice the size of these images!!!
jpeg::writeJPEG(matrix(T[,1],nrow=nrow(img),ncol=ncol(img)), "output_III_3_b_pc1.jpg")
jpeg::writeJPEG(matrix(T[,2],nrow=nrow(img),ncol=ncol(img)), "output_III_3_b_pc2.jpg")
jpeg::writeJPEG(matrix(T[,3],nrow=nrow(img),ncol=ncol(img)), "output_III_3_b_pc3.jpg")

# (c) first component is brightness (mostly); second is the amount of red/yellow in the
#     image as only the red traffic bars, brick buildings is background, and yellow Chinese
#     symbols have standout values; third has small values for green and larger values for
#     very red, mostly differentiating the green roof from everything else.



