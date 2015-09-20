###################################
# Galton's height data
#urlBase <- "http://euler.stat.yale.edu/~tba3/stat612/lectures/lec03/data/"
urlBase <- "~/files/stat612/lectures/lec03/data/"
h <- read.csv(paste0(urlBase, "galton_heights.csv"))

# Use gender offset
out <- lm(Height ~ Father + Gender, data=h)
coef(out)

pch <- rep(19, nrow(h))
pch[h$Gender == "M"] <- 1
col <- rep("red", nrow(h))
col[h$Gender == "M"] <- "blue"

plot(h$Father, h$Height, pch=pch, cex=0.5, col=col)
abline(coef(out)[1], coef(out)[2], lty="dashed", col="red")
abline(coef(out)[1] + coef(out)[3],
       coef(out)[2], lty="dashed", col="blue")

text(77, 72, "Sons", col="blue")
text(77, 72 - coef(out)[3], "Daughters", col="red")

# Regression table of the results
summary(out)

# Now use varying slopes
out <- lm(Height ~ Father:Gender, data=h)
coef(out)

plot(h$Father, h$Height, pch=pch, cex=0.5, col=col)
abline(coef(out)[1], coef(out)[2], lty="dashed", col="red")
abline(coef(out)[1], coef(out)[3], lty="dashed", col="blue")

summary(out)

# Finally, varying height and slope
out <- lm(Height ~ Father*Gender, data=h)
coef(out)

out2 <- lm(Height ~ Father, data=h, subset=Gender=='F')
coef(out2) # compare to coef(out)

summary(out) # what's odd here!?

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








