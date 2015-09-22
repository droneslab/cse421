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








