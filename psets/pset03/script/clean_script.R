# Create dataset for the problem set
x <- readRDS("euler.stat.yale.edu/~tba3/lectures/lec08/data/airline2007_clean.Rds")
topArr <- names(sort(table(x$dest),decreasing=TRUE)[1:25])
x <- x[x$dest %in% topArr,]
x$group <- sample(c("I", "II", "III"), nrow(x), replace=TRUE)
saveRDS(x, "airline2007_pset03.Rds")

