# Create dataset for the problem set
x <- readRDS("airline2007_clean.Rds")
topArr <- names(sort(table(x$dest),decreasing=TRUE)[1:25])
x <- x[x$dest %in% topArr,]
x <- x[sample(1:nrow(x),nrow(x)*0.1),]
x <- x[order(x$dTime),]
rownames(x) <- NULL
x$group <- "I"
x$group[as.numeric(format(x$dTime, "%j")) > 365/2] <- "II"
saveRDS(x, "airline2007_pset04.Rds")







