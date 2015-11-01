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

#####

