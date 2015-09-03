# Figure normal_density.pdf
pdf("img/normal_density.pdf", height=7.5, width=9)
SD <- c(0.25, 0.5, 1, 2)
xset <- seq(-4,4,by=0.001)
plot(0,0,col="white", xlim=range(xset), ylim=c(0,2), xlab="position", ylab="density")
for (i in 1:length(SD)) {
  lines(xset, dnorm(xset, sd=SD[i]))
}
text(0, dnorm(0, sd=SD[1])+0.07, expression(paste(sigma," = ", 0.25)))
text(0, dnorm(0, sd=SD[2])+0.07, expression(paste(sigma," = ", 0.5)))
text(0, dnorm(0, sd=SD[3])+0.07, expression(paste(sigma," = ", 1)))
text(0, dnorm(0, sd=SD[4])+0.07, expression(paste(sigma," = ", 2)))
dev.off()


# Simulation of y = x b + error
x <- c(rep(1,10),100)
y <- x + rnorm(11)