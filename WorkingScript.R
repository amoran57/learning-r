oj <- read.csv("oj.csv")
head(oj)
oj$brand
levels(oj$brand)
oj[1]
oj[1:10,2]
levels(oj$price)
values(oj$price)

nrow(oj)
summary(oj)
hist (oj$price)
plot (oj$price ~ oj$brand)
oj[1,]
plot (sales~brand, data=oj)
plot (sales~brand, data=oj, log="y")
plot(sales~price, data=oj, log="y", col=oj$brand)
legend("topright", fill =1:3, legend = levels(oj$brand))

fit <- glm(log(sales)~ .,data=oj)
summary(fit)

#bootstrapping the distribution
B <- 10000
mub <- c()
for (b in 1:B) {
  samp_b <- sample.int(nrow(oj),replace = TRUE)
  mub <- c(mub, mean(log(oj$sales[samp_b])))
}

#now we have B=10000 mean sales estimates, each drawn on a full-size sample
plot(mub)
#this is our distribution:
hist(mub,freq=FALSE)
#and to overlay the standard normal distribution:
xbar <- mean(log(oj$sales))
xbse <-  sd(log(oj$sales))/sqrt(nrow(oj))
xx <- seq(9,10,length=1000)
lines(xx, dnorm(xx, xbar, xbse), col="royalblue", lwd=1.5)
sort(samp_b)[1:10]
