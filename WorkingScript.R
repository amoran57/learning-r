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
B <- 1000
mub <- c()
for (b in 1:B) {
  samp_b <- sample.int(nrow(oj),replace = TRUE)
  lsales <- log(oj$sales[samp_b])
  mulsales <- mean(lsales)
  mub <- c(mub, mulsales)
}

#now we have B=1000 mean sales estimates, each drawn on a full-size sample
sd(mub)
plot(sales~log(sales), data=oj)
