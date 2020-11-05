library(MASS)
library(leaps)
library(tidyr)
data(cpus)
str(cpus)

index <- sample(nrow(cpus),nrow(cpus)*0.80)
train <- cpus[index,]
test  <- cpus[-index,]

#6.1
t<- regsubsets(perf~syct+mmin+mmax+cach+chmin+chmax, 
                           data=train, nbest= 2)
plot(t, scale="bic")
summary(t)

#6.2
model <- lm(perf~syct+mmin+mmax+cach+chmax, data = train)
summary(model)
predict <- predict(model, test)
a <- seq(1,42,1)
test_new <- as.data.frame(test)
plot(predict, test_new$perf, xlab = 'Predict', ylab = 'True')
model2 <- lm(pre~test_new$perf)
summary(model2)
abline(model2)
sqrt(mean((test_new$perf - pre)^2)/42)
