#数据简单，可直接输入
Elevation <- c(180,305,381,488,549,640,762,883)/1000
Temp <- c(13.3,12.2,13.3,10.0,8.3,9.4,8.3,7.2)

plot(Elevation, Temp, xlab = 'Elevation/km', ylab = 'Temperature/degrees ℃')
model <- lm(Temp~Elevation)
coef(model)
summary(model)
abline(model)
