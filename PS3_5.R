Data <- read.csv("The Big Bang Theory.csv", header = T)
Velocity <- Data$Velocity
Distance <- Data$Distance
Distance <- Distance* 1e6 * 1e12 * 30.9
#5.1
plot(Velocity, Distance, xlab = "Velocity(km per second)", ylab = "Distance(megaparsec)")
#散点分布在某一条线两边，可以明显看出这一趋势

#5.2
model <- lm(Distance~Velocity)
abline(model)
summary(model)

#5.3
#1.如果理论正确，刚开始衰退速度小，没有经过时间的积累，所以距离为零，那么，截距为零；
#2.距离等于时间乘以速度，那么宇宙的年龄即为斜率。
model2 <- lm(Distance~Velocity-1)
abline(model2,col = 'red')
summary(model2)
#斜率即为年龄

#5.4
#由图知，画出来的图宇宙伊始距离不为零肯定不对，但是宇宙的年龄是等于斜率且不变的，所
#以通过使截距（初始距离）变为零以后看斜率。通过在 lm（ ）函数里把速度根据图上的截距
#误差设为-1，使截距为 0，得到斜率如图即为年龄。更正距离测量误差后，结果更精确。
# good work
