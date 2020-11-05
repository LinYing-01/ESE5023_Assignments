library(dplyr)
library(MASS)
library(leaps)
library(tidyr)
library(ggplot2)
#7.1
data1<-c(9.95,9.33,9.49,9.00,10.09,9.15)
data2<-c(9.52,9.33,9.16,9.37,9.11,9.27)
t.test(data1,data2)

#7.2
ggplot(InsectSprays, aes(x = spray, y = count, fill = spray)) +
  geom_boxplot() +
  theme_classic()
anova_one_way <- aov(count~factor(spray), data = InsectSprays)
summary(anova_one_way)

#7.3
data(Boston)
str(Boston)
sample_index <- sample(nrow(Boston),nrow(Boston)*0.90)
Boston_train <- Boston[sample_index,]
Boston_test  <- Boston[-sample_index,]
model_1 <- lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat, data=Boston_train)
model_1 <- lm(medv ~ ., data=Boston_train)
summary(model_1)