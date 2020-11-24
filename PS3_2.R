library(tidyr)
library(dplyr)
library(ggplot2)

Tyrannosaurus <- read.csv("Tyrannosaurus.csv", header = T, encoding = "UTF-8")
#数据导入excel，另存为逗号分隔形式文件，并读取
Tyrannosaurus <- as_tibble(Tyrannosaurus)

#使用one-way ANOVA test
anova_one_way <- aov(Oxygen.isotopic.composition~factor(Bone), data = Tyrannosaurus)
summary(anova_one_way)
#P值低于0.05，数据之间存在统计差异，两者之间存在差异

ggplot(Tyrannosaurus, aes(x = Bone, y = Oxygen.isotopic.composition , fill = Bone)) +
  geom_boxplot() +
  theme_classic()
#从图形中也可看出两者存在差异
#以上可以证明雷克斯暴龙是温血动物

# MingYANG noticed:
# wrong conclusion!
