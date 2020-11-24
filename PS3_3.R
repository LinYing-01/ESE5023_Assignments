library(tidyr)
library(dplyr)
library(ggplot2)
Zn<-read.csv('Vegetarians and Zinc.csv')
head(Zn)

anova_one_way <- aov(Zinc~Vegetarians, data = Zn)
summary(anova_one_way)

Zn_tbl<-as_tibble(Zn) 
Zn_tbl %>% 
pn <- Zn_tbl %>% 
  select(Vegetarians, Zinc) %>%
  filter(Vegetarians == "Pregnant nonvegetarians") %>% 
  pull(Zinc)
pv<- Zn_tbl %>% 
  select(Vegetarians, Zinc) %>% 
  filter(Vegetarians == "Pregnant vegetarians") %>% 
  pull(Zinc)
npv <- Zn_tbl %>% 
  select(Vegetarians, Zinc) %>% 
  filter(Vegetarians == "Nonpregnant vegetarians") %>% 
  pull(Zinc)
t.test(pn,pv)
t.test(pv,npv)
# MingYANG noticed
# Is there evidence that the means are different for the different bones? Does the dataset support Tyrannosaurus Rex is warm-blooded or not？
# the end
