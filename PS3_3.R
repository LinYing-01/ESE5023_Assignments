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
  