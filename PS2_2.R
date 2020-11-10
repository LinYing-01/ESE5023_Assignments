library(tidyr)
library(dplyr)
library(ggplot2)
wind=read.csv(file="2281305.csv",header = T)
wind.tbl<-as_tibble(wind)
wind.tbl %>% 
  select(DATE,WND) %>% 
  filter(substr(WND,1,3)!="999") %>%
  filter(substr(WND,9,12)!="9999") %>%
  filter(substr(WND,5,5)=="1") %>% 
  filter(substr(WND,7,7)=="N") %>% 
  filter(substr(WND,14,14)=="1")%>%
  mutate(speed_rate=as.numeric(substr(WND,9,12))) %>% 
# special notice here: the SCALING FACTOR is 10
# so using the original data is not the true wind speed
# you should multiply 0.1  （1/10）
  mutate(date_month=as.numeric(paste(substr(DATE,1,4),substr(DATE,6,7),sep = ""))) %>% 
  select(date_month,speed_rate) %>% 
  group_by(date_month) %>% 
  summarize(monthly_mean=mean(speed_rate,na.rm=T)) %>% 
  ggplot(aes(x=date_month,y=monthly_mean))+
  geom_line()
