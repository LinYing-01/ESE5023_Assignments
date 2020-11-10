library(tidyr)
library(dplyr)
library(ggplot2)
sz <- read.csv(file="water.csv",header = T,encoding = "UTF-8")
sz_tbl<-as_tibble(sz)
sz_tbl %>%
  filter(Salinity..psu.>0)%>%
  mutate(date=as.Date(Dates)) %>%
  mutate(S=as.double(Salinity..psu.)) %>% 
  select(date,S)%>%
  ggplot(aes(x=date,y=S))+
  geom_line()
# good work 

 
 
