library(tydyr)
library(dplyr)
library(ggplot2)
EQ_D<-read.csv(file="signif.txt",sep = "\t",header = T)
Sig_Eqs=as_tibble(EQ_D)

#1.2
Sig_Eqs %>% 
  select(COUNTRY,TOTAL_DEATHS) %>% 
  group_by(COUNTRY) %>% 
  summarize(EachC_total_death=sum(TOTAL_DEATHS,na.rm = T)) %>% 
  arrange(desc(EachC_total_death))
# @MingYANG recommended:
# the pipe operation %>% means to send the data to next step
# that also means this operation will not be saved if
# Highly recommended that using "<-"in line 8 to save this procedure.
# just like:  "Total_Death <- Sig_Eqs %>%"
# this is nothing with your score but just a recommendation
# the end

#1.3
Sig_Eqs %>% 
  select(YEAR,EQ_PRIMARY) %>%
  filter(EQ_PRIMARY>6.0) %>% 
  group_by(YEAR) %>% 
  summarize(total_per=sum(EQ_PRIMARY,na.rm = T)) %>% 
  ggplot((aes(x=YEAR,y=total_per)))+
  geom_line()
#随着年份增长，呈现显著增加的趋势，我认为主要原因是因为科技越来越发
#达，记录到的地震的次数和情况也越来越全面和详细，以前主要靠人为记录
#较多，也不排除人类对自然的改造对地质构造产生了影响，使得局部地区构
#造断裂带较以往相对活跃，但我认为这并不是使地震记录次数显著增长的主
#要原因

#1.4
#返回该国有史以来最大地震的日期
CountEq_LargestEq<-function(country){
  Largest_EQ<-Sig_Eqs %>% 
    filter(COUNTRY==country) %>% 
    mutate(date=paste(YEAR,MONTH,DAY,sep = '-')) %>% 
    select(date,EQ_PRIMARY) %>% 
    summarise(Largest_EQ=n(),date_max=
                date[which(EQ_PRIMARY==max(EQ_PRIMARY))]) %>% 
List(Largest_EQ,date_max)
  return(date_max)
}
#返回该国自公元前2150年以来的地震总数
CountEq_LargestEq<-function(country){
  Count_EQ<-Sig_Eqs %>% 
    filter(COUNTRY==country) %>% 
    mutate(date=paste(YEAR,MONTH,DAY,sep = '-')) %>% 
    select(date,EQ_PRIMARY) %>% 
    summarise(Count_EQ=n(),country=
                country[which(EQ_PRIMARY==max(EQ_PRIMARY))]) %>% 
List(Count_EQ,country)
  return(Count_EQ)
}
#good work
