#7.1
Keeling_Data<-read.csv(file="water.csv",header=T,encoding="UTF-8")
#问了同学关于read.csv()中关于中文编码的问题
Day<- Keeling_Data$Dates
S  <- Keeling_Data$Salinity..psu.
S[which(S <0)]  <- NA
#7.2
Day<-as.Date(Day)
plot(Day,S,type="l")
#问了同学画图显示不出来的问题，因为数据格式没有转换
#7.3
S<-as.numeric(S)
max(S,na.rm = T)
min(S,na.rm = T)
mean(S,na.rm = T)
median(S,na.rm = T)
range(S,na.rm = T)
