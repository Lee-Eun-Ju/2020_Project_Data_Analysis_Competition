
setwd("C:\\Users\\eunju\\Desktop\\3학년 2학기\\공모전_2020년 통계데이터 분석활용대회\\확정_그래프")
data = read.csv("data.csv")
data[,1]<-as.Date(data[,1])

row = read.csv("row.csv")
##zezo
zezo= data.frame()
for(i in 2:76){
  z= data.frame(rep(row[i,2],31))
  zezo= rbind(zezo, z)
}
str(zezo)

##date
date = colnames(row)
date = date[3:33]
date = rep(date, 75)
str(date)

install.packages("stringr")
library(stringr)
date= str_replace_all(date, "X", "")
date<-as.Date(date,format="%Y.%m.%d")

##produce
data= read.csv("생산지수_data.csv",header=FALSE)
pro= data.frame()
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    k = data.frame(data[i,j])
    pro = dplyr::bind_rows(pro, k)
  }
}
str(pro)


##완성
library("ggplot2")
data=data.frame(zezo,date,pro)
colnames(data)=c("zezo","date","생산지수")
str(data)
ggplot(data=data,aes(x=date,y=pro,group=zezo,color=zezo))+
  geom_line()+facet_grid(.~ zezo)

library("dplyr")
install.packages("hrbrthemes")
library("hrbrthemes")
#담배제조업
data %>%
  mutate( highlight=ifelse(zezo=="담배 제조업", "담배 제조업", "Other")) %>%
  ggplot( aes(x=date, y=생산지수, group=zezo, color=highlight, size=highlight)) +
  geom_line()+
  coord_cartesian(ylim = c(0,200))+
  scale_color_manual(values = c("lightgrey","#69b3a2")) +
  scale_size_manual(values=c(0.2,1.5))+
  theme(legend.position="none") +
  theme_ipsum()
#의료용 기기
data %>%
  mutate( highlight=ifelse(zezo=="의료용 기기 제조업", "의료용 기기 제조업", "Other")) %>%
  ggplot( aes(x=date, y=생산지수, group=zezo, color=highlight, size=highlight)) +
  geom_line()+
  coord_cartesian(ylim = c(0,200))+
  scale_color_manual(values = c("lightgrey","#69b3a2")) +
  scale_size_manual(values=c(0.2,1.5))+
  theme(legend.position="none") +
  theme_ipsum()
#선박
data %>%
  mutate( highlight=ifelse(zezo=="선박 및 보트 건조업", "선박 및 보트 건조업", "Other")) %>%
  ggplot( aes(x=date, y=생산지수, group=zezo, color=highlight, size=highlight)) +
  geom_line()+
  coord_cartesian(ylim = c(0,200))+
  scale_color_manual(values = c("lightgrey","#69b3a2")) +
  scale_size_manual(values=c(0.2,1.5))+
  theme(legend.position="none") +
  theme_ipsum()
#항공기
data %>%
  mutate( highlight=ifelse(zezo=="항공기,우주선 및 부품 제조업", "항공기,우주선 및 부품 제조업", "Other")) %>%
  ggplot( aes(x=date, y=생산지수, group=zezo, color=highlight, size=highlight)) +
  geom_line()+
  coord_cartesian(ylim = c(0,200))+
  scale_color_manual(values = c("lightgrey","#69b3a2")) +
  scale_size_manual(values=c(0.2,1.5))+
  theme(legend.position="none") +
  theme_ipsum()


    