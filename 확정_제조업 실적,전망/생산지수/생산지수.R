
setwd("C:\\Users\\eunju\\Desktop\\3학년 2학기\\공모전_2020년 통계데이터 분석활용대회\\확정_제조업 실적,전망")
row = read.csv("시도_산업별_광공업생산지수(2018-2020,3level).csv")

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
colnames(data)=c("zezo","date","pro")
str(data)
ggplot(data=data,aes(x=date,y=pro,group=zezo,color=zezo))+
  geom_line()+facet_grid(.~ zezo)

install.packages("dygraphs")
library(dygraphs)
install.packages("xts")
library(xts)
dy = read.csv("dygraph.csv")
dy[,1]<-as.Date(dy[,1])
dyg= xts(dy[,2:74], order.by=dy[,1])
dygraph(dyg)%>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(0,550))

dyg= xts(dy[,2:10], order.by=dy[,1])
dygraph(dyg) %>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(20,200))

dyg= xts(dy[,11:20], order.by=dy[,1])
dygraph(dyg) %>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(20,200))

dyg= xts(dy[,21:30], order.by=dy[,1])
dygraph(dyg) %>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(20,200))

dyg= xts(dy[,31:40], order.by=dy[,1])
dygraph(dyg) %>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(0,200))

dyg= xts(dy[,41:50], order.by=dy[,1])
dygraph(dyg) %>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(0,200))

dyg= xts(dy[,51:60], order.by=dy[,1])
dygraph(dyg) %>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(0,200))

dyg= xts(dy[,61:74], order.by=dy[,1])
dygraph(dyg) %>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(0,200))



dy2 = read.csv("dygraph2.csv")
dy2[,1]<-as.Date(dy2[,1])
dyg2= xts(dy2[,3:26], order.by=dy2[,1])
dygraph(dyg2) %>% 
  dyLegend(show = "onmouseover") %>% 
  dyAxis("y",valueRange = c(0,200))

dyg2= xts(dy2[,2:10], order.by=dy2[,1])
dygraph(dyg2) %>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(0,200))

dyg2= xts(dy2[,11:20], order.by=dy2[,1])
dygraph(dyg2) %>% 
  dyLegend(show = "never") %>% 
  dyAxis("y",valueRange = c(40,161))
dyg2= xts(dy2[,21:26], order.by=dy2[,1])
dygraph(dyg2) %>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(0,200))

dyg2= xts(dy2[,c(4,5,6,7,8,14,17,23,24)], order.by=dy2[,1])
dygraph(dyg2) %>% dyRangeSelector()
