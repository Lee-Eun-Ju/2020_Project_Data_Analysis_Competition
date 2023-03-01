
setwd("C:\\Users\\eunju\\Desktop\\3학년 2학기\\공모전_2020년 통계데이터 분석활용대회\\확정_제조업 실적,전망\\생산능력지수")
data= read.csv("생산능력지수.csv")

install.packages("dygraphs")
library(dygraphs)
install.packages("xts")
library(xts)

data[,1]<-as.Date(data[,1])
dyg= xts(data[,2:22], order.by=data[,1])
dygraph(dyg)%>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(40,161))
