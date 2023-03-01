##시계열
##2018 grdp 제외

setwd("C:\\Users\\eunju\\Desktop\\3학년 2학기\\공모전_2020년 통계데이터 분석활용대회\\자치구별 GRDP")
grdp<- read.csv("GRDP_요인.csv",header=TRUE)
head(grdp)

grdp<- grdp[-c(4,5)]
grdp<- grdp[-which(grdp$"기간"==2018),]
grdp
summary(grdp)

lm_g = lm(지역내총생산~.-기간-자치구, data=grdp)
summary(lm_g)
plot(lm_g)

