
setwd("C:\\Users\\eunju\\Desktop\\3학년 2학기\\공모전_2020년 통계데이터 분석활용대회\\확정_그래프")
data = read.csv("data.csv")
data[,1]<-as.Date(data[,1])

##담배제조업
library(ggplot2)
smoke = data[,c(1,12)]

ggplot(data, aes(x=smoke[,1], y=smoke[,2])) +
  geom_segment( aes(x=smoke[,1], xend=smoke[,1], y=50, yend=smoke[,2]), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("담배생산지수")


##의류산업
#dygraph
dy = read.csv("data.csv")
dy[,1]<-as.Date(dy[,1])

dyg=xts(dy[,c(13,14,15,16,17,18,19,20,21,22)],order.by=dy[,1])
dygraph(dyg) %>% 
  dyLegend(show = "follow") %>% 
  dyAxis("y",valueRange = c(0,200))

#ggplot
cloth = data[,c(1,13,14,15,16,17,18,19,20,21,22)]
plot(cloth[,2]~doctor[,1] ,type="b" , xlab="year" , 
     ylab="생산지수" , col=rgb(0.2,0.4,0.1,0.7)  , ylim=c(0,200))
lines(cloth[,3]~cloth[,1] , col=rgb(0.1,0.4,0.1,0.7)   , type="b" )
lines(cloth[,4]~cloth[,1] , col=rgb(0.3,0.2,0.1,0.7)   , type="b" )
lines(cloth[,5]~cloth[,1] , col=rgb(0.4,0.4,0.6,0.7), type="b" )
lines(cloth[,6]~cloth[,1] , col=rgb(0.5,0.4,0.1,0.9) , type="b" )
lines(cloth[,7]~cloth[,1] , col=rgb(0.6,0.4,0.1,0.3)   , type="b" )
lines(cloth[,8]~cloth[,1] , col=rgb(0.7,0.4,0.2,0.7)  , type="b" )
lines(cloth[,9]~cloth[,1] , col=rgb(0.8,0.9,0.1,0.7)  , type="b" )
lines(cloth[,10]~cloth[,1] , col=rgb(0.9,0.4,0.1,0.7)   , type="b" )
lines(cloth[,11]~cloth[,1] , col=rgb(1,0.4,0.8,0.7)  , type="b" )

#radarchart
cloth2 = cloth[which(cloth$산업별=="2018-03-01"),]
cloth2 = rbind(cloth2, cloth[which(cloth$산업별=="2019-03-01"),])
cloth2 = rbind(cloth2, cloth[which(cloth$산업별=="2020-03-01"),])
rownames(cloth2) = as.factor(cloth2$산업별)
cloth2 = cloth2[,-1]

library(RColorBrewer)
coul <- brewer.pal(3, "BuPu")
colors_border <- coul
library(scales)
colors_in <- alpha(coul,0.3)

library(fmsb)
radarchart( cloth2  , axistype=0 , maxmin=F,
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(cloth2), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


##의료산업
doctor = data[,c(1,36,53)]
plot(doctor[,2]~doctor[,1], type="b" , bty="l" , xlab="year" , 
     ylab="생산지수" , col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17, ylim=c(90,160) )
lines(doctor[,3]~doctor[,1] , col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )

legend("topleft", 
       legend = c("의약품 제조업", "의료용 기기 제조업"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


##선박과 항공기(운송수단)
vehicle = data[,c(1,67,69)]
plot(vehicle[,2]~vehicle[,1], type="b" , bty="l" , xlab="year" , 
     ylab="생산지수" , col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17, ylim=c(40,140)) 
lines(vehicle[,3]~vehicle[,1] , col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )

legend("topleft", 
       legend = c("선박 및 보트 건조업", "항공기 우주선 및 부품 제조업"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
