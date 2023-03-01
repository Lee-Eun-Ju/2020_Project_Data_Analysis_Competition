
setwd("C:\\Users\\eunju\\Desktop\\3학년 2학기\\공모전_2020년 통계데이터 분석활용대회\\3.제조업 변화")
data1= read.csv("제조업_생산능력지수.csv")
data2= read.csv("제조업_가동률지수.csv")

#################?????ɷ?????
library("ggplot2")
colnames(data)
ggplot(data1, aes(data1[,1], data1[,2])) +geom_line()

colnames(data1)<-c("date","B","C","D","E","F","G","H","I","J","K",
                   "L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z",
                   "AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ","AK",
                   "AL","AM","AN","AO","AP","AQ","AR","AS","AT","AU","AV","AW","AX","AY","AZ",
                   "BA","BB","BC","BD","BE","BF","BG","BH","BI","BJ","BK",
                   "BL","BM","BN","BO","BP","BQ","BR","BS","BT","BU","BV","BW","BX","BY","BZ",
                   "CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL")

#theme_set(theme_minimal()) #ggplot2 background annotation ?ּ?ȭ
#ggplot(data1, aes(date, C, group = 1)) + geom_point() + geom_line() + labs(x = "date", y = "��??����??")
#ggplot(data1) + geom_line(aes(x = date, y = B,C,D,E), cex = 0.8, show.legend = T)


ggplot(data1) + 
  geom_line(aes(x = date, y = B, group = 1), color = "#000000") +
  geom_line(aes(x = date, y = C, group = 1), color = "#000099") +
  geom_line(aes(x = date, y = D, group = 1), color = "#0000FF") +
  geom_line(aes(x = date, y = E, group = 1), color = "#003300") +
  geom_line(aes(x = date, y = F, group = 1), color = "#00CC00") +
  geom_line(aes(x = date, y = G, group = 1), color = "#00FFFF") +
  geom_line(aes(x = date, y = H, group = 1), color = "#006666") +
  geom_line(aes(x = date, y = I, group = 1), color = "#0099FF") +
  geom_line(aes(x = date, y = J, group = 1), color = "#333300") +
  geom_line(aes(x = date, y = K, group = 1), color = "#660066") +
  geom_line(aes(x = date, y = L, group = 1), color = "#6666FF") +
  geom_line(aes(x = date, y = M, group = 1), color = "#666633") +
  geom_line(aes(x = date, y = N, group = 1), color = "#6666FF") +
  geom_line(aes(x = date, y = O, group = 1), color = "#999999") +
  geom_line(aes(x = date, y = P, group = 1), color = "#999900") +
  geom_line(aes(x = date, y = Q, group = 1), color = "#9999CC") +
  geom_line(aes(x = date, y = R, group = 1), color = "#99CCCC") +
  geom_line(aes(x = date, y = S, group = 1), color = "#0000FF") +
  geom_line(aes(x = date, y = T, group = 1), color = "#993300") +
  geom_line(aes(x = date, y = U, group = 1), color = "#FFE400") +
  geom_line(aes(x = date, y = V, group = 1), color = "#FFD8D8") +
  geom_line(aes(x = date, y = W, group = 1), color = "#E8D9FF") +
  geom_line(aes(x = date, y = X, group = 1), color = "#FF5E00") +
  geom_line(aes(x = date, y = Y, group = 1), color = "#CEFBC9") +
  geom_line(aes(x = date, y = Z, group = 1), color = "gray") + 
  xlab('date') +ylab('score')
 
  
ggplot(data1) +   
  geom_line(aes(x = date, y = AA, group = 1), color = "pink") +
  geom_line(aes(x = date, y = AB, group = 1), color = "#000000") + #??��
  geom_line(aes(x = date, y = AC, group = 1), color = "#000099") + #????
  geom_line(aes(x = date, y = AD, group = 1), color = "#0000FF") + #??��?Ķ?
  geom_line(aes(x = date, y = AE, group = 1), color = "#003300") + #??��?ʷ?
  geom_line(aes(x = date, y = AF, group = 1), color = "#00CC00") + #???λ?
  geom_line(aes(x = date, y = AG, group = 1), color = "#00FFFF") + #?????ϴ?
  geom_line(aes(x = date, y = AH, group = 1), color = "#006666") + #cjdrornfl??
  geom_line(aes(x = date, y = AI, group = 1), color = "#0099FF") + #?ϴ?
  geom_line(aes(x = date, y = AJ, group = 1), color = "#333300") + #????
  geom_line(aes(x = date, y = AK, group = 1), color = "#660066") + #????
  geom_line(aes(x = date, y = AL, group = 1), color = "#6666FF") + #?ڹ?Ʈ????
  geom_line(aes(x = date, y = AM, group = 1), color = "#666633") + #?ø???
  geom_line(aes(x = date, y = AN, group = 1), color = "#6666FF") + #?????????Ķ?
  geom_line(aes(x = date, y = AO, group = 1), color = "#999999") + #ȸ??
  geom_line(aes(x = date, y = AP, group = 1), color = "#999900") + #??Ŭ??
  geom_line(aes(x = date, y = AQ, group = 1), color = "#9999CC") + #ä????��??????
  geom_line(aes(x = date, y = AR, group = 1), color = "#99CCCC") + #???????Ķ?
  geom_line(aes(x = date, y = AS, group = 1), color = "#0000FF") + #?Ķ?
  geom_line(aes(x = date, y = AT, group = 1), color = "#993300") + #???ǵ𰥻?
  geom_line(aes(x = date, y = AU, group = 1), color = "#FFE400") + #????
  geom_line(aes(x = date, y = AV, group = 1), color = "#FFD8D8") + #????ȫ
  geom_line(aes(x = date, y = AW, group = 1), color = "#E8D9FF") + #??????
  geom_line(aes(x = date, y = AX, group = 1), color = "#FF5E00") + #??Ȳ
  geom_line(aes(x = date, y = AY, group = 1), color = "#CEFBC9") + #?????????λ?
  geom_line(aes(x = date, y = AZ, group = 1), color = "gray") +
  xlab('date') +ylab('score')
  
ggplot(data1) +  
  geom_line(aes(x = date, y = BA, group = 1), color = "pink") +
  geom_line(aes(x = date, y = BB, group = 1), color = "#000000") +
  geom_line(aes(x = date, y = BC, group = 1), color = "#000099") +
  geom_line(aes(x = date, y = BD, group = 1), color = "#0000FF") +
  geom_line(aes(x = date, y = BE, group = 1), color = "#003300") +
  geom_line(aes(x = date, y = BF, group = 1), color = "#00CC00") +
  geom_line(aes(x = date, y = BG, group = 1), color = "#00FFFF") +
  geom_line(aes(x = date, y = BH, group = 1), color = "#006666") +
  geom_line(aes(x = date, y = BI, group = 1), color = "#0099FF") +
  geom_line(aes(x = date, y = BJ, group = 1), color = "#333300") +
  geom_line(aes(x = date, y = BK, group = 1), color = "#660066") +
  geom_line(aes(x = date, y = BL, group = 1), color = "#6666FF") +
  geom_line(aes(x = date, y = BM, group = 1), color = "#666633") +
  geom_line(aes(x = date, y = BN, group = 1), color = "#6666FF") +
  geom_line(aes(x = date, y = BO, group = 1), color = "#999999") +
  geom_line(aes(x = date, y = BP, group = 1), color = "#999900") +
  geom_line(aes(x = date, y = BQ, group = 1), color = "#9999CC") +
  geom_line(aes(x = date, y = BR, group = 1), color = "#99CCCC") +
  geom_line(aes(x = date, y = BS, group = 1), color = "#0000FF") +
  geom_line(aes(x = date, y = BT, group = 1), color = "#993300") +
  geom_line(aes(x = date, y = BU, group = 1), color = "#FFE400") +
  geom_line(aes(x = date, y = BV, group = 1), color = "#FFD8D8") +
  geom_line(aes(x = date, y = BW, group = 1), color = "#E8D9FF") +
  geom_line(aes(x = date, y = BX, group = 1), color = "#FF5E00") +
  geom_line(aes(x = date, y = BY, group = 1), color = "#CEFBC9") +
  geom_line(aes(x = date, y = BZ, group = 1), color = "gray") +
  xlab('date') +ylab('score')
  
ggplot(data1) +  
  geom_line(aes(x = date, y = CA, group = 1), color = "pink") +
  geom_line(aes(x = date, y = CB, group = 1), color = "#000000") +
  geom_line(aes(x = date, y = CC, group = 1), color = "#000099") +
  geom_line(aes(x = date, y = CD, group = 1), color = "#0000FF") +
  geom_line(aes(x = date, y = CE, group = 1), color = "#003300") +
  geom_line(aes(x = date, y = CF, group = 1), color = "#00CC00") +
  geom_line(aes(x = date, y = CG, group = 1), color = "#00FFFF") +
  geom_line(aes(x = date, y = CH, group = 1), color = "#006666") +
  geom_line(aes(x = date, y = CI, group = 1), color = "#0099FF") +
  geom_line(aes(x = date, y = CJ, group = 1), color = "#333300") +
  geom_line(aes(x = date, y = CK, group = 1), color = "#660066") +
  geom_line(aes(x = date, y = CL, group = 1), color = "#6666FF") +
  xlab('date') +ylab('score') 


#################??????????
library("ggplot2")

colnames(data2)<-c("date","B","C","D","E","F","G","H","I","J","K",
                   "L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z",
                   "AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ","AK",
                   "AL","AM","AN","AO","AP","AQ","AR","AS","AT","AU","AV","AW","AX","AY","AZ",
                   "BA","BB","BC","BD","BE","BF","BG","BH","BI","BJ","BK",
                   "BL","BM","BN","BO","BP","BQ","BR","BS","BT","BU","BV","BW","BX","BY","BZ",
                   "CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL")

#theme_set(theme_minimal()) #ggplot2 background annotation ?ּ?ȭ
#ggplot(data1, aes(date, C, group = 1)) + geom_point() + geom_line() + labs(x = "date", y = "��??����??")
#ggplot(data1) + geom_line(aes(x = date, y = B,C,D,E), cex = 0.8, show.legend = T)


ggplot(data2) + 
  geom_line(aes(x = date, y = B, group = 1), color = "#000000") +
  geom_line(aes(x = date, y = C, group = 1), color = "#000099") +
  geom_line(aes(x = date, y = D, group = 1), color = "#0000FF") +
  geom_line(aes(x = date, y = E, group = 1), color = "#003300") +
  geom_line(aes(x = date, y = F, group = 1), color = "#00CC00") +
  geom_line(aes(x = date, y = G, group = 1), color = "#00FFFF") +
  geom_line(aes(x = date, y = H, group = 1), color = "#006666") +
  geom_line(aes(x = date, y = I, group = 1), color = "#0099FF") +
  geom_line(aes(x = date, y = J, group = 1), color = "#333300") +
  geom_line(aes(x = date, y = K, group = 1), color = "#660066") +
  geom_line(aes(x = date, y = L, group = 1), color = "#6666FF") +
  geom_line(aes(x = date, y = M, group = 1), color = "#666633") +
  geom_line(aes(x = date, y = N, group = 1), color = "#6666FF") +
  geom_line(aes(x = date, y = O, group = 1), color = "#999999") +
  geom_line(aes(x = date, y = P, group = 1), color = "#999900") +
  geom_line(aes(x = date, y = Q, group = 1), color = "#9999CC") +
  geom_line(aes(x = date, y = R, group = 1), color = "#99CCCC") +
  geom_line(aes(x = date, y = S, group = 1), color = "#0000FF") +
  geom_line(aes(x = date, y = T, group = 1), color = "#993300") +
  geom_line(aes(x = date, y = U, group = 1), color = "#FFE400") +
  geom_line(aes(x = date, y = V, group = 1), color = "#FFD8D8") +
  geom_line(aes(x = date, y = W, group = 1), color = "#E8D9FF") +
  geom_line(aes(x = date, y = X, group = 1), color = "#FF5E00") +
  geom_line(aes(x = date, y = Y, group = 1), color = "#CEFBC9") +
  geom_line(aes(x = date, y = Z, group = 1), color = "gray") + 
  xlab('date') +ylab('score')


ggplot(data2) +   
  geom_line(aes(x = date, y = AA, group = 1), color = "pink") +
  geom_line(aes(x = date, y = AB, group = 1), color = "#000000") + #??��
  geom_line(aes(x = date, y = AC, group = 1), color = "#000099") + #????
  geom_line(aes(x = date, y = AD, group = 1), color = "#0000FF") + #??��?Ķ?
  geom_line(aes(x = date, y = AE, group = 1), color = "#003300") + #??��?ʷ?
  geom_line(aes(x = date, y = AF, group = 1), color = "#00CC00") + #???λ?
  geom_line(aes(x = date, y = AG, group = 1), color = "#00FFFF") + #?????ϴ?
  geom_line(aes(x = date, y = AH, group = 1), color = "#006666") + #cjdrornfl??
  geom_line(aes(x = date, y = AI, group = 1), color = "#0099FF") + #?ϴ?
  geom_line(aes(x = date, y = AJ, group = 1), color = "#333300") + #????
  geom_line(aes(x = date, y = AK, group = 1), color = "#660066") + #????
  geom_line(aes(x = date, y = AL, group = 1), color = "#6666FF") + #?ڹ?Ʈ????
  geom_line(aes(x = date, y = AM, group = 1), color = "#666633") + #?ø???
  geom_line(aes(x = date, y = AN, group = 1), color = "#6666FF") + #?????????Ķ?
  geom_line(aes(x = date, y = AO, group = 1), color = "#999999") + #ȸ??
  geom_line(aes(x = date, y = AP, group = 1), color = "#999900") + #??Ŭ??
  geom_line(aes(x = date, y = AQ, group = 1), color = "#9999CC") + #ä????��??????
  geom_line(aes(x = date, y = AR, group = 1), color = "#99CCCC") + #???????Ķ?
  geom_line(aes(x = date, y = AS, group = 1), color = "#0000FF") + #?Ķ?
  geom_line(aes(x = date, y = AT, group = 1), color = "#993300") + #???ǵ𰥻?
  geom_line(aes(x = date, y = AU, group = 1), color = "#FFE400") + #????
  geom_line(aes(x = date, y = AV, group = 1), color = "#FFD8D8") + #????ȫ
  geom_line(aes(x = date, y = AW, group = 1), color = "#E8D9FF") + #??????
  geom_line(aes(x = date, y = AX, group = 1), color = "#FF5E00") + #??Ȳ
  geom_line(aes(x = date, y = AY, group = 1), color = "#CEFBC9") + #?????????λ?
  geom_line(aes(x = date, y = AZ, group = 1), color = "gray") +
  xlab('date') +ylab('score')

ggplot(data2) +  
  geom_line(aes(x = date, y = BA, group = 1), color = "pink") +
  geom_line(aes(x = date, y = BB, group = 1), color = "#000000") +
  geom_line(aes(x = date, y = BC, group = 1), color = "#000099") +
  geom_line(aes(x = date, y = BD, group = 1), color = "#0000FF") +
  geom_line(aes(x = date, y = BE, group = 1), color = "#003300") +
  geom_line(aes(x = date, y = BF, group = 1), color = "#00CC00") +
  geom_line(aes(x = date, y = BG, group = 1), color = "#00FFFF") +
  geom_line(aes(x = date, y = BH, group = 1), color = "#006666") +
  geom_line(aes(x = date, y = BI, group = 1), color = "#0099FF") +
  geom_line(aes(x = date, y = BJ, group = 1), color = "#333300") +
  geom_line(aes(x = date, y = BK, group = 1), color = "#660066") +
  geom_line(aes(x = date, y = BL, group = 1), color = "#6666FF") +
  geom_line(aes(x = date, y = BM, group = 1), color = "#666633") +
  geom_line(aes(x = date, y = BN, group = 1), color = "#6666FF") +
  geom_line(aes(x = date, y = BO, group = 1), color = "#999999") +
  geom_line(aes(x = date, y = BP, group = 1), color = "#999900") +
  geom_line(aes(x = date, y = BQ, group = 1), color = "#9999CC") +
  geom_line(aes(x = date, y = BR, group = 1), color = "#99CCCC") +
  geom_line(aes(x = date, y = BS, group = 1), color = "#0000FF") +
  geom_line(aes(x = date, y = BT, group = 1), color = "#993300") +
  geom_line(aes(x = date, y = BU, group = 1), color = "#FFE400") +
  geom_line(aes(x = date, y = BV, group = 1), color = "#FFD8D8") +
  geom_line(aes(x = date, y = BW, group = 1), color = "#E8D9FF") +
  geom_line(aes(x = date, y = BX, group = 1), color = "#FF5E00") +
  geom_line(aes(x = date, y = BY, group = 1), color = "#CEFBC9") +
  geom_line(aes(x = date, y = BZ, group = 1), color = "gray") +
  xlab('date') +ylab('score') 

ggplot(data2) +  
  geom_line(aes(x = date, y = CA, group = 1), color = "pink") +
  geom_line(aes(x = date, y = CB, group = 1), color = "#000000") +
  geom_line(aes(x = date, y = CC, group = 1), color = "#000099") +
  geom_line(aes(x = date, y = CD, group = 1), color = "#0000FF") +
  geom_line(aes(x = date, y = CE, group = 1), color = "#003300") +
  geom_line(aes(x = date, y = CF, group = 1), color = "#00CC00") +
  geom_line(aes(x = date, y = CG, group = 1), color = "#00FFFF") +
  geom_line(aes(x = date, y = CH, group = 1), color = "#006666") +
  geom_line(aes(x = date, y = CI, group = 1), color = "#0099FF") +
  geom_line(aes(x = date, y = CJ, group = 1), color = "#333300") +
  geom_line(aes(x = date, y = CK, group = 1), color = "#660066") +
  geom_line(aes(x = date, y = CL, group = 1), color = "#6666FF") +
  xlab('date') +ylab('score') 
