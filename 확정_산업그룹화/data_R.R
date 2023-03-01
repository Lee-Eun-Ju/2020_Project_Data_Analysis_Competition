
setwd("C:\\Users\\eunju\\Desktop\\3학년 2학기\\공모전_2020년 통계데이터 분석활용대회\\1.산업그룹화")
data=read.csv("data.csv",head=TRUE)
str(data)
head(data)
rownames(data)=c("A","B","C","D","E","F","G",
                 "H","I","J","K","L","M","N","P","Q","R","S")
data=data[,-1]

###다중공선성 문제
R=round(cor(data),3)
R
###기업체수,부채비율,부채,자본,전체기업체대비신규기업체수 제외
data=data[,-c(1,5,10,11,15)]
#정규성 검정
library(MVN)
result= mvn(data, mvnTest = "mardia", multivariatePlot =  "qq")
result

################################################################################PCA
R=round(cor(data),3); R
#[Step 3] Spectral Decomposition (# of factor)
eigen.R=eigen(R)
round(eigen.R$values, 2) # Eigenvalues
V=round(eigen.R$vectors, 2) # Eigenvectors
V
#[Step 4] Number of factors : m (# of factor)
p=ncol(data)
gof=eigen.R$values/p*100 # Goodness-of fit
round(gof, 3) # contribution rate
plot(eigen.R$values, type="b", main="Scree Graph", xlab="Factor Number", ylab="Eigenvalue")

#[Step 5]Factor LoadinPCAgs and Communality
V2=V[,1:2]
L=V2%*%diag(sqrt(eigen.R$values[1:2])) # Loading matrix
rownames(L) = colnames(data)
colnames(L) = c("요인1","요인2")
round(L, 3)
round(diag(L%*%t(L)), 3) 

#[Step 6]Specific Variance
Psi=diag(R-L%*%t(L))
round(Psi, 3)

#[Step 7] Residual Matrix
Rm = R-(L%*%t(L) + diag(Psi)) 
round(Rm, 3)

#PCs scores
Z=scale(data, scale=T) #Standardized Data Matrix
P=Z%*%V2 #PCs Scores
round(P, 3)

plot(P[,1], P[,2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[,2], labels=rownames(data), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

###Biplot (Singular Value Decomposition)
n= nrow(data) 
rownames(data)
colnames(data)
joinnames=c(rownames(data),colnames(data))

Z=scale(data, scale=T)
svd.Z <- svd(Z) 
U1 <- svd.Z$u    
V1 <- svd.Z$v 
D1 <- diag(svd.Z$d)
G1 <- (sqrt(n-1)*U1)[,1:2]
H1 <- (sqrt(1/(n-1))*V1%*%D1)[,1:2] 
C1 <- rbind(G1, H1)
rownames(G1)<-rownames(data)
rownames(H1)<-colnames(data)
rownames(C1)<-joinnames

# Godness-of-fit
eig1 <- (svd.Z$d)^2 
per1 <- eig1/sum(eig1)*100
per1
gof1 <- sum(per1[1:2])

# Biplots
lim<-range(pretty(G1))
lim<-c(-2.5,3.5)
xlim<-c(-1,3)
ylim<-c(-2,3)
biplot(G1[,1:2],H1[,1:2], xlab="1st PC",ylab="2nd PC", main="biplot function",
       xlim=lim,lim=ylim,cex=0.8,pch=16)
abline(v=0,h=0)


######################################################################MLFA
Z=scale(data, scale=T)
library(psych)
mlfa= factanal(Z, factors = 2, rotation="varimax", score="regression")
mlfa

# Residual Matrix
Lm=mlfa$loading[, 1:2]
round(Lm, 3)
Psim=mlfa$uniquenesses
round(Psim,3)
Rmm = R-(Lm%*%t(Lm) + diag(Psim))
round(Rmm, 3)

#Plot of Factor loadings
lim<-range(pretty(Lm))
plot(Lm[,1], Lm[,2],main="ML Factor Loadings : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(Lm[,1], Lm[, 2], labels=rownames(Lm), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, Lm[,1], Lm[, 2], col=2, code=2, length=0.1)

## Factor Scores
fml=mlfa$scores
round(fml, 3)

# Plot of Factor Scores
lim<-range(pretty(fml))
plot(fml[,1], fml[,2],main="Factor Scores : ml f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(fml[,1], fml[,2], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

#Biplot
# Biplot based on the Singular Value Decomposition
svd.Z <- svd(Z) 
U <- svd.Z$u    
V <- svd.Z$v 
D <- diag(svd.Z$d)
F <- (sqrt(n-1)*U)[,1:4]  # Factor Scores Matrix : F
L <- (sqrt(1/(n-1))*V%*%D)[,1:4] # Factor Loadings Matrix : Lambda
C <- rbind(F, L)
rownames(F)<-rownames(data)
rownames(L)<-colnames(data)

# Godness-of-fit
eig <- (svd.Z$d)^2 
per <- eig/sum(eig)*100
gof <- sum(per[1:4])
per
gof

# Varimax Rotated Biplot
varimax<-varimax(L)
Lt = varimax$loadings 
T=varimax$rotmat
T
Ft= F%*%T

lim <- c(-2,2)
biplot(Ft[,c(1,2)],Lt[,c(1,2)], xlab="f1",ylab="f2", main="(a) Varimax Rotated Biplot : f1 and f2",
       xlim=lim,ylim=lim,cex=0.8,pch=16)
abline(v=0,h=0)





#######clustering
#(1)
X = data
rownames(X) = rownames(data)
for (i in 1:10) {
  X[,i] = ifelse(X[,i] >= mean(X[,i]),1,0)}
X = as.matrix(X); X

#(2)
n = dim(X)[1]; p = dim(X)[2]
J = matrix(1,n,p)
Cs = (X%*%t(X) + (J-X)%*%t(J-X))/p; Cs
ds = sqrt(p*(1-Cs))
ds = as.dist(ds); round(ds,3)

#(3)
Y = data
rownames(Y) = rownames(data)

z = scale(Y, scale=T)
d = dist(z, method="euclidean"); round(d,3)

single = hclust(d, method="single")
average = hclust(d, method="average")
ward = hclust(d, method="ward.D2")

plot(single, hang=-1, labels=rownames(Y))
plot(average, hang=-1, labels=rownames(Y))
plot(ward, hang=-1, labels=rownames(Y))

#(4)
#K-means Method
kmeans = kmeans(z,4)
cluster = data.frame(rownames(Y), cluster=kmeans$cluster)
c1 = cluster[(cluster[,2]==1),]
c2 = cluster[(cluster[,2]==2),]
c3 = cluster[(cluster[,2]==3),]
c4 = cluster[(cluster[,2]==4),]
c1; c2; c3; c4

aggregate(Y, by=list(kmeans$cluster), FUN=mean)

#K-medoids Method
library(cluster)
kmedoids = pam(z, 5, metric="euclidean")
cluster = data.frame(rownames(Y), cluster=kmedoids$cluster)
c1 = cluster[(cluster[,2]==1),]
c2 = cluster[(cluster[,2]==2),]
c3 = cluster[(cluster[,2]==3),]
c4 = cluster[(cluster[,2]==4),]
c5 = cluster[(cluster[,2]==5),]
c1; c2; c3; c4; c5

aggregate(Y, by=list(kmedoids$cluster), FUN=mean)

#(6)
kmeans_Y = kmeans(Y,5)
cluster = data.frame(rownames(Y), cluster=kmeans_Y$cluster)
c1 = cluster[(cluster[,2]==1),]
c2 = cluster[(cluster[,2]==2),]
c3 = cluster[(cluster[,2]==3),]
c4 = cluster[(cluster[,2]==4),]
c5 = cluster[(cluster[,2]==5),]
c1; c2; c3; c4; c5

aggregate(Y, by=list(kmeans_Y$cluster), FUN=mean)

install.packages("gplots")
library("gplots")
heatmap.2(z,scale="none",col=bluered(100), trace="none", density.info="none")
