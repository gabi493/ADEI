#df<-read.table("sorting.csv",header=T)
df
summary(df)
attach(df)

oneway.test(cputime~f.algo,data=df)
library(FactoMineR)
condes(df[,c(1,3)],2)

oneway.test(I(cputime/size)~f.algo,data=df)
kruskal.test(I(cputime/size)~f.algo,data=df)

pairwise.wilcox.test(I(cputime/size),f.algo,data=df)

shapiro.test(I(cputime/size))

m1<-lm(cputime~size)# small
summary(m1)

library(car)
scatterplot(cputime~size|f.algo,smooth=F)
m2<-lm(cputime~size*f.algo) # interaccions
m2a<-lm(cputime~size+f.algo) # additiu

# hii: hatvalues
hatvalues(m2)
Boxplot(hatvalues(m2))
cooks.distance(m2)

llista<-Boxplot(cooks.distance(m2))
df[llista,]

summary(m2)
summary(m2a)

anova(m2a,m2)
head(df)

anova(m1,m2)




m1<-lm(I(cputime/size)~f.algo)
summary(m1)
par(mfrow=c(2,2))
plot(m1)
library(car)
scatterplot(cputime~size|f.algo,data=df,smooth=F)
scatterplot(log(cputime)~log(size)|f.algo,data=df,smooth=F)
m5<-lm(log(cputime)~log(size)*f.algo,data=df )
summary(m5)

