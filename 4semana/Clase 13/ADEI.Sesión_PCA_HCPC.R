# Instalar el package que se va a utilizar; este package se tiene que instalar previamente
### CARGAR LOS PACKAGES FACTOMINER Y MASS

library(FactoMineR)
library(MASS)

rm(list = ls()) #eliminar objetos

# Leer la base (que está en este mismo directorio)
baseB<-read.csv2("Croacia-Scores-B-SocioeconoBIS.csv",row.names=1,header=TRUE,dec=".")
baseCD<-read.csv2("Croacia_Var_C_D.csv",row.names=1,header=TRUE,dec=".")
base<-cbind(baseB,baseCD)
colnames(base)

# ordenar los niveles de las variables categóricas e indicar que serán
# consideradas como factores (si hace falta)

base$SKOLA<-factor(base$SKOLA,levels=c("no_school","primary_school","handicraft_school",
                 "grammar_school","high_school","faculty_PhD_MSc"))

base$B1<-factor(base$B1,levels=c("health_excellent","health_very good","health_good","health_fair","health_poor" ))

summary(base$SKOLA)
summary(base$B1)
#...hacer lo mismo para las otras variables categóricas   Imperativo!
# no cambia los resultados pero facilita la lectura y el manejo de ciertos comandos

# Crear una nueva variable cruzando nivel de estudios y edad_en_clases
SKOLAbis<-base$SKOLA
summary(SKOLAbis)
levels(SKOLAbis)<-c("primary-no-school","primary-no-school","gram-handi-school","gram-handi-school","faculty-high-school","faculty-high-school")          
summary(SKOLAbis)

Edadcla<-base$Edad_classe
levels(Edadcla)
summary(Edadcla)
levels(Edadcla)<-c("18_35 años","18_35 años","36_55 años","36_55 años","56 y más","56 y más","56 y más")
summary(Edadcla)

JovHigh<-which((Edadcla=="18_35 años") & (SKOLAbis=="faculty-high-school"))
JovHigh
JovMedium<-which((Edadcla=="18_35 años") & (SKOLAbis=="gram-handi-school"))
JovLow<-which((Edadcla=="18_35 años") & (SKOLAbis=="primary-no-school"))
MidHigh<-which((Edadcla=="36_55 años") & (SKOLAbis=="faculty-high-school"))
MidMedium<-which((Edadcla=="36_55 años") & (SKOLAbis=="gram-handi-school"))
MidLow<-which((Edadcla=="36_55 años") & (SKOLAbis=="primary-no-school"))
OldHigh<-which((Edadcla=="56 y más") & (SKOLAbis=="faculty-high-school"))
OldMedium<-which((Edadcla=="56 y más") & (SKOLAbis=="gram-handi-school"))
OldLow<-which((Edadcla=="56 y más") & (SKOLAbis=="primary-no-school"))
OldLow
Edad_Skol<-factor()
levels(Edad_Skol)<-c("JovHigh","JovMedium","JovLow","MidHigh","MidMedium","MidLow","OldHigh","OldMedium","OldLow")
Edad_Skol[1:100]
Edad_Skol[JovHigh]<-"JovHigh"
Edad_Skol[JovMedium]<-"JovMedium"
Edad_Skol[JovLow]<-"JovLow"
Edad_Skol[MidHigh]<-"MidHigh"
Edad_Skol[MidMedium]<-"MidMedium"
Edad_Skol[MidLow]<-"MidLow"
Edad_Skol[OldHigh]<-"OldHigh"
Edad_Skol[OldMedium]<-"OldMedium"
Edad_Skol[OldLow]<-"OldLow"
summary(Edad_Skol)

dim(base)
base[,83]<-Edad_Skol
colnames(base)<-c(colnames(base)[1:82],"Edad_Skol")
colnames(base)

# Se empieza el análisis en componentes principales tomando los "scores" como activos
par(cex=0.5)
### primero, sólo las variables activas
summary(base[,45:52])
?PCA
res.pca<-PCA(base[,45:52]) 
names(res.pca)
summary(res.pca)

#
### a. eigenvalues and dominant axes. How many axes we have to interpret? ##############
round(res.pca$eig,2)
barplot(res.pca$eig[,1],main="valores propios",names.arg=paste("dim",1:nrow(res.pca$eig)))


### b. Interpreting the axes:  Individuals point of view
# Are they any individuals "too contributive"       
names(res.pca$ind)
# in this case, no
# To better understand the axes through the extreme individuals
res.pca$ind$coord
rango<-order(res.pca$ind$coord[,1])
row.names(base)[rango[5028:5037]]
row.names(base)[rango[1]]
base[which(row.names(base)=="I0536"),44]
base[which(row.names(base)=="I0536"),45:52]
base[which(row.names(base)=="I3328"),45:52]
base[which(row.names(base)=="I3631"),45:52]
base[which(row.names(base)=="I1780"),45:52]
base[which(row.names(base)=="I1780"),44]
####

### c. Interpreting the axes:  Variables point of view
dimdesc(res.pca)

### MUY IMPORTANTE: elementos suplementarios   
d. Tomar en cuenta variables suplementarias

colnames(base[,c(2,44,45:52)])
res.pca <- PCA(base[,c(2,44,45:52)],quali.sup=c(1),quanti.sup=2,ncp=4)

?plot.PCA
plot.PCA(res.pca,invisible="ind")
lines(res.pca$quali.sup$coord[1:6,1],res.pca$quali.sup$coord[1:6,2],lwd=2,col="black")
res.pca$quali.sup 
res.pca$quanti.sup
par(cex=0.6)
plot(res.pca,invisible="ind",xlim=c(-1.5,1.0),ylim=c(-0.5,0.5))
lines(res.pca$quali.sup$coord[1:6,1],res.pca$quali.sup$coord[1:6,2],lwd=2,col="black")

d.bis Otra variable categórica suplementaria
colnames(base[,c(45:52,83)])
res.pca <- PCA(base[,c(45:52,83)],quali.sup=c(9),ncp=4)
res.pca$quali.sup 

plot.PCA(res.pca,invisible="ind")
lines(res.pca$quali.sup$coord[1:3,1],res.pca$quali.sup$coord[1:3,2],lwd=2,col="black")
lines(res.pca$quali.sup$coord[4:6,1],res.pca$quali.sup$coord[4:6,2],lwd=2,col="black")
lines(res.pca$quali.sup$coord[7:9,1],res.pca$quali.sup$coord[7:9,2],lwd=2,col="black")

d.bis Otra variable categórica suplementaria
colnames(base[,c(1,45:52,83)])
res.pca <- PCA(base[,c(1,45:52,83)],quali.sup=c(1,10))
res.pca$quali.sup 
par(cex=0.7)
plot.PCA(res.pca,invisible="ind")
lines(res.pca$quali.sup$coord[3:5,1],res.pca$quali.sup$coord[3:5,2],lwd=2,col="black")
lines(res.pca$quali.sup$coord[6:8,1],res.pca$quali.sup$coord[6:8,2],lwd=2,col="black")
lines(res.pca$quali.sup$coord[9:11,1],res.pca$quali.sup$coord[9:11,2],lwd=2,col="black")

##############################################################
###  Synthesis through HCPC (Clustering the individuals)   ###
##############################################################
#Hoy trabajamos con esta base

colnames(base[,c(1,5,44,45:52,83)])
### Before, you have to perform a PCA with the number of axes ###  that you decided to take into account (indicated through ncp in PCA) ###
#analisis pero sin que salgan las figuras porque pone "graph=f"
res.pca<-PCA(base[,c(1,5,44,45:52,83)],quanti.sup=3,quali.sup=c(1,2,12),ncp=2,graph=F)

#cuanto mas larga es la linea de los ejes, mas relacion hay
#el eje X diferencia bastante los que tiene buena salud (por encima) de mala salud (por debajo)
#el % de la dimension es la variabilidad que se explica en la dimension correspondiente
#res.pca<-PCA(base[,c(1,5,44,45:52,83)],quanti.sup=3,quali.sup=c(1,2,12),ncp=2,graph=T)

#para mostrar los porcentajes de las distintas dimensiones
#cada valor propio (eigvalue, primera columna), da la variabilidad sobre la dimension
#res.pca$eig


### First check arguments and values of HCPC ----> es la AYUDA
#?HCPC

#trabajamos con RES (resultados del analisis de componentes), nb.clust(numero de clases que queremos tener en el arbol, si ponemos 0 lo cortamos por donde nosotros queramos con el raton), min (minimo de clases que queremos ejecutar)
### Perform a HCPC ###
res.hcpc<-HCPC(res.pca,nb.clust=0,min=2)
#lo proyectamos en el eje X para no mezclar los colores rojo y negro
#el resultado con 3a dimension es muy lioso con tantos datos, pero con unos 10 ayuda mucho a ver las alturas y distancias entre los individuos

### Interprete the results of clustering ###
#muestra los objetos: individuos, variables, etc...
names(res.hcpc)

### data.clust ### 
### The original data with a supplementary row containing the partition ###

res.hcpc$data.clust				#añadir a la BD original una nueva variable, que es el grupo
row.names(res.hcpc$data.clust)	#nombre de los individuos
summary(res.hcpc$data.clust$clust)	#cantidad de individuos en cada clase

# Atención! el orden de los respondientes en data$clust no es siempre el inicial.  Mejor, rectificarlo. En este caso el identificador
DataClust<-res.hcpc$data.clust
DataClust<-DataClust[row.names(base),]
row.names (DataClust)

### desc.var ###
### A. The description of the clusters by the variables ###
names(res.hcpc$desc.var)

### desc.var$test.chi2 ###
### A.1. The categorical variables which characterizes the clusters ###
res.hcpc$desc.var$test.chi2		#p.valor y diferencia. Como el p.value es menor de 0.05 rechazamos la H0 y podemos decir que estan relacionados

SexXClus<-table(res.hcpc$data.clust$Sex,res.hcpc$data.clust$clust)
SexXClus
	
sweep(SexXClus,2,apply(SexXClus,2,sum),"/")	#hace una tabla cruzando individuos
#vemos que hay muchas mas mujeres en el grupo 1 que en el grupo 2. Hace comparaciones por proporcion, porcentajes

### desc.var$category ###
### A.2. The description of each cluster by the categories ##
res.hcpc$desc.var$category	#como son valores categoricos, nos da los porcentajes: la primera columna indica que el 90% de los que tienen health_poor, han caido en el grupo 1. La segunda columna indica, cuantos de los del grupo 1, tienen esa salud -> no ha de ser 100 la suma con los del otro grupo! Global es respecto a total de individuos, cuantos tienen health_poor. Por lo tanto, busca las categorías que caracterizan a cada clase. 
table(res.hcpc$data.clust$B1,res.hcpc$data.clust$clust)
poorXclust1<-table(res.hcpc$data.clust$B1,res.hcpc$data.clust$clust)[5,1]
poorXclust1
res.hcpc$data.clust$B1
poor<-sum(res.hcpc$data.clust$B1=="health_poor")	#cuantos tienen en total health_poor?
poor

clust1<-sum(res.hcpc$data.clust$clust=="1")
clust1
N<-nrow(res.hcpc$data.clust)
poorXclust1/poor ###Cla/Mod
poorXclust1/clust1 ###Mod/Cla
poor/N ###Global

#v.test: 
# +: mucho mas presente en la clase que en el global
# -: poco presente en la clase respecto al global


### desc.var$quanti.var ###
### A.3. The quantitative variables which characterizes the partition  ###
res.hcpc$desc.var$quanti.var	#estan corelacionadas las variables con la relacion?

### desc.var$quanti ###


### A.4. The description of each cluster by the quantitative variables ###
res.hcpc$desc.var$quanti	#para ver que tipo de relacion hay, con las medias, porque no son variables categoricas, son cuantitativas:
# en la primera clase las medias son menores respecto al global. En la segunda clase son mayores.
mean(res.hcpc$data.clust$Edad[res.hcpc$data.clust$clust==1])
mean(res.hcpc$data.clust$Edad)
sd(res.hcpc$data.clust$Edad[res.hcpc$data.clust$clust==1])
sd(res.hcpc$data.clust$Edad)

### desc.axes ###
### B. The description of the clusters by the axes ###
names(res.hcpc$desc.axes)
res.hcpc$desc.axes$quanti.var
res.hcpc$desc.axes$quanti	#esta mas diferenciado en la 1a dimension que en la 2a

### desc.ind ###
### C. The description of the clusters by the individuals ###
names(res.hcpc$desc.ind)
res.hcpc$desc.ind$para	#los individuos mas cercanos al centro de gravedad de la clase (nos da 5 individuos de cada clase)
res.hcpc$desc.ind$dist	#los individuos mas particulares de la clase, los mas extremos

para1<-which(rownames(res.pca$ind$coord)%in%names(res.hcpc$desc.ind$para[[1]]))
para2<-which(rownames(res.pca$ind$coord)%in%names(res.hcpc$desc.ind$para[[2]]))

dist1<-which(rownames(res.pca$ind$coord)%in%names(res.hcpc$desc.ind$dist[[1]]))
dist2<-which(rownames(res.pca$ind$coord)%in%names(res.hcpc$desc.ind$dist[[2]]))
?plot.PCA
plot(res.pca,label="none",invisible="quali")
points(res.pca$ind$coord[para1,1],res.pca$ind$coord[para1,2],col="blue",cex=2,pch=16)
points(res.pca$ind$coord[dist1,1],res.pca$ind$coord[dist1,2],col="orange",cex=2,pch=16)
points(res.pca$ind$coord[para2,1],res.pca$ind$coord[para2,2],col="blue",cex=2,pch=16)
points(res.pca$ind$coord[dist2,1],res.pca$ind$coord[dist2,2],col="orange",cex=2,pch=16)
#vemos los extremos y los representantes

res.hcpc$data.clust[which(rownames(res.hcpc$data.clust)%in%names(res.hcpc$desc.ind$para[[1]])),]	#individuos medios de la primera clase
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust)%in%names(res.hcpc$desc.ind$dist[[1]])),]
#vemos que la primera tabla estan todos en la media, y la segunda tabla estan todos en 0


res.hcpc$data.clust[which(rownames(res.hcpc$data.clust)%in%names(res.hcpc$desc.ind$para[[2]])),]	#media muy alta
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust)%in%names(res.hcpc$desc.ind$dist[[2]])),]	#media extrema, de 100



### call ###
### Other parameters and objects of HCPC ###
names(res.hcpc$call)

### call$t ###
### Results for the hierarchical tree ###
names(res.hcpc$call$t)
### Results for the PCA ###
res.hcpc$call$t$res
### The suggested level to cut the tree  ###
res.hcpc$call$t$nb.clust
### Within inertias ###
res.hcpc$call$t$within[1:5]	#inercia intra
### Ratio between within inertias ###
res.hcpc$call$t$quot[1:5]
2.244554/5.458357
### Inertia gain ###
res.hcpc$call$t$inert.gain[1:5]

####
#### THE END
####
 
# Belchin: belchin3541@gmail.com


