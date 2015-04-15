
rm(list = ls()) #eliminar objet


library(FactoMineR)
library(Matrix)



###  lectura de los datos: tabla léxica y variables cerradas
# Leer la base (que está en este mismo directorio)
baseB<-read.csv2("Croacia-Scores-B-SocioeconoBIS.csv",row.names=1,header=TRUE,dec=".")
baseCD<-read.csv2("Croacia_Var_C_D.csv",row.names=1,header=TRUE,dec=".")
base<-cbind(baseB,baseCD)
colnames(base)

####
dim(base)	
colnames(base)

#
# MCA with only active categorical variables
?MCA	
		#level.ventil agrupa la categoria escogida por menos de 1-2% de los individuos en el lado opuesto para que se vean más, "ventilamos" esta categoría
		#
		#

colnames(base[,6:40])
res.mca<-MCA(base[,6:40],level.ventil = 2)
summary(res.mca)	#nos genera las gráficas:
						#forma de parábola, se han ordenado los indivíduos -> efecto Guttman: orden sobre las filas y las columnas, y una relación entre esos dos. Se detecta con frecuencia.
						#nos muestra los nombres de las categorías en el grafo (en vez de ID)
						#
round(res.mca$eig[1:6,],2)
barplot(res.mca$eig[,1],main="valeurs propres",names.arg=1:nrow(res.mca$eig)) 
		#un primer valor propio muy distinto a los demás


# Analysis with less variables, in order to better understand
colnames(base [,c(18:24,28:36)])
res.mca<-MCA(base [,c(18:24,28:36)],level.ventil = 2)
round(res.mca$eig[1:6,],2)
barplot(res.mca$eig[,1],main="valeurs propres",names.arg=1:nrow(res.mca$eig))

# Second Reduced analysis
colnames(base [,c(18:21)])
res.mca<-MCA(base [,c(18:21)],level.ventil = 2)
round(res.mca$eig[1:6,],2)
barplot(res.mca$eig[,1],main="valeurs propres",names.arg=1:nrow(res.mca$eig))

# Third Reduced analysis
colnames(base [,c(22:24,28:36)])
res.mca<-MCA(base [,c(22:24,28:36)],level.ventil = 2)
round(res.mca$eig[1:6,],2)
barplot(res.mca$eig[,1],main="valeurs propres",names.arg=1:nrow(res.mca$eig))


# Complete analysis: MCA with quantitative and categorical supplementary variab
res.mca<-MCA(base,quanti.sup=c(44:52),quali.sup=c(1:5,41:43),level.ventil = 2)
base[which(row.names(base)=="I3040"),]
base[which(row.names(base)=="I2479"),]
base[which(row.names(base)=="I4578"),]


# exploitation of the results of this MCA
# 1. Valores propios
round(res.mca$eig[1:6,],2)
barplot(res.mca$eig[,1],main="valeurs propres",names.arg=1:nrow(res.mca$eig))
# 2. Gráficos
?plot.MCA
# sólo los individuos
plot(res.mca,invisible=c("var","quali.sup","quanti.sup"))
#
#  categorías suplementarias
plot(res.mca,choix="ind",invisible=c("ind","var"))
#
# quantitative suplementaria
plot(res.mca,choix=c("quanti.sup"))

# quantitativa suplementaria
plot(res.mca,choix=c("var"),invisible=c("quali.sup","quanti.sup"))

#
# descripciçon de los ejes
dimdesc(res.mca,proba=0.01)

##### clustering
res.mca<-MCA(base[,1:6])
### First check arguments and values of HCPC
?HCPC

### Perform a HCPC ###
res.hcpc<-HCPC(res.mca,nb.clust=0,order=TRUE)

### Interprete the results of clustering ###
names(res.hcpc)

### data.clust ### 
### The original data with a supplementary row containing the partition ###
res.hcpc$data.clust
colnames(res.hcpc$data.clust)

### Counts of individuals in each cluster ###
summary(res.hcpc$data.clust$clust)

### desc.var ###
### A. The description of the clusters by the variables ###
names(res.hcpc$desc.var)

### desc.var$test.chi2 ###
### A.1. The categorical variables which characterizes the clusters ###
res.hcpc$desc.var$test.chi2

### desc.var$category ###
### A.2. The description of each cluster by the categories ##
res.hcpc$desc.var$category

### desc.var$quanti.var ###
### A.3. The quantitative variables which characterizes the clusters ###
res.hcpc$desc.var$quanti.var

### desc.var$quanti ###
### A.4. The description of each cluster by the quantitative variables ###
res.hcpc$desc.var$quanti

### desc.axes ###
### B. The description of the clusters by the axes ###
names(res.hcpc$desc.axes)
res.hcpc$desc.axes$quanti.var
res.hcpc$desc.axes$quanti

### desc.ind ###
### C. The description of the clusters by the individuals ###
names(res.hcpc$desc.ind)
res.hcpc$desc.ind$para
res.hcpc$desc.ind$dist

### THE END ###

