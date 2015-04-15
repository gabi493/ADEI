# Instalar el package que se va a utilizar; este package se tiene que instalar previamente
### CARGAR LOS PACKAGES FACTOMINER Y MASS

library(FactoMineR)
library(MASS)
library(missMDA)

rm(list = ls()) #eliminar objetos

# Leer la base (que está en este mismo directorio)
baseB<-read.csv2("Croacia-Scores-B-SocioeconoBIS.csv",row.names=1,header=TRUE,dec=".")
baseCD<-read.csv2("Croacia_Var_C_D.csv",row.names=1,header=TRUE,dec=".")
base<-cbind(baseB,baseCD)
colnames(base)

# Con los valores "0" en los scores

res.pca<-PCA(base[,45:52]) 


# Leer la base Croacia-Scores.csv donde los "0" en los scores son ahora missing 
base<-read.csv2("Croacia-Scores.csv",row.names=1,header=TRUE,dec=".")
colnames(base)
summary(base)

res.pca <- PCA(base[,6:13])

## 
?estim_ncpPCA
nb <- estim_ncpPCA(base[,6:13],ncp.min=0,ncp.max=5,method.cv="Kfold",nbsim=20,pNA=0.05)
nb

res.comp <- imputePCA(base[,6:13],ncp=nb$ncp)
res.pca <- PCA(res.comp$completeObs)

res.comp <- imputePCA(base[,6:13],ncp=2)
res.pca <- PCA(res.comp$completeObs)


