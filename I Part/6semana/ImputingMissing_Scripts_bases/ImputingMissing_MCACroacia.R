
rm(list = ls()) #eliminar objet


library(FactoMineR)
library(Matrix)
library(missMDA)



###  lectura de los datos: tabla léxica y variables cerradas
# Leer la base (que está en este mismo directorio)
baseB<-read.csv2("Croacia-Scores-B-SocioeconoBIS.csv",row.names=1,header=TRUE,dec=".")
baseCD<-read.csv2("Croacia_Var_C_D_missing.csv",row.names=1,header=TRUE,dec=".",na.strings="      ")
base<-cbind(baseB,baseCD)
dim(base)
colnames(base)
summary(base)

res.mca<-MCA(base[,53:58])

result <- estim_ncpMCA(base[,53:58], ncp.min=0, ncp.max=5,method.cv="Kfold",nbsim=20,pNA=0.05)
result

## Impute the indicator matrix and perform a MCA
?imputeMCA
tab.disj<-imputeMCA(base[,53:58], ncp=5)$tab.disj
tab.disj[1:10,53:58]
res.impute <- imputeMCA(base[,53:58], ncp=5)
res.impute
res.impute$tab.disj[1:10,]
## The imputed indicator matrix can be used as an input of the MCA function of the
## FactoMineR package to perform the MCA on the incomplete data ozone 
?MCA
res.mca <- MCA(base[,53:58],tab.disj=res.impute$tab.disj) 
res.mca <- MCA(res.impute$completeObs) 
