library(FactoMineR)
library(missMDA)

data(orange)
## 
?estim_ncpPCA
nb <- estim_ncpPCA(orange,ncp.min=0,ncp.max=5,method.cv="Kfold",nbsim=20,pNA=0.05)
nb

res.comp <- imputePCA(orange,ncp=nb$ncp)
res.pca <- PCA(res.comp$completeObs)
resMI <- MIPCA(orange,ncp=2)
plot(resMI)

## Case MCA 

?estim_ncpMCA
data(vnf)
View(vnf[1:20,])
vnf[1:20,]
summary(vnf[1:20,])

result <- estim_ncpMCA(vnf[1:20,], ncp.min=0, ncp.max=5,method.cv="Kfold",nbsim=20,pNA=0.05)
result

## Impute the indicator matrix and perform a MCA
?imputeMCA
tab.disj<-imputeMCA(vnf[1:20,], ncp=5)$tab.disj
tab.disj
res.impute <- imputeMCA(vnf[1:20,], ncp=5)
res.impute
res.impute$tab.disj
## The imputed indicator matrix can be used as an input of the MCA function of the
## FactoMineR package to perform the MCA on the incomplete data ozone 
?MCA
res.mca <- MCA(vnf[1:20,],tab.disj=res.impute$tab.disj) 
res.mca <- MCA(res.impute$completeObs) 
## End