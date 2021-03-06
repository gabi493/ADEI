rm(list = ls()) #eliminar objetos

### CARGAR LOS PACKAGES FACTOMINER Y MASS
library(FactoMineR)
library(MASS)


# Leer la base (que est� en este mismo directorio)
baseB<-read.csv2("Croacia-Scores-B-SocioeconoBIS.csv",row.names=1,header=TRUE,dec=".")
baseCD<-read.csv2("Croacia_Var_C_D.csv",row.names=1,header=TRUE,dec=".")
base<-cbind(baseB,baseCD)


### SF36 Health Survey ###
#browseURL("http://www.sf-36.org/tools/sf36.shtml")
 
# informaci�n inicial sobre la base
dim(base)
colnames(base)
summary(base)
?summary
summary(base,maxsum=100)
summary(base$Sex)


# ordenar los niveles de las variables categ�ricas e indicar que ser�n
# consideradas como factores (si hace falta)
summary(base$SKOLA)
base$SKOLA<-factor(base$SKOLA,levels=c("no_school","primary_school","handicraft_school",
                  "grammar_school","high_school","faculty_PhD_MSc"))

summary(base$B1)
base$B1<-factor(B1,levels=c("health_excellent","health_very good","health_good","health_fair","health_poor" ))
summary(base$B1)
#...hacer lo mismo para las otras variables categ�ricas   Imperativo!
# no cambia los resultados pero facilita la lectura y el manejo de ciertos comandos

summary(base[,1:3],maxsum=100)
summary(base[,4:13])


# Crear una nueva variable cruzando nivel de estudios y edad_en_clases
SKOLAbis<-base$SKOLA
levels(SKOLAbis)<-c("faculty-high-school","faculty-high-school","gram-handi-school","gram-handi-school","primary-no-school","primary-no-school")          
summary(SKOLAbis)
Edadcla<-base$Edad_classe
summary(Edadcla)
levels(Edadcla)<-c("18-35 a�os","18-35 a�os","36-55 a�os","36-55 a�os","56 y m�s","56 y m�s","56 y m�s")
summary(Edadcla)

JovHigh<-which((Edadcla=="18-35 a�os") & (SKOLAbis=="faculty-high-school"))
JovHigh
JovMedium<-which((Edadcla=="18-35 a�os") & (SKOLAbis=="gram-handi-school"))
JovLow<-which((Edadcla=="18-35 a�os") & (SKOLAbis=="primary-no-school"))
MidHigh<-which((Edadcla=="36-55 a�os") & (SKOLAbis=="faculty-high-school"))
MidMedium<-which((Edadcla=="36-55 a�os") & (SKOLAbis=="gram-handi-school"))
MidLow<-which((Edadcla=="36-55 a�os") & (SKOLAbis=="primary-no-school"))
OldHigh<-which((Edadcla=="56 y m�s") & (SKOLAbis=="faculty-high-school"))
OldMedium<-which((Edadcla=="56 y m�s") & (SKOLAbis=="gram-handi-school"))
OldLow<-which((Edadcla=="56 y m�s") & (SKOLAbis=="primary-no-school"))
Edad_Skol<-factor()
levels(Edad_Skol)<-c("JovHigh","JovMedium","JovLow","MidHigh","MidMedium","MidLow","OldHigh","OldMedium","OldLow")
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


#  Profiling  =============================================================================================================


#### Describir variables privilegiadas (cuantitativas o cualitativas)
summary(base[,52])
condes(base,num.var=52)   # caracterizar el score global de salud

summary(base[,5])
catdes(base,num.var=5)   # caracterizar el sexo

#Gabriel
#catdes(base[,c(4:6, 45, 49)], num, var=2)	#describir variable categorica B1



