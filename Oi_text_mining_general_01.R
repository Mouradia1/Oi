

##ESTABLECE EL DIRECTORIO DE TRABAJO
setwd("/Users/em185043/Documents/Teradata/3.-Telcos/Oi/Classification")
##setwd("~/Clientes/Oi/")

##ESTABLECE VARIABLES
vruta <- "/Users/em185043/Documents/Teradata/3.-Telcos/Oi/Classification/source/"
vaudiofiles <- "AudioFiles.csv"
vhierarquia <- "Hierarquia.csv"
vtarget <- "PerfilCREP.csv"

##CARGAMOS INFORMACION

##METADATA
vmetadata = Lectura_AudioFiles(vruta,vaudiofiles)
write.csv(vmetadata,paste(vruta, "metadata.csv" ,sep=""),row.names=FALSE)

##JERARQUIA
vjerarq.crudo <- Lectura_Hierarquia(vruta,vhierarquia)

##TARGET
vtarget.crudo<-Lectura_Target(vruta,vtarget)
#Separa las variables importantes
vtarget.nivel1<-vtarget.crudo[,c('CLS','NIVEL_I')]

#Separa las variables importantes
vtarget.nivel2<-vtarget.crudo[,c('CLS','NIVEL_II')]

#Separa las variables importantes
vtarget.nivel3<-vtarget.crudo[,c('CLS','NIVEL_III')]

