rm(list = ls(all = TRUE))

library(rjson) #manejo de json files
library(Rfacebook) #scraping de facebook
library(reshape2) #manipulación de data frames
library(dplyr) #manipulación de data frames
library(tidytext) #text mining que utiliza 'dplyr'
library(ggplot2) #gráficos
library(grid) #mas de un gráfico por marco
library(tm) #text mining
library(wordcloud) #nube de palabras
library(tidyr) #funciones especiales para tidy
library(igraph) #network analysis
library(ggraph) #grafica redes
library(topicmodels) #topic models

##ESTABLECE EL DIRECTORIO DE TRABAJO
setwd("~/Clientes/Oi/")


###########################################################################
##ARCHIVOS DE METADATOS
###########################################################################

# #Captura de los txt que acompañan al chunk de audio files
# 
# meta.crudo <- read.table("~/Clientes/Oi/Round3/NOMEARQUIVOS.txt",sep = "|", as.is = TRUE)
# 
# #Nombre del archivo de audio
# meta.crudo$audio_file<-meta.crudo$V3
# 
# #Separa la zona del texto donde estan las fechas
# meta.crudo$pos1 <- regexpr(' ', trimws(meta.crudo$V2))
# meta.crudo$f1 <- trimws(substr(meta.crudo$V2,meta.crudo$pos1-3,nchar(meta.crudo$V2)))
# 
# #Separa la zona del texto que contiene fecha y hora
# meta.crudo$pos2 <- regexpr('_', trimws(meta.crudo$f1))
# meta.crudo$f2 <- trimws(substr(meta.crudo$f1,meta.crudo$pos2+1,nchar(meta.crudo$f1)))
# 
# #Extrae el mes
# meta.crudo$pos3 <- regexpr(' ', trimws(meta.crudo$f2))
# meta.crudo$mes1 <- trimws(substr(meta.crudo$f2,1, meta.crudo$pos3))
# meta.crudo$f3 <- trimws(substr(meta.crudo$f2,meta.crudo$pos3+1,nchar(meta.crudo$f2)))
# 
# #Convierte a mes numerico
# meta.crudo$mes <- match(meta.crudo$mes1,month.abb)
# 
# #Extrae el dia
# meta.crudo$pos4 <- regexpr(' ', trimws(meta.crudo$f3))
# meta.crudo$dia <- trimws(substr(meta.crudo$f3,1, meta.crudo$pos4))
# meta.crudo$f4 <- trimws(substr(meta.crudo$f3,meta.crudo$pos4+1,nchar(meta.crudo$f3)))
# 
# #Extrae el anio
# meta.crudo$pos5 <- regexpr(' ', trimws(meta.crudo$f4))
# meta.crudo$anio <- trimws(substr(meta.crudo$f4,1, meta.crudo$pos5))
# meta.crudo$f5 <- trimws(substr(meta.crudo$f4,meta.crudo$pos5+1,nchar(meta.crudo$f4)))
# 
# #Extrae la hora
# meta.crudo$pos6 <- regexpr(' ', trimws(meta.crudo$f5))
# meta.crudo$hora1 <- trimws(substr(meta.crudo$f5,1, meta.crudo$pos6))
# meta.crudo$f6 <- trimws(substr(meta.crudo$f5,meta.crudo$pos6+1,nchar(meta.crudo$f5)))
# 
# #Extrae los minutos
# meta.crudo$minuto <- trimws(substr(meta.crudo$f6,1,2))
# meta.crudo$am_pm <- trimws(substr(meta.crudo$f6,3,4))
# 
# #Extrae el ultimo grupo de numeros que parecen ser correlativos
# meta.crudo$correlativo<-gsub(pattern = ".nmf",replacement = "",
#                              trimws(substr(meta.crudo$f6,6,nchar(meta.crudo$f6))),fixed = TRUE)
# 
# #Crea el campo fecha
# meta.crudo$fecha<-do.call(paste, c(meta.crudo[c("anio", "mes", "dia")], sep = "-")) 
# 
# #Convierte a hora 24
# meta.crudo$hora2<-with(meta.crudo,
#      ifelse(meta.crudo$am_pm=='AM'& meta.crudo$hora1=='12', as.integer(meta.crudo$hora1)-12,
#           ifelse(meta.crudo$am_pm=='PM'& meta.crudo$hora1=='12', as.integer(meta.crudo$hora1),
#                  as.integer(meta.crudo$hora1)+12))
# )
# 
# #Crea el campo hora-min-seg
# meta.crudo$hora<-do.call(paste, c(meta.crudo[c("hora2", "minuto")], "00.000", sep = ":"))
# 
# metadata<-meta.crudo[,c('audio_file','correlativo','fecha','hora')]

#Captura del excel que nos enviaron ya procesado (desde un csv pq es + eficiente)

meta.crudo <- read.table("~/Clientes/Oi/Round3/AudioFiles.csv",sep = ";", header = TRUE, as.is = TRUE
                         ,fill = TRUE ,colClasses = c("character"),encoding = 'UTF-8')

#Separa las variables importantes
meta.crudo1<-meta.crudo[,c('Var5','Var13','Var14','Var15','Var16')]
colnames(meta.crudo1)<-c('key','startDate','endDate','originalFile','finalFile')

############################
##Trabaja la fecha de inicio
#Extrae el mes
meta.crudo1$pos1 <- regexpr(' ', trimws(meta.crudo1$startDate))
meta.crudo1$mes1 <- trimws(substr(meta.crudo1$startDate,1,meta.crudo1$pos1))
meta.crudo1$f1 <- trimws(substr(meta.crudo1$startDate,meta.crudo1$pos1+1,nchar(meta.crudo1$startDate)))

#Convierte a mes numerico
meta.crudo1$mes <- match(meta.crudo1$mes1,month.abb)

#Extrae el dia
meta.crudo1$pos2 <- regexpr(' ', trimws(meta.crudo1$f1))
meta.crudo1$dia <- trimws(substr(meta.crudo1$f1,1,meta.crudo1$pos2))
meta.crudo1$f2 <- trimws(substr(meta.crudo1$f1,meta.crudo1$pos2+1,nchar(meta.crudo1$f1)))

#Extrae el anio
meta.crudo1$pos3 <- regexpr(' ', trimws(meta.crudo1$f2))
meta.crudo1$anio <- trimws(substr(meta.crudo1$f2,1,meta.crudo1$pos3))
meta.crudo1$f3 <- trimws(substr(meta.crudo1$f2,meta.crudo1$pos3+1,nchar(meta.crudo1$f2)))

#Extrae la hora
meta.crudo1$pos4 <- regexpr(' ', trimws(meta.crudo1$f3))
meta.crudo1$hora <- trimws(substr(meta.crudo1$f3,1,meta.crudo1$pos4))
meta.crudo1$f4 <- trimws(substr(meta.crudo1$f3,meta.crudo1$pos4+1,nchar(meta.crudo1$f3)))

#Extrae los minutos y am-pm
meta.crudo1$minuto <- trimws(substr(meta.crudo1$f4,1,2))
meta.crudo1$am_pm <- trimws(substr(meta.crudo1$f4,3,4))

#Crea el campo fecha
meta.crudo1$fecha<-do.call(paste, c(meta.crudo1[c("anio", "mes", "dia")], sep = "-")) 
 
#Convierte a hora 24
meta.crudo1$hora2<-with(meta.crudo1,
     ifelse(meta.crudo1$am_pm=='AM'& meta.crudo1$hora=='12', as.integer(meta.crudo1$hora)-12,
          ifelse(meta.crudo1$am_pm=='PM'& meta.crudo1$hora=='12', as.integer(meta.crudo1$hora),
                 as.integer(meta.crudo1$hora)+12))
)

#Crea el campo hora-min-seg
meta.crudo1$hora3<-do.call(paste, c(meta.crudo1[c("hora2", "minuto")], "00.000", sep = ":"))

#Crea el timestamp de inicio

meta.crudo1$startTime<-do.call(paste, c(meta.crudo1[c("fecha", "hora3")], sep = " "))

meta.crudo2<-meta.crudo1[,c('key','startTime','endDate','originalFile','finalFile')]

#########################
##Trabaja la fecha de fin
#Extrae el mes
meta.crudo2$pos1 <- regexpr(' ', trimws(meta.crudo2$endDate))
meta.crudo2$mes1 <- trimws(substr(meta.crudo2$endDate,1,meta.crudo2$pos1))
meta.crudo2$f1 <- trimws(substr(meta.crudo2$endDate,meta.crudo2$pos1+1,nchar(meta.crudo2$endDate)))

#Convierte a mes numerico
meta.crudo2$mes <- match(meta.crudo2$mes1,month.abb)

#Extrae el dia
meta.crudo2$pos2 <- regexpr(' ', trimws(meta.crudo2$f1))
meta.crudo2$dia <- trimws(substr(meta.crudo2$f1,1,meta.crudo2$pos2))
meta.crudo2$f2 <- trimws(substr(meta.crudo2$f1,meta.crudo2$pos2+1,nchar(meta.crudo2$f1)))

#Extrae el anio
meta.crudo2$pos3 <- regexpr(' ', trimws(meta.crudo2$f2))
meta.crudo2$anio <- trimws(substr(meta.crudo2$f2,1,meta.crudo2$pos3))
meta.crudo2$f3 <- trimws(substr(meta.crudo2$f2,meta.crudo2$pos3+1,nchar(meta.crudo2$f2)))

#Extrae la hora
meta.crudo2$pos4 <- regexpr(' ', trimws(meta.crudo2$f3))
meta.crudo2$hora <- trimws(substr(meta.crudo2$f3,1,meta.crudo2$pos4))
meta.crudo2$f4 <- trimws(substr(meta.crudo2$f3,meta.crudo2$pos4+1,nchar(meta.crudo2$f3)))

#Extrae los minutos y am-pm
meta.crudo2$minuto <- trimws(substr(meta.crudo2$f4,1,2))
meta.crudo2$am_pm <- trimws(substr(meta.crudo2$f4,3,4))

#Crea el campo fecha
meta.crudo2$fecha<-do.call(paste, c(meta.crudo2[c("anio", "mes", "dia")], sep = "-")) 

#Convierte a hora 24
meta.crudo2$hora2<-with(meta.crudo2,
                        ifelse(meta.crudo2$am_pm=='AM'& meta.crudo2$hora=='12', as.integer(meta.crudo2$hora)-12,
                               ifelse(meta.crudo2$am_pm=='PM'& meta.crudo2$hora=='12', as.integer(meta.crudo2$hora),
                                      as.integer(meta.crudo2$hora)+12))
)

#Crea el campo hora-min-seg
meta.crudo2$hora3<-do.call(paste, c(meta.crudo2[c("hora2", "minuto")], "00.000", sep = ":"))

#Crea el timestamp de fin

meta.crudo2$endTime<-do.call(paste, c(meta.crudo2[c("fecha", "hora3")], sep = " "))

metadata<-meta.crudo2[,c('key','startTime','endTime','originalFile','finalFile')]

#Guerda el archivo
write.csv(metadata,"~/Clientes/Oi/Round3/metadata.csv",row.names=FALSE)


limpia<-c('meta.crudo','meta.crudo1','meta.crudo2')

rm(list=limpia)

###########################################################################
##PROCESAMIENTO DE LAS CATEGORIAS DE CADA NIVEL
###########################################################################

# jerarq.crudo <- read.table("~/Clientes/Oi/Round3/Hierarquia.csv",sep = ";", header = TRUE, as.is = TRUE
#                            ,fill = TRUE ,colClasses = c("character"),encoding = 'UTF-8')


##ddply(dt,~group,summarise,mean=mean(age),sd=sd(age))


###########################################################################
##ARCHIVOS DE TARGET
###########################################################################

# target.crudo <- read.table("~/Clientes/Oi/Round3/PerfilCREP.csv",sep = ";", header = TRUE, as.is = TRUE
#                          ,fill = TRUE ,colClasses = c("character"),encoding = 'UTF-8')
# 
# #Separa las variables importantes
# target.nivel1<-target.crudo[,c('CLS','NIVEL_I')]
# 
# #Separa las variables importantes
# target.nivel2<-target.crudo[,c('CLS','NIVEL_II')]
# 
# #Separa las variables importantes
# target.nivel3<-target.crudo[,c('CLS','NIVEL_III')]


###########################################################################
##ARCHIVOS DE SPEECH TO TEXT
###########################################################################

# ##CREA LA ESTRUCTURA DEL DATA FRAME DE PALABRAS DONDE SE INSERTARÁN
# ##LOS RESULTADOS DE CADA ARCHIVO ANALIZADO
# 
# base <- data.frame(name=factor(),
#                    startTime=factor(),
#                    respuestas=factor(),
#                           stringsAsFactors=FALSE)
# 
# ##CREA UNA LISTA CON LOS NOMBRES DE LOS ARCHIVOS JSON DISPONIBLES
# 
# json_files <- list.files(pattern = "*.json$")
# 
# ##APLICA EL PROCESO A CADA ARCHIVO
# 
# for(filename in json_files){
# 
#   crudo<-as.data.frame(fromJSON(file=filename))
# 
#   ##LIMPIA, SELECCIONA Y SEPARA EL TEXTO CAPTURADO
# 
#   base0<-data.frame(crudo[,1],crudo[,4],
#                     apply(crudo[grepl("transcript", names(crudo))],1,paste,collapse=" "))
#   colnames(base0)<-c('name','startTime','respuestas')
#   rownames(base0)<-NULL
# 
#   base<-rbind(base,base0)
# 
#   ##ELIMINA FILES INNECESARIAS
#   remove(base0,crudo)
# }
# 
# write.csv(base,"~/Clientes/Oi/base03.csv")


##CREA LA ESTRUCTURA DEL DATA FRAME DE PALABRAS DONDE SE INSERTARÁN
##LOS RESULTADOS DE CADA ARCHIVO ANALIZADO

setwd("~/Clientes/Oi/Round3/b1/")

base <- data.frame(V1=factor(),V2=factor(),V3=factor(),V4=factor(),V5=factor(),stringsAsFactors=FALSE)

##CREA UNA LISTA CON LOS NOMBRES DE LOS ARCHIVOS JSON DISPONIBLES

audio_files <- list.files(pattern = "*")

##APLICA EL PROCESO A CADA ARCHIVO

for(filename in audio_files){
  
  ##LIMPIA, SELECCIONA Y SEPARA EL TEXTO CAPTURADO
  
  base0<-read.table(filename,sep = ",", header = FALSE, as.is = TRUE
                    ,fill = TRUE ,colClasses = c("character"),encoding = 'UTF-8')
  
  base<-rbind(base,base0)
  
  ##ELIMINA FILES INNECESARIAS
  remove(base0)
}

base1<-base[,c('V1','V2','V5')]
colnames(base1)<-c('finalFile','text','originalFile')

write.csv(base1,"~/Clientes/Oi/Round3/audioFilesFinal.csv")

base2<-base[,c('V1','V2')]
colnames(base2)<-c('doc_id','text')

base2$text<- removeNumbers(base2$text)

###########################################################################
##MANIPULACION DE DATOS
###########################################################################

##GENERA UN CORPUS CON LOS DATOS

corp.copy<-corp <- Corpus(DataframeSource(base2))

corp.lower <- tm_map(corp, content_transformer(tolower))
corp.stopw <- tm_map(corp.lower, removeWords, stopwords("portuguese"))
corp.stemd <- tm_map(corp.stopw, stemDocument, language = "portuguese") 
##corp.final <- tm_map(corp.stemd, stemCompletion, dictionary = corp.stopw)  

##inspect(corp.stemd)

dtm <- DocumentTermMatrix(corp.stemd)

##Elimina los términos menos frecuentes

#dtm.Sparse<-removeSparseTerms(dtm,0.7)

##LLEVA EL DATAFRAME AL FORMATO TIDY Y TRASPONE LA MATRIZ DE PALABRAS

#dtm.tidy <-tidy(dtm.Sparse)
dtm.tidy <-tidy(dtm)
#dtm.tidy<-as.data.frame(as.matrix(dtm), stringsAsFactors=False)

dtm.tidy$marca<-1

colnames(dtm.tidy)<-c('key_0','term','count','marca')

dtm.wide <- spread(dtm.tidy, term, marca)
dtm.wide[is.na(dtm.wide)] <- 0
dtm.FlagMatrix<-aggregate(. ~ key_0, dtm.wide, max)

##Guarda las diferentes matrices creadas hasta ahora

write.csv(dtm.tidy,"~/Clientes/Oi/Round3/dtm_tidy.csv")
write.csv(dtm.FlagMatrix,"~/Clientes/Oi/Round3/dtm_FlagMatrix.csv")


