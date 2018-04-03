rm(list = ls(all = TRUE))


library(rjson) #manejo de json files
library(Rfacebook) #scraping de facebook
library(reshape2) #manipulaci?n de data frames
library(dplyr) #manipulaci?n de data frames
library(stringir) #manipulaci?n de data frames
library(tidytext) #text mining que utiliza 'dplyr'
library(ggplot2) #gr?ficos
library(grid) #mas de un gr?fico por marco
library(tm) #text mining
library(wordcloud) #nube de palabras
library(tidyr) #funciones especiales para tidy
library(igraph) #network analysis
library(ggraph) #grafica redes
library(topicmodels) #topic models


# meta0 <- read.table("~/Clientes/Oi/Round3/nomearquivos/PART 5 - NOMEARQUIVOS.txt",sep = "|", header = FALSE, as.is = TRUE
#                              ,fill = TRUE ,colClasses = c("character"),encoding = 'UTF-8')
# 
# origen<-'PART 5 - NOMEARQUIVOS.txt'
# meta0$origin<-origen

setwd("~/Clientes/Oi/Round3/nomearquivos/")

###########################################################################
##ARCHIVOS DE METADATOS
###########################################################################

# Lectura_AudioFiles <- function(ruta,archivo) 
# {
#   meta.crudo <- read.table(    paste (ruta,archivo, sep = "") ,sep = ";", header = TRUE, as.is = TRUE
#                                ,fill = TRUE ,colClasses = c("character"),encoding = 'UTF-8')
  
  
meta <- data.frame(V1=factor(),V2=factor(),V3=factor(),origin=factor(),stringsAsFactors=FALSE)

##CREA UNA LISTA CON LOS NOMBRES DE LOS ARCHIVOS
text_files <- list.files(pattern = "*.txt")

##APLICA EL PROCESO A CADA ARCHIVO
for(filename in text_files){
  meta0<-read.table(filename,sep = "|", header = FALSE, as.is = TRUE
                    ,fill = TRUE ,colClasses = c("character"),encoding = 'UTF-8')
  meta0$origin<-filename
  meta<-rbind(meta,meta0)
  remove(meta0)
}

#Separa las variables útiles
meta.crudo1<-meta[,c('V2','V3','origin')]

#Separa la V2 por '_' en múltiples variables
meta.crudo1$len <- sapply(gregexpr("_", meta.crudo1$V2), length) + 1

meta.crudo2 <- data.frame(cbind(meta.crudo1,str_split_fixed(meta.crudo1$V2, "_", max(meta.crudo1$len))))

meta.crudo3<-meta.crudo2[,c('V3','origin','X3','X4','X10','X11','X12')]
names(meta.crudo3)[names(meta.crudo3) == 'V3'] <- 'audio_file'
names(meta.crudo3)[names(meta.crudo3) == 'X3'] <- 'CLS1'
names(meta.crudo3)[names(meta.crudo3) == 'X4'] <- 'CLS2'

#funcion
is.letter <- function(x) grepl("[[:alpha:]]", x)

#Reacomoda las variables restantes
meta.crudo3$startDate<-ifelse(is.letter(substr(meta.crudo3$X10,1,1))==TRUE, 
                             as.character(meta.crudo3$X10),as.character(meta.crudo3$X11))

meta.crudo3$endDate<-ifelse(is.letter(substr(meta.crudo3$X10,1,1))==TRUE, 
                             as.character(meta.crudo3$X11),as.character(meta.crudo3$X12))

meta.crudo4<-meta.crudo3[,c('audio_file',"CLS1","CLS2","startDate","endDate")]


###########################
##Trabaja la fecha de inicio
#Extrae el mes
meta.crudo4$pos1 <- regexpr(' ', trimws(meta.crudo4$startDate))
meta.crudo4$mes1 <- trimws(substr(meta.crudo4$startDate,1,meta.crudo4$pos1))
meta.crudo4$f1 <- trimws(substr(meta.crudo4$startDate,meta.crudo4$pos1+1,nchar(meta.crudo4$startDate)))

#Convierte a mes numerico
meta.crudo4$mes <- match(meta.crudo4$mes1,month.abb)

#Extrae el dia
meta.crudo4$pos2 <- regexpr(' ', trimws(meta.crudo4$f1))
meta.crudo4$dia <- trimws(substr(meta.crudo4$f1,1,meta.crudo4$pos2))
meta.crudo4$f2 <- trimws(substr(meta.crudo4$f1,meta.crudo4$pos2+1,nchar(meta.crudo4$f1)))
  
#Extrae el anio
meta.crudo4$pos3 <- regexpr(' ', trimws(meta.crudo4$f2))
meta.crudo4$anio <- trimws(substr(meta.crudo4$f2,1,meta.crudo4$pos3))
meta.crudo4$f3 <- trimws(substr(meta.crudo4$f2,meta.crudo4$pos3+1,nchar(meta.crudo4$f2)))

#Extrae la hora
meta.crudo4$pos4 <- regexpr(' ', trimws(meta.crudo4$f3))
meta.crudo4$hora <- trimws(substr(meta.crudo4$f3,1,meta.crudo4$pos4))
meta.crudo4$f4 <- trimws(substr(meta.crudo4$f3,meta.crudo4$pos4+1,nchar(meta.crudo4$f3)))

#Extrae los minutos y am-pm
meta.crudo4$minuto <- trimws(substr(meta.crudo4$f4,1,2))
meta.crudo4$am_pm <- trimws(substr(meta.crudo4$f4,3,4))

#Crea el campo fecha
meta.crudo4$fecha<-do.call(paste, c(meta.crudo4[c("anio", "mes", "dia")], sep = "-")) 

#Convierte a hora 24
meta.crudo4$hora2<-with(meta.crudo4,
                        ifelse(meta.crudo4$am_pm=='AM'& meta.crudo4$hora=='12', as.integer(meta.crudo4$hora)-12,
                               ifelse(meta.crudo4$am_pm=='PM'& meta.crudo4$hora=='12', as.integer(meta.crudo4$hora),
                                      as.integer(meta.crudo4$hora)+12))
)

#Crea el campo hora-min-seg
meta.crudo4$hora3<-do.call(paste, c(meta.crudo4[c("hora2", "minuto")], "00.000", sep = ":"))

#Crea el timestamp de inicio

meta.crudo4$startTime<-do.call(paste, c(meta.crudo4[c("fecha", "hora3")], sep = " "))

meta.crudo5<-meta.crudo4[,c('audio_file',"CLS1","CLS2","startTime","endDate")]

#########################
##Trabaja la fecha de fin
#Extrae el mes
meta.crudo5$pos1 <- regexpr(' ', trimws(meta.crudo5$endDate))
meta.crudo5$mes1 <- trimws(substr(meta.crudo5$endDate,1,meta.crudo5$pos1))
meta.crudo5$f1 <- trimws(substr(meta.crudo5$endDate,meta.crudo5$pos1+1,nchar(meta.crudo5$endDate)))

#Convierte a mes numerico
meta.crudo5$mes <- match(meta.crudo5$mes1,month.abb)

#Extrae el dia
meta.crudo5$pos2 <- regexpr(' ', trimws(meta.crudo5$f1))
meta.crudo5$dia <- trimws(substr(meta.crudo5$f1,1,meta.crudo5$pos2))
meta.crudo5$f2 <- trimws(substr(meta.crudo5$f1,meta.crudo5$pos2+1,nchar(meta.crudo5$f1)))
  
#Extrae el anio
meta.crudo5$pos3 <- regexpr(' ', trimws(meta.crudo5$f2))
meta.crudo5$anio <- trimws(substr(meta.crudo5$f2,1,meta.crudo5$pos3))
meta.crudo5$f3 <- trimws(substr(meta.crudo5$f2,meta.crudo5$pos3+1,nchar(meta.crudo5$f2)))

#Extrae la hora
meta.crudo5$pos4 <- regexpr(' ', trimws(meta.crudo5$f3))
meta.crudo5$hora <- trimws(substr(meta.crudo5$f3,1,meta.crudo5$pos4))
meta.crudo5$f4 <- trimws(substr(meta.crudo5$f3,meta.crudo5$pos4+1,nchar(meta.crudo5$f3)))

#Extrae los minutos y am-pm
meta.crudo5$minuto <- trimws(substr(meta.crudo5$f4,1,2))
meta.crudo5$am_pm <- trimws(substr(meta.crudo5$f4,3,4))

#Crea el campo fecha
meta.crudo5$fecha<-do.call(paste, c(meta.crudo5[c("anio", "mes", "dia")], sep = "-")) 

#Convierte a hora 24
meta.crudo5$hora2<-with(meta.crudo5,
                        ifelse(meta.crudo5$am_pm=='AM'& meta.crudo5$hora=='12', as.integer(meta.crudo5$hora)-12,
                               ifelse(meta.crudo5$am_pm=='PM'& meta.crudo5$hora=='12', as.integer(meta.crudo5$hora),
                                      as.integer(meta.crudo5$hora)+12))
)

#Crea el campo hora-min-seg
meta.crudo5$hora3<-do.call(paste, c(meta.crudo5[c("hora2", "minuto")], "00.000", sep = ":"))

#Crea el timestamp de fin

meta.crudo5$endTime<-do.call(paste, c(meta.crudo5[c("fecha", "hora3")], sep = " "))

metadata<-meta.crudo5[,c('audio_file',"CLS1","CLS2","startTime","endTime")]

lista<-c('meta','meta.crudo1','meta.crudo2','meta.crudo3','meta.crudo4','meta.crudo5','filename'
         ,'text_files','is.letter')

rm(list=lista)
rm(lista)

#return(metadata)
#}


###########################################################################
##PROCESAMIENTO DE LAS CATEGORIAS DE CADA NIVEL
###########################################################################
Lectura_Hierarquia <- function(ruta,archivo) 
{
jerarq.crudo <- read.table(paste (ruta,archivo, sep = ""),sep = ";", header = TRUE, as.is = TRUE
                           ,fill = TRUE ,colClasses = c("character"),encoding = 'UTF-8')
return(jerarq.crudo)
}

###########################################################################
##ARCHIVOS DE TARGET
###########################################################################

Lectura_Target <- function(ruta,archivo) 
{
 
  target.crudo <- read.table(paste (ruta,archivo, sep = ""),sep = ";", header = TRUE, as.is = TRUE
                             ,fill = TRUE ,colClasses = c("character"),encoding = 'UTF-8')
  
  #Separa las variables importantes
  target.nivel1<-target.crudo[,c('CLS','NIVEL_I')]
  
  #Separa las variables importantes
  target.nivel2<-target.crudo[,c('CLS','NIVEL_II')]
  
  #Separa las variables importantes
  target.nivel3<-target.crudo[,c('CLS','NIVEL_III')]
  
  return(target.crudo)
}












###########################################################################
##ARCHIVOS DE SPEECH TO TEXT
###########################################################################

##CREA LA ESTRUCTURA DEL DATA FRAME DE PALABRAS DONDE SE INSERTARÁN
##LOS RESULTADOS DE CADA ARCHIVO ANALIZADO

setwd("~/Clientes/Oi/Round3/speech_to_text_b1-3/")

base <- data.frame(V1=factor(),V2=factor(),V3=factor(),V4=factor(),V5=factor(),stringsAsFactors=FALSE)

##CREA UNA LISTA CON LOS NOMBRES DE LOS ARCHIVOS DISPONIBLES
audio_files <- list.files(pattern = "*")
  
##APLICA EL PROCESO A CADA ARCHIVO

for(filename in audio_files){
  
  ##LEE EL ARCHIVO
  base0<-read.table(filename,sep = ",", header = FALSE, as.is = TRUE
                    ,fill = TRUE ,colClasses = c("character"),encoding = 'UTF-8')
  ##ACUMULA EL RESULTADO EN LA BASE
  base<-rbind(base,base0)
  ##ELIMINA FILES INNECESARIAS
  remove(base0)
}


base1<-base[,c('V1','V2')]
colnames(base1)<-c('doc_id','text')

write.csv(base1,"~/Clientes/Oi/Round3/audioFilesFinal.csv")

base2<-base1

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

#dtm <- DocumentTermMatrix(corp.stemd)
dtm <- DocumentTermMatrix(corp.lower)

##Elimina los términos menos frecuentes

#dtm.Sparse<-removeSparseTerms(dtm,0.7)

##LLEVA EL DATAFRAME AL FORMATO TIDY Y TRASPONE LA MATRIZ DE PALABRAS

#dtm.tidy <-tidy(dtm.Sparse)
dtm.tidy <-tidy(dtm)
#dtm.tidy<-as.data.frame(as.matrix(dtm), stringsAsFactors=False)

dtm.tidy$marca<-1

colnames(dtm.tidy)<-c('key_0','term','count','marca')

##Guarda la matriz creada hasta ahora

write.csv(dtm.tidy,"~/Clientes/Oi/Round3/dtm_tidy.csv")
