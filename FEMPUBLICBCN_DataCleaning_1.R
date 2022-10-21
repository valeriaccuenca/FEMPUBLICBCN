########################
####0. borramos todo lo que potencialmente tengamos abierto en R/R-Studio####
########################
rm(list=ls(all=T))
########################
####1. Establecemos que queremos ver hasta 10000000 al printar resultados####
########################
options(max.print=1000000)
########################
####2. definimos las carpetas con las que trabajaremos - debemos incluir \\ en vez de \ como separadores####
########################
getwd()
mainData <- "C:\\Users\\valer\\Documents\\Máster SP\\Segundo curso\\TFM\\Data Cleaning"
outputs <- "C:\\Users\\valer\\Documents\\Máster SP\\Segundo curso\\TFM\\Data Cleaning"
#1.1. Le indicamos a R donde tenemos este archivo utilizando los objetos que hemos definido en 1.
setwd(mainData)
getwd()
#1.2. Abrimos el archivo excel
#install.packages("readxl") #esto solo debe correrse la primera vez que se quiere instalar este paquete. Después de haberlo instalado podeis colocar "#" delante de esta linea
##install.packages("readxl")
library(readxl)
## Español
dbSpanish<-as.data.frame(read_excel("FEMPUBLICBCN_-_ESPAÑOL_clean.xlsx"))  
## Inglés
dbEnglish<-as.data.frame(read_excel("FEMPUBLICBCN_-_ENGLISH_final_version.xlsx"))  
## Clone Español
dbSpanishClone<-as.data.frame(read_excel("Clone_of_FEMPUBLICBCN_-_ESPAÑOL_final_version.xlsx"))  
## Catalan
dbCatalan<-as.data.frame(read_excel("FEMPUBLICBCN_-_CATALAN_final_version.xlsx"))  


####******************************#####
#######1.1.PENSAMOS LAS VARIABLES QUE VAMOS A QUERER PARA ANALIZAR#### 
####******************************#####

# en este caso, queremos TODAS
##*Cambios visitas A, B, C, D
##*Cambios cambios tiempo A, B, C, D
##*Cambios actividades (?)
##*ANTES Y DESPUÉS PARA TODAS LAS SIGUIENTES
##*Servicios, echen, desalojen, delincuencia, ricos, redes, comunidad, fuera, renovación, no bienvenido
##*Turistas, hoteles
##*Cuidados
##*Salud
##*Concentrarse, sueño, papel, decisiones, tensión, superar, disfrutar, problemas, deprimida, confianza, valer, feliz
##*Dormir
##*Género (no hay antes)
##*Nacimiento (no hay antes)
##*Estudios (no hay antes)
##*Monetaria
##*Laboral
##*
####******************************#####
#######1.2.VAMOS A CREAR UNA BASE DE DATOS JUNTANDO TODAS LAS BASES DE DATOS PERO SOLO DE LAS VARIABLES DE INTERÉS#### 
####******************************#####


###1.2.4.Creamos nueva variable####
head(dbSpanish)
names(dbSpanish) == names(dbEnglish) 
head(dbEnglish)
names(dbSpanish) == names(dbCatalan) 
head(dbCatalan)
names(dbSpanish) == names(dbSpanishClone)
head(dbSpanishClone)

### PRIMERO UNIMOS LAS BASES SPANISH Y ENGLISH
dbSpanish_English <- rbind(dbSpanish, dbEnglish)
head(dbSpanish_English)

### SEGUNDO UNIMOS SPANGLISH Y CAT
names(dbSpanish_English) == names(dbCatalan)
dbSpanish_English_Catalan <- rbind(dbSpanish_English, dbCatalan)
head(dbSpanish_English_Catalan)

### POR ÚLTIMO, UNIMOS CATSPANGLISH CON SPANISH CLONE
names(dbSpanish_English_Catalan) == names(dbSpanishClone)
DB_FPBCN <- rbind(dbSpanish_English_Catalan, dbSpanishClone)
head(DB_FPBCN)

DB_1 <- DB_FPBCN
###1.2.2.Cambiamos las posibles respuestas de las variables####
#*Es decir, necesitamos que las variables tengan las mismas respuestas
#*
#*PRIMERO CHEQUEAMOS CÓMO HAN SIDO GUARDADAS Y DESPUÉS UNIFICAMOS LAS RESPUESTAS
#Edad
table(DB_1$Edad)
DB_1$Edad[DB_1$Edad=="-75"]<- "75"
DB_1$Edad[DB_1$Edad=="8"]<- "80"

#ENTREVISTADOR  
table(DB_1$Entrevistador)
DB_1$Entrevistador[DB_1$Entrevistador=="Hanne\r\n"]<- "Hanne"
DB_1$Entrevistador[DB_1$Entrevistador=="Sarah\r\n"]<- "Sarah"
DB_1$Entrevistador[DB_1$Entrevistador=="María\r\n"]<- "Maria"
DB_1$Entrevistador[DB_1$Entrevistador=="María"]<- "Maria"
## BARRIO
table(DB_1$Barrio)
DB_1$Barrio[DB_1$Barrio=="Sant_Antoni"]<- "Sant Antoni"
DB_1$Barrio[DB_1$Barrio=="sant_antoni"]<- "Sant Antoni"
DB_1$Barrio[DB_1$Barrio=="barceloneta"]<- "Barceloneta"

####### CAMBIO DEL USO DEL ESPACIO PÚBLICO #####

##### CAMBIO EN VISITAS #####

#SUPERILLA Y PLAYA
table(DB_1$Cambios_visitas_A)
#Disminuido mucho
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="Ha disminuido  mucho"] <- "Disminuido mucho"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="No ha canviat"] <- "No cambio"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="no_cambio"] <- "No cambio"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_visitas_A[DB_1$Cambios_visitas_A=="No aplicable"] <- "NA"

## ESPACIOS A MENOS DE 15 MINUTOS
table(DB_1$Cambios_visitas_B)
#Disminuido mucho
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="Ha disminuido  mucho"] <- "Disminuido mucho"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="No ha canviat"] <- "No cambio"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="no_cambio"] <- "No cambio"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="No aplicable"] <- "NA"
DB_1$Cambios_visitas_B[DB_1$Cambios_visitas_B=="no_aplicable"] <- "NA"

### ESPACIOS A MÁS DE 15 MIN
table(DB_1$Cambios_visitas_C)
#Disminuido mucho
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="Ha disminuido  mucho"] <- "Disminuido mucho"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="ha_disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="No ha canviat"] <- "No cambio"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="no_cambio"] <- "No cambio"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="No aplicable"] <- "NA"
DB_1$Cambios_visitas_C[DB_1$Cambios_visitas_C=="no_aplicable"] <- "NA"

# ESPACIOS EN TORNO A BARCELONA
table(DB_1$Cambios_visitas_D)
#Disminuido mucho
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="Ha disminuido mucho"] <- "Disminuido mucho"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="ha_disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="Ha disminuït"] <- "Disminuido"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="it_has_decreased"] <- "Disminuido"
#No cambio
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="No ha canviat"] <- "No cambio"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="no_cambio"] <- "No cambio"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="no_ha_cambiado"] <- "No cambio"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="it_has_not_changed"] <- "No cambio"
#Aumentado
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="Ha augmentat"] <- "Aumentado"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="it_has_increased"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="Ha augmentat molt"] <- "Aumentado mucho"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="it_has_increased_drastically"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="No aplicable"] <- "NA"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="no_aplicable"] <- "NA"
DB_1$Cambios_visitas_D[DB_1$Cambios_visitas_D=="does_not_apply"] <- "NA"

### COMPROBAMOS QUE ESTÉN TODAS IGUAL
table(DB_1$Cambios_visitas_A)
table(DB_1$Cambios_visitas_B)
table(DB_1$Cambios_visitas_C)
table(DB_1$Cambios_visitas_D)


##### CAMBIO EN TIEMPO #####

#SUPERILLA Y PLAYA
table(DB_1$Cambios_tiempo_A)
#Disminuido mucho
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="Ha disminuido  mucho"] <- "Disminuido mucho"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="No ha canviat"] <- "No cambio"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="no_cambio"] <- "No cambio"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_tiempo_A[DB_1$Cambios_tiempo_A=="No aplicable"] <- "NA"

## ESPACIOS A MENOS DE 15 MINUTOS
table(DB_1$Cambios_tiempo_B)
#Disminuido mucho
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="Ha disminuido  mucho"] <- "Disminuido mucho"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="No ha canviat"] <- "No cambio"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="no_cambio"] <- "No cambio"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="No aplicable"] <- "NA"
DB_1$Cambios_tiempo_B[DB_1$Cambios_tiempo_B=="no_aplicable"] <- "NA"

### ESPACIOS A MÁS DE 15 MIN
table(DB_1$Cambios_tiempo_C)
#Disminuido mucho
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="Ha disminuido  mucho"] <- "Disminuido mucho"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="ha_disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="No ha canviat"] <- "No cambio"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="no_cambio"] <- "No cambio"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="No aplicable"] <- "NA"
DB_1$Cambios_tiempo_C[DB_1$Cambios_tiempo_C=="no_aplicable"] <- "NA"

# ESPACIOS EN TORNO A BARCELONA
table(DB_1$Cambios_tiempo_D)
#Disminuido mucho
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="Ha disminuido mucho"] <- "Disminuido mucho"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="No ha canviat"] <- "No cambio"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="no_cambio"] <- "No cambio"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="No aplicable"] <- "NA"
DB_1$Cambios_tiempo_D[DB_1$Cambios_tiempo_D=="no_aplicable"] <- "NA"

### COMPROBAMOS QUE ESTÉN TODAS IGUAL
table(DB_1$Cambios_tiempo_A)
table(DB_1$Cambios_tiempo_B)
table(DB_1$Cambios_tiempo_C)
table(DB_1$Cambios_tiempo_D)

# CAMBIOS ACTIVIDADES

# PICNICS
table(DB_1$Cambios_picnic)
#Disminuido mucho
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="Ha disminuido mucho"] <- "Disminuido mucho"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="disminuido"] <- "Disminuido"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="No ha canviat"] <- "No cambio"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="no_cambio"] <- "No cambio"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="aumentado"] <- "Aumentado"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="No aplicable"] <- "NA"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="no_aplicable"] <- "NA"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="No aplicable (si la persona ni abans ni ara utilitza l'espai per aquests usos)"] <- "NA"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="No aplicable (si la persona ni antes ni ahora utiliza el espacio para estos usos)"] <- "NA"
DB_1$Cambios_picnic[DB_1$Cambios_picnic=="no_aplicable__si_la_persona_ni_antes_ni_"] <- "NA"
table(DB_1$Cambios_picnic) 

# PASEAR
table(DB_1$Cambios_pasear)
#Disminuido mucho
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="Ha disminuido mucho"] <- "Disminuido mucho"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="Ha disminuït molt"] <- "Disminuido mucho"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="ha_disminuido_mucho"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="disminuido"] <- "Disminuido"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="No ha canviat"] <- "No cambio"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="no_cambio"] <- "No cambio"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="aumentado"] <- "Aumentado"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="No aplicable"] <- "NA"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="no_aplicable"] <- "NA"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="No aplicable (si la persona ni abans ni ara utilitza l'espai per aquests usos)"] <- "NA"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="No aplicable (si la persona ni antes ni ahora utiliza el espacio para estos usos)"] <- "NA"
DB_1$Cambios_pasear[DB_1$Cambios_pasear=="no_aplicable__si_la_persona_ni_antes_ni_"] <- "NA"

table(DB_1$Cambios_pasear)

# TRANQUILIDAD
table(DB_1$Cambios_tranquilidad)
#Disminuido mucho
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="Ha disminuido mucho"] <- "Disminuido mucho"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="ha_disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="disminuido"] <- "Disminuido"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="No ha canviat"] <- "No cambio"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="no_cambio"] <- "No cambio"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="aumentado"] <- "Aumentado"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="No aplicable"] <- "NA"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="no_aplicable"] <- "NA"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="No aplicable (si la persona ni abans ni ara utilitza l'espai per aquests usos)"] <- "NA"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="No aplicable (si la persona ni antes ni ahora utiliza el espacio para estos usos)"] <- "NA"
DB_1$Cambios_tranquilidad[DB_1$Cambios_tranquilidad=="no_aplicable__si_la_persona_ni_antes_ni_"] <- "NA"

table(DB_1$Cambios_tranquilidad)

# REUNIRSE 
table(DB_1$Cambios_reunirse)
#Disminuido mucho
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="Ha disminuido mucho"] <- "Disminuido mucho"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="ha_disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="disminuido"] <- "Disminuido"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="No ha canviat"] <- "No cambio"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="no_cambio"] <- "No cambio"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="aumentado"] <- "Aumentado"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="No aplicable"] <- "NA"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="no_aplicable__si_la_persona_ni_antes_ni_"] <- "NA"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="no_aplicable"] <- "NA"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="No aplicable (si la persona ni abans ni ara utilitza l'espai per aquests usos)"] <- "NA"
DB_1$Cambios_reunirse[DB_1$Cambios_reunirse=="No aplicable (si la persona ni antes ni ahora utiliza el espacio para estos usos)"] <- "NA"

table(DB_1$Cambios_reunirse)

# DEPENDIENTES
table(DB_1$Cambios_dependientes)
#Disminuido mucho
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="Ha disminuido mucho"] <- "Disminuido mucho"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="ha_disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="disminuido"] <- "Disminuido"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="No ha canviat"] <- "No cambio"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="no_cambio"] <- "No cambio"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="aumentado"] <- "Aumentado"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="No aplicable (si la persona ni antes ni ahora utiliza el espacio para estos usos)"] <- "NA"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="No aplicable (si la persona ni abans ni ara utilitza l'espai per aquests usos)"] <- "NA"
DB_1$Cambios_dependientes[DB_1$Cambios_dependientes=="no_aplicable__si_la_persona_ni_antes_ni_"] <- "NA"
table(DB_1$Cambios_dependientes)

# JUGAR
table(DB_1$Cambios_jugar)
#Disminuido mucho
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="Ha disminuido mucho"] <- "Disminuido mucho"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="disminuido"] <- "Disminuido"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="No ha canviat"] <- "No cambio"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="no_cambio"] <- "No cambio"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="aumentado"] <- "Aumentado"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="No aplicable (si la persona ni abans ni ara utilitza l'espai per aquests usos)"] <- "NA"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="No aplicable (si la persona ni antes ni ahora utiliza el espacio para estos usos)"] <- "NA"
DB_1$Cambios_jugar[DB_1$Cambios_jugar=="no_aplicable__si_la_persona_ni_antes_ni_"] <- "NA"

table(DB_1$Cambios_jugar)

# ESTUDIAR
table(DB_1$Cambios_estudiar)
#Disminuido mucho
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="Ha disminuido mucho"] <- "Disminuido mucho"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="disminuido_mucho"] <- "Disminuido mucho"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="Ha disminuït molt"] <- "Disminuido mucho"
#Disminuido
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="Ha disminuido"] <- "Disminuido"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="disminuido"] <- "Disminuido"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="ha_disminuido"] <- "Disminuido"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="Ha disminuït"] <- "Disminuido"
#No cambio
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="No ha cambiado"] <- "No cambio"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="No ha canviat"] <- "No cambio"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="no_cambio"] <- "No cambio"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="no_ha_cambiado"] <- "No cambio"
#Aumentado
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="ha_aumentado"] <- "Aumentado"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="aumentado"] <- "Aumentado"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="Ha aumentado"] <- "Aumentado"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="Ha augmentat"] <- "Aumentado"
#Aumentado mucho
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="ha_aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="aumentado_mucho"] <- "Aumentado mucho"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="Ha aumentado mucho"] <- "Aumentado mucho"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="Ha augmentat molt"] <- "Aumentado mucho"
#NO APLICA
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="No aplicable (si la persona ni abans ni ara utilitza l'espai per aquests usos)"] <- "NA"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="No aplicable (si la persona ni antes ni ahora utiliza el espacio para estos usos)"] <- "NA"
DB_1$Cambios_estudiar[DB_1$Cambios_estudiar=="no_aplicable__si_la_persona_ni_antes_ni_"] <- "NA"

table(DB_1$Cambios_estudiar)

#Guardar base 1 por si la cagamos en los siguientes pasos
DB1_1<- DB_1



##### GENTRIFICACIÓN ANTES
## SERVICIOS
table(DB_1$Antes_servicios)
#Muy de acuerdo
DB_1$Antes_servicios[DB_1$Antes_servicios=="Muy de acuerdo"]<- 5
DB_1$Antes_servicios[DB_1$Antes_servicios=="muy_acuerdo"]<- 5
DB_1$Antes_servicios[DB_1$Antes_servicios=="muy_de_acuerdo"]<- 5
DB_1$Antes_servicios[DB_1$Antes_servicios=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Antes_servicios[DB_1$Antes_servicios=="De acuerdo"]<- 4
DB_1$Antes_servicios[DB_1$Antes_servicios=="acuerdo"]<- 4
DB_1$Antes_servicios[DB_1$Antes_servicios=="de_acuerdo"]<- 4
DB_1$Antes_servicios[DB_1$Antes_servicios=="D'acord"]<- 4
#Neutral
DB_1$Antes_servicios[DB_1$Antes_servicios=="neutral"]<- 3
DB_1$Antes_servicios[DB_1$Antes_servicios=="Neutral"]<- 3
#Desacuerdo
DB_1$Antes_servicios[DB_1$Antes_servicios=="En desacuerdo"]<- 2
DB_1$Antes_servicios[DB_1$Antes_servicios=="desacuerdo"]<- 2
DB_1$Antes_servicios[DB_1$Antes_servicios=="en_desacuerdo"]<- 2
DB_1$Antes_servicios[DB_1$Antes_servicios=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Antes_servicios[DB_1$Antes_servicios=="Muy en desacuerdo"]<- 1
DB_1$Antes_servicios[DB_1$Antes_servicios=="muy_desacuerdo"]<- 1
DB_1$Antes_servicios[DB_1$Antes_servicios=="Molt en desacord"]<- 1
#No aplica
DB_1$Antes_servicios[DB_1$Antes_servicios=="No contesta / No aplica"]<- 0
DB_1$Antes_servicios[DB_1$Antes_servicios=="no_contesta___no_aplica"]<- 0

DB_1$Antes_servicios<-as.numeric(DB_1$Antes_servicios)

#RICOS
table(DB_1$Antes_ricos)
#Muy de acuerdo
DB_1$Antes_ricos[DB_1$Antes_ricos=="Muy de acuerdo"]<- 5
DB_1$Antes_ricos[DB_1$Antes_ricos=="muy_acuerdo"]<- 5
DB_1$Antes_ricos[DB_1$Antes_ricos=="muy_de_acuerdo"]<- 5
DB_1$Antes_ricos[DB_1$Antes_ricos=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Antes_ricos[DB_1$Antes_ricos=="De acuerdo"]<- 4
DB_1$Antes_ricos[DB_1$Antes_ricos=="acuerdo"]<- 4
DB_1$Antes_ricos[DB_1$Antes_ricos=="de_acuerdo"]<- 4
DB_1$Antes_ricos[DB_1$Antes_ricos=="D'acord"]<- 4
#Neutral
DB_1$Antes_ricos[DB_1$Antes_ricos=="neutral"]<- 3
DB_1$Antes_ricos[DB_1$Antes_ricos=="Neutral"]<- 3
#Desacuerdo
DB_1$Antes_ricos[DB_1$Antes_ricos=="En desacuerdo"]<- 2
DB_1$Antes_ricos[DB_1$Antes_ricos=="desacuerdo"]<- 2
DB_1$Antes_ricos[DB_1$Antes_ricos=="en_desacuerdo"]<- 2
DB_1$Antes_ricos[DB_1$Antes_ricos=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Antes_ricos[DB_1$Antes_ricos=="Muy en desacuerdo"]<- 1
DB_1$Antes_ricos[DB_1$Antes_ricos=="muy_desacuerdo"]<- 1
DB_1$Antes_ricos[DB_1$Antes_ricos=="Molt en desacord"]<- 1
#No aplica
DB_1$Antes_ricos[DB_1$Antes_ricos=="No contesta / No aplica"]<- 0
DB_1$Antes_ricos[DB_1$Antes_ricos=="no_contesta___no_aplica"]<- 0
DB_1$Antes_ricos[DB_1$Antes_ricos=="NA"]<- 0

DB_1$Antes_ricos <- as.numeric(DB_1$Antes_ricos)

#DELINCUENCIA
table(DB_1$Antes_delincuencia)
#Muy de acuerdo
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="Muy de acuerdo"]<- 5
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="muy_acuerdo"]<- 5
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="muy_de_acuerdo"]<- 5
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="De acuerdo"]<- 4
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="acuerdo"]<- 4
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="de_acuerdo"]<- 4
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="D'acord"]<- 4
#Neutral
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="neutral"]<- 3
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="Neutral"]<- 3
#Desacuerdo
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="En desacuerdo"]<- 2
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="desacuerdo"]<- 2
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="en_desacuerdo"]<- 2
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="Muy en desacuerdo"]<- 1
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="muy_desacuerdo"]<- 1
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="muy_en_desacuerdo"]<- 1
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="Molt en desacord"]<- 1
#No aplica
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="No contesta / No aplica"]<- 0
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="no_contesta___no_aplica"]<- 0
DB_1$Antes_delincuencia[DB_1$Antes_delincuencia=="NA"]<- 0

DB_1$Antes_delincuencia <- as.numeric(DB_1$Antes_delincuencia)

#ECHEN
table(DB_1$Antes_echen)
#Muy de acuerdo
DB_1$Antes_echen[DB_1$Antes_echen=="Muy de acuerdo"]<- 5
DB_1$Antes_echen[DB_1$Antes_echen=="muy_acuerdo"]<- 5
DB_1$Antes_echen[DB_1$Antes_echen=="muy_de_acuerdo"]<- 5
DB_1$Antes_echen[DB_1$Antes_echen=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Antes_echen[DB_1$Antes_echen=="De acuerdo"]<- 4
DB_1$Antes_echen[DB_1$Antes_echen=="acuerdo"]<- 4
DB_1$Antes_echen[DB_1$Antes_echen=="de_acuerdo"]<- 4
DB_1$Antes_echen[DB_1$Antes_echen=="D'acord"]<- 4
#Neutral
DB_1$Antes_echen[DB_1$Antes_echen=="neutral"]<- 3
DB_1$Antes_echen[DB_1$Antes_echen=="Neutral"]<- 3
#Desacuerdo
DB_1$Antes_echen[DB_1$Antes_echen=="En desacuerdo"]<- 2
DB_1$Antes_echen[DB_1$Antes_echen=="desacuerdo"]<- 2
DB_1$Antes_echen[DB_1$Antes_echen=="en_desacuerdo"]<- 2
DB_1$Antes_echen[DB_1$Antes_echen=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Antes_echen[DB_1$Antes_echen=="Muy en desacuerdo"]<- 1
DB_1$Antes_echen[DB_1$Antes_echen=="muy_desacuerdo"]<- 1
DB_1$Antes_echen[DB_1$Antes_echen=="muy_en_desacuerdo"]<- 1
DB_1$Antes_echen[DB_1$Antes_echen=="Molt en desacord"]<- 1
#No aplica
DB_1$Antes_echen[DB_1$Antes_echen=="No contesta / No aplica"]<- 0
DB_1$Antes_echen[DB_1$Antes_echen=="no_contesta___no_aplica"]<- 0
DB_1$Antes_echen[DB_1$Antes_echen=="NA"]<- 0

DB_1$Antes_echen <- as.numeric(DB_1$Antes_echen)

#REDES
table(DB_1$Antes_redes)
#Muy de acuerdo
DB_1$Antes_redes[DB_1$Antes_redes=="Muy de acuerdo"]<- 5
DB_1$Antes_redes[DB_1$Antes_redes=="muy_acuerdo"]<- 5
DB_1$Antes_redes[DB_1$Antes_redes=="muy_de_acuerdo"]<- 5
DB_1$Antes_redes[DB_1$Antes_redes=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Antes_redes[DB_1$Antes_redes=="De acuerdo"]<- 4
DB_1$Antes_redes[DB_1$Antes_redes=="acuerdo"]<- 4
DB_1$Antes_redes[DB_1$Antes_redes=="de_acuerdo"]<- 4
DB_1$Antes_redes[DB_1$Antes_redes=="D'acord"]<- 4
#Neutral
DB_1$Antes_redes[DB_1$Antes_redes=="neutral"]<- 3
DB_1$Antes_redes[DB_1$Antes_redes=="Neutral"]<- 3
#Desacuerdo
DB_1$Antes_redes[DB_1$Antes_redes=="En desacuerdo"]<- 2
DB_1$Antes_redes[DB_1$Antes_redes=="desacuerdo"]<- 2
DB_1$Antes_redes[DB_1$Antes_redes=="en_desacuerdo"]<- 2
DB_1$Antes_redes[DB_1$Antes_redes=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Antes_redes[DB_1$Antes_redes=="Muy en desacuerdo"]<- 1
DB_1$Antes_redes[DB_1$Antes_redes=="muy_desacuerdo"]<- 1
DB_1$Antes_redes[DB_1$Antes_redes=="muy_en_desacuerdo"]<- 1
DB_1$Antes_redes[DB_1$Antes_redes=="Molt en desacord"]<- 1
#No aplica
DB_1$Antes_redes[DB_1$Antes_redes=="No contesta / No aplica"]<- 0
DB_1$Antes_redes[DB_1$Antes_redes=="no_contesta___no_aplica"]<- 0

DB_1$Antes_redes <-as.numeric(DB_1$Antes_redes)

#DESALOJEN
table(DB_1$Antes_desalojen)
#Muy de acuerdo
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="Muy de acuerdo"]<- 5
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="muy_acuerdo"]<- 5
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="muy_de_acuerdo"]<- 5
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="De acuerdo"]<- 4
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="acuerdo"]<- 4
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="de_acuerdo"]<- 4
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="D'acord"]<- 4
#Neutral
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="neutral"]<- 3
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="Neutral"]<- 3
#Desacuerdo
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="En desacuerdo"]<- 2
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="desacuerdo"]<- 2
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="en_desacuerdo"]<- 2
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="Muy en desacuerdo"]<- 1
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="muy_desacuerdo"]<- 1
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="muy_en_desacuerdo"]<- 1
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="Molt en desacord"]<- 1
#No aplica
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="No contesta / No aplica"]<- 0
DB_1$Antes_desalojen[DB_1$Antes_desalojen=="no_contesta___no_aplica"]<- 0

DB_1$Antes_desalojen <- as.numeric(DB_1$Antes_desalojen)

#FUERA
table(DB_1$Antes_fuera)
#Muy de acuerdo
DB_1$Antes_fuera[DB_1$Antes_fuera=="Muy de acuerdo"]<- 5
DB_1$Antes_fuera[DB_1$Antes_fuera=="muy_acuerdo"]<- 5
DB_1$Antes_fuera[DB_1$Antes_fuera=="muy_de_acuerdo"]<- 5
DB_1$Antes_fuera[DB_1$Antes_fuera=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Antes_fuera[DB_1$Antes_fuera=="De acuerdo"]<- 4
DB_1$Antes_fuera[DB_1$Antes_fuera=="acuerdo"]<- 4
DB_1$Antes_fuera[DB_1$Antes_fuera=="de_acuerdo"]<- 4
DB_1$Antes_fuera[DB_1$Antes_fuera=="D'acord"]<- 4
#Neutral
DB_1$Antes_fuera[DB_1$Antes_fuera=="neutral"]<- 3
DB_1$Antes_fuera[DB_1$Antes_fuera=="Neutral"]<- 3
#Desacuerdo
DB_1$Antes_fuera[DB_1$Antes_fuera=="En desacuerdo"]<- 2
DB_1$Antes_fuera[DB_1$Antes_fuera=="desacuerdo"]<- 2
DB_1$Antes_fuera[DB_1$Antes_fuera=="en_desacuerdo"]<- 2
DB_1$Antes_fuera[DB_1$Antes_fuera=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Antes_fuera[DB_1$Antes_fuera=="Muy en desacuerdo"]<- 1
DB_1$Antes_fuera[DB_1$Antes_fuera=="muy_desacuerdo"]<- 1
DB_1$Antes_fuera[DB_1$Antes_fuera=="muy_en_desacuerdo"]<- 1
DB_1$Antes_fuera[DB_1$Antes_fuera=="Molt en desacord"]<- 1
#No aplica
DB_1$Antes_fuera[DB_1$Antes_fuera=="No contesta / No aplica"]<- 0
DB_1$Antes_fuera[DB_1$Antes_fuera=="no_contesta___no_aplica"]<- 0

DB_1$Antes_fuera <- as.numeric(DB_1$Antes_fuera)

#NO BIENVENIDO
table(DB_1$Antes_nobienvenido)
#Muy de acuerdo
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="Muy de acuerdo"]<- 5
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="muy_acuerdo"]<- 5
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="muy_de_acuerdo"]<- 5
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="De acuerdo"]<- 4
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="acuerdo"]<- 4
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="de_acuerdo"]<- 4
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="D'acord"]<- 4
#Neutral
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="neutral"]<- 3
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="Neutral"]<- 3
#Desacuerdo
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="En desacuerdo"]<- 2
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="desacuerdo"]<- 2
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="en_desacuerdo"]<- 2
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="Muy en desacuerdo"]<- 1
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="muy_desacuerdo"]<- 1
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="muy_en_desacuerdo"]<- 1
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="Molt en desacord"]<- 1
#No aplica
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="No contesta / No aplica"]<- 0
DB_1$Antes_nobienvenido[DB_1$Antes_nobienvenido=="no_contesta___no_aplica"]<- 0

DB_1$Antes_nobienvenido <-as.numeric(DB_1$Antes_nobienvenido)

#COMUNIDAD
table(DB_1$Antes_comunidad)
#Muy de acuerdo
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="Muy de acuerdo"]<- 5
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="muy_acuerdo"]<- 5
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="muy_de_acuerdo"]<- 5
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="De acuerdo"]<- 4
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="acuerdo"]<- 4
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="de_acuerdo"]<- 4
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="D'acord"]<- 4
#Neutral
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="neutral"]<- 3
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="Neutral"]<- 3
#Desacuerdo
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="En desacuerdo"]<- 2
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="desacuerdo"]<- 2
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="en_desacuerdo"]<- 2
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="Muy en desacuerdo"]<- 1
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="muy_desacuerdo"]<- 1
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="muy_en_desacuerdo"]<- 1
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="Molt en desacord"]<- 1
#No aplica
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="No contesta / No aplica"]<- 0
DB_1$Antes_comunidad[DB_1$Antes_comunidad=="no_contesta___no_aplica"]<- 0

DB_1$Antes_comunidad <- as.numeric(DB_1$Antes_comunidad)

#RENOVACIÓN
table(DB_1$Antes_renovacion)
#Muy de acuerdo
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="Muy de acuerdo"]<- 5
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="muy_acuerdo"]<- 5
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="muy_de_acuerdo"]<- 5
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="De acuerdo"]<- 4
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="acuerdo"]<- 4
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="de_acuerdo"]<- 4
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="D'acord"]<- 4
#Neutral
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="neutral"]<- 3
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="Neutral"]<- 3
#Desacuerdo
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="En desacuerdo"]<- 2
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="desacuerdo"]<- 2
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="en_desacuerdo"]<- 2
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="Muy en desacuerdo"]<- 1
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="muy_desacuerdo"]<- 1
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="muy_en_desacuerdo"]<- 1
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="Molt en desacord"]<- 1
#No aplica
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="No contesta / No aplica"]<- 0
DB_1$Antes_renovacion[DB_1$Antes_renovacion=="no_contesta___no_aplica"]<- 0

DB_1$Antes_renovacion <- as.numeric(DB_1$Antes_renovacion)

#* En segundo lugar, obtenemos la variable de Disruption (ND) como media de las 6 antes mencionadas

DB_1 <- DB_1 %>% dplyr::mutate(Antes_ND = ((Antes_echen + Antes_redes + Antes_desalojen + Antes_fuera + Antes_nobienvenido + Antes_comunidad)/6))
head(DB_1[,c("Antes_ND", "Antes_echen", "Antes_redes", "Antes_desalojen", "Antes_fuera", "Antes_nobienvenido", "Antes_comunidad")]) #we do this to check that it has worked well

#* En tercer lugar, obtenemos la variable de Gentrification (PG) como media de las otras 4 variables

DB_1 <- DB_1 %>% dplyr::mutate(Antes_PG = ((Antes_servicios + Antes_ricos + Antes_delincuencia + Antes_renovacion)/4))
head(DB_1[,c("Antes_PG", "Antes_servicios", "Antes_ricos", "Antes_delincuencia", "Antes_renovacion")])


#* Por último, sumamos ambas medias para obtener la variable de NCGS
DB_1 <- DB_1 %>% dplyr::mutate(Antes_NCGS = (Antes_ND + Antes_PG))
head(DB_1[,c("Antes_PG", "Antes_ND", "Antes_NCGS")])

##### GENTRIFICACIÓN AHORA

## SERVICIOS
table(DB_1$Ahora_servicios)
#Muy de acuerdo
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="Muy de acuerdo"]<- 5
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="muy_acuerdo"]<- 5
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="muy_de_acuerdo"]<- 5
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="De acuerdo"]<- 4
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="acuerdo"]<- 4
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="de_acuerdo"]<- 4
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="D'acord"]<- 4
#Neutral
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="neutral"]<- 3
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="Neutral"]<- 3
#Desacuerdo
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="En desacuerdo"]<- 2
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="desacuerdo"]<- 2
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="en_desacuerdo"]<- 2
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="Muy en desacuerdo"]<- 1
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="muy_desacuerdo"]<- 1
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="Molt en desacord"]<- 1
#No aplica
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="No contesta / No aplica"]<- 0
DB_1$Ahora_servicios[DB_1$Ahora_servicios=="no_contesta___no_aplica"]<- 0

DB_1$Ahora_servicios <- as.numeric(DB_1$Ahora_servicios)

#RICOS
table(DB_1$Ahora_ricos)
#Muy de acuerdo
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="Muy de acuerdo"]<- 5
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="muy_acuerdo"]<- 5
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="muy_de_acuerdo"]<- 5
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="De acuerdo"]<- 4
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="acuerdo"]<- 4
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="de_acuerdo"]<- 4
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="D'acord"]<- 4
#Neutral
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="neutral"]<- 3
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="Neutral"]<- 3
#Desacuerdo
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="En desacuerdo"]<- 2
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="desacuerdo"]<- 2
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="en_desacuerdo"]<- 2
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="Muy en desacuerdo"]<- 1
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="muy_desacuerdo"]<- 1
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="Molt en desacord"]<- 1
#No aplica
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="No contesta / No aplica"]<- 0
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="no_contesta___no_aplica"]<- 0
DB_1$Ahora_ricos[DB_1$Ahora_ricos=="NA"]<- 0

DB_1$Ahora_ricos <- as.numeric(DB_1$Ahora_ricos)

#DELINCUENCIA
table(DB_1$Ahora_delincuencia)
#Muy de acuerdo
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="Muy de acuerdo"]<- 5
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="muy_acuerdo"]<- 5
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="muy_de_acuerdo"]<- 5
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="De acuerdo"]<- 4
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="acuerdo"]<- 4
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="de_acuerdo"]<- 4
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="D'acord"]<- 4
#Neutral
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="neutral"]<- 3
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="Neutral"]<- 3
#Desacuerdo
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="En desacuerdo"]<- 2
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="desacuerdo"]<- 2
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="en_desacuerdo"]<- 2
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="Muy en desacuerdo"]<- 1
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="muy_desacuerdo"]<- 1
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="muy_en_desacuerdo"]<- 1
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="Molt en desacord"]<- 1
#No aplica
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="No contesta / No aplica"]<- 0
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="no_contesta___no_aplica"]<- 0
DB_1$Ahora_delincuencia[DB_1$Ahora_delincuencia=="NA"]<- 0

DB_1$Ahora_delincuencia <- as.numeric(DB_1$Ahora_delincuencia)

#ECHEN
table(DB_1$Ahora_echen)
#Muy de acuerdo
DB_1$Ahora_echen[DB_1$Ahora_echen=="Muy de acuerdo"]<- 5
DB_1$Ahora_echen[DB_1$Ahora_echen=="muy_acuerdo"]<- 5
DB_1$Ahora_echen[DB_1$Ahora_echen=="muy_de_acuerdo"]<- 5
DB_1$Ahora_echen[DB_1$Ahora_echen=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Ahora_echen[DB_1$Ahora_echen=="De acuerdo"]<- 4
DB_1$Ahora_echen[DB_1$Ahora_echen=="acuerdo"]<- 4
DB_1$Ahora_echen[DB_1$Ahora_echen=="de_acuerdo"]<- 4
DB_1$Ahora_echen[DB_1$Ahora_echen=="D'acord"]<- 4
#Neutral
DB_1$Ahora_echen[DB_1$Ahora_echen=="neutral"]<- 3
DB_1$Ahora_echen[DB_1$Ahora_echen=="Neutral"]<- 3
#Desacuerdo
DB_1$Ahora_echen[DB_1$Ahora_echen=="En desacuerdo"]<- 2
DB_1$Ahora_echen[DB_1$Ahora_echen=="desacuerdo"]<- 2
DB_1$Ahora_echen[DB_1$Ahora_echen=="en_desacuerdo"]<- 2
DB_1$Ahora_echen[DB_1$Ahora_echen=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Ahora_echen[DB_1$Ahora_echen=="Muy en desacuerdo"]<- 1
DB_1$Ahora_echen[DB_1$Ahora_echen=="muy_desacuerdo"]<- 1
DB_1$Ahora_echen[DB_1$Ahora_echen=="muy_en_desacuerdo"]<- 1
DB_1$Ahora_echen[DB_1$Ahora_echen=="Molt en desacord"]<- 1
#No aplica
DB_1$Ahora_echen[DB_1$Ahora_echen=="No contesta / No aplica"]<- 0
DB_1$Ahora_echen[DB_1$Ahora_echen=="no_contesta___no_aplica"]<- 0
DB_1$Ahora_echen[DB_1$Ahora_echen=="NA"]<- 0

DB_1$Ahora_echen <- as.numeric(DB_1$Ahora_echen)

#REDES
table(DB_1$Ahora_redes)
#Muy de acuerdo
DB_1$Ahora_redes[DB_1$Ahora_redes=="Muy de acuerdo"]<- 5
DB_1$Ahora_redes[DB_1$Ahora_redes=="muy_acuerdo"]<- 5
DB_1$Ahora_redes[DB_1$Ahora_redes=="muy_de_acuerdo"]<- 5
DB_1$Ahora_redes[DB_1$Ahora_redes=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Ahora_redes[DB_1$Ahora_redes=="De acuerdo"]<- 4
DB_1$Ahora_redes[DB_1$Ahora_redes=="acuerdo"]<- 4
DB_1$Ahora_redes[DB_1$Ahora_redes=="de_acuerdo"]<- 4
DB_1$Ahora_redes[DB_1$Ahora_redes=="D'acord"]<- 4
#Neutral
DB_1$Ahora_redes[DB_1$Ahora_redes=="neutral"]<- 3
DB_1$Ahora_redes[DB_1$Ahora_redes=="Neutral"]<- 3
#Desacuerdo
DB_1$Ahora_redes[DB_1$Ahora_redes=="En desacuerdo"]<- 2
DB_1$Ahora_redes[DB_1$Ahora_redes=="desacuerdo"]<- 2
DB_1$Ahora_redes[DB_1$Ahora_redes=="en_desacuerdo"]<- 2
DB_1$Ahora_redes[DB_1$Ahora_redes=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Ahora_redes[DB_1$Ahora_redes=="Muy en desacuerdo"]<- 1
DB_1$Ahora_redes[DB_1$Ahora_redes=="muy_desacuerdo"]<- 1
DB_1$Ahora_redes[DB_1$Ahora_redes=="muy_en_desacuerdo"]<- 1
DB_1$Ahora_redes[DB_1$Ahora_redes=="Molt en desacord"]<- 1
#No aplica
DB_1$Ahora_redes[DB_1$Ahora_redes=="No contesta / No aplica"]<- 0
DB_1$Ahora_redes[DB_1$Ahora_redes=="no_contesta___no_aplica"]<- 0

DB_1$Ahora_redes <- as.numeric(DB_1$Ahora_redes)

#DESALOJEN
table(DB_1$Ahora_desalojen)
#Muy de acuerdo
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="Muy de acuerdo"]<- 5
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="muy_acuerdo"]<- 5
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="muy_de_acuerdo"]<- 5
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="De acuerdo"]<- 4
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="acuerdo"]<- 4
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="de_acuerdo"]<- 4
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="D'acord"]<- 4
#Neutral
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="neutral"]<- 3
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="Neutral"]<- 3
#Desacuerdo
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="En desacuerdo"]<- 2
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="desacuerdo"]<- 2
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="en_desacuerdo"]<- 2
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="Muy en desacuerdo"]<- 1
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="muy_desacuerdo"]<- 1
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="muy_en_desacuerdo"]<- 1
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="Molt en desacord"]<- 1
#No aplica
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="No contesta / No aplica"]<- 0
DB_1$Ahora_desalojen[DB_1$Ahora_desalojen=="no_contesta___no_aplica"]<- 0

DB_1$Ahora_desalojen <- as.numeric(DB_1$Ahora_desalojen)

#FUERA
table(DB_1$Ahora_fuera)
#Muy de acuerdo
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="Muy de acuerdo"]<- 5
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="muy_acuerdo"]<- 5
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="muy_de_acuerdo"]<- 5
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="De acuerdo"]<- 4
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="acuerdo"]<- 4
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="de_acuerdo"]<- 4
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="D'acord"]<- 4
#Neutral
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="neutral"]<- 3
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="Neutral"]<- 3
#Desacuerdo
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="En desacuerdo"]<- 2
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="desacuerdo"]<- 2
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="en_desacuerdo"]<- 2
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="Muy en desacuerdo"]<- 1
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="muy_desacuerdo"]<- 1
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="muy_en_desacuerdo"]<- 1
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="Molt en desacord"]<- 1
#No aplica
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="No contesta / No aplica"]<- 0
DB_1$Ahora_fuera[DB_1$Ahora_fuera=="no_contesta___no_aplica"]<- 0

DB_1$Ahora_fuera <- as.numeric(DB_1$Ahora_fuera)

#NO BIENVENIDO
table(DB_1$Ahora_nobienvenido)
#Muy de acuerdo
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="Muy de acuerdo"]<- 5
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="muy_acuerdo"]<- 5
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="muy_de_acuerdo"]<- 5
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="De acuerdo"]<- 4
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="acuerdo"]<- 4
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="de_acuerdo"]<- 4
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="D'acord"]<- 4
#Neutral
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="neutral"]<- 3
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="Neutral"]<- 3
#Desacuerdo
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="En desacuerdo"]<- 2
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="desacuerdo"]<- 2
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="en_desacuerdo"]<- 2
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="Muy en desacuerdo"]<- 1
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="muy_desacuerdo"]<- 1
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="muy_en_desacuerdo"]<- 1
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="Molt en desacord"]<- 1
#No aplica
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="No contesta / No aplica"]<- 0
DB_1$Ahora_nobienvenido[DB_1$Ahora_nobienvenido=="no_contesta___no_aplica"]<- 0

DB_1$Ahora_nobienvenido <- as.numeric(DB_1$Ahora_nobienvenido)

#COMUNIDAD
table(DB_1$Ahora_comunidad)
#Muy de acuerdo
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="Muy de acuerdo"]<- 5
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="muy_acuerdo"]<- 5
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="muy_de_acuerdo"]<- 5
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="De acuerdo"]<- 4
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="acuerdo"]<- 4
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="de_acuerdo"]<- 4
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="D'acord"]<- 4
#Neutral
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="neutral"]<- 3
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="Neutral"]<- 3
#Desacuerdo
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="En desacuerdo"]<- 2
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="desacuerdo"]<- 2
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="en_desacuerdo"]<- 2
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="Muy en desacuerdo"]<- 1
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="muy_desacuerdo"]<- 1
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="muy_en_desacuerdo"]<- 1
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="Molt en desacord"]<- 1
#No aplica
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="No contesta / No aplica"]<- 0
DB_1$Ahora_comunidad[DB_1$Ahora_comunidad=="no_contesta___no_aplica"]<- 0

DB_1$Ahora_comunidad <- as.numeric(DB_1$Ahora_comunidad)

#RENOVACIÓN
table(DB_1$Ahora_renovacion)
#Muy de acuerdo
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="Muy de acuerdo"]<- 5
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="muy_acuerdo"]<- 5
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="muy_de_acuerdo"]<- 5
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="Molt d'acord"]<- 5
#De acuerdo
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="De acuerdo"]<- 4
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="acuerdo"]<- 4
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="de_acuerdo"]<- 4
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="D'acord"]<- 4
#Neutral
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="neutral"]<- 3
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="Neutral"]<- 3
#Desacuerdo
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="En desacuerdo"]<- 2
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="desacuerdo"]<- 2
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="en_desacuerdo"]<- 2
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="En desacord"]<- 2
#Muy en desacuerdo
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="Muy en desacuerdo"]<- 1
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="muy_desacuerdo"]<- 1
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="muy_en_desacuerdo"]<- 1
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="Molt en desacord"]<- 1
#No aplica
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="No contesta / No aplica"]<- 0
DB_1$Ahora_renovacion[DB_1$Ahora_renovacion=="no_contesta___no_aplica"]<- 0

DB_1$Ahora_renovacion <- as.numeric(DB_1$Ahora_renovacion)
summary(DB_1)

DB_1 <- DB_1 %>% dplyr::mutate(Ahora_ND = ((Ahora_echen + Ahora_redes + Ahora_desalojen + Ahora_fuera + Ahora_nobienvenido + Ahora_comunidad)/6))
head(DB_1[,c("Ahora_ND", "Ahora_echen","Antes_redes", "Ahora_desalojen", "Ahora_fuera", "Ahora_nobienvenido", "Ahora_comunidad")]) #we do this to check that it has worked well

#* En tercer lugar, obtenemos la variable de Gentrification (PG) como media de las otras 4 variables

DB_1 <- DB_1 %>% dplyr::mutate(Ahora_PG = ((Ahora_servicios + Ahora_ricos + Ahora_delincuencia + Ahora_renovacion)/4))
head(DB_1[,c("Ahora_PG", "Ahora_servicios", "Antes_ricos", "Ahora_delincuencia", "Ahora_renovacion")])


#* Por último, sumamos ambas medias para obtener la variable de NCGS
DB_1 <- DB_1 %>% dplyr::mutate(Ahora_NCGS = (Ahora_ND + Ahora_PG))
head(DB_1[,c("Ahora_PG", "Ahora_ND", "Ahora_NCGS")])

DB_2<- DB_1
DB_1<- DB_2

###### TURISMO ANTES ######

##Turistas
table(DB_1$Antes_turistas)
#Aumento drastico
DB_1$Antes_turistas[DB_1$Antes_turistas=="aumentado_drastica"]<- 5
DB_1$Antes_turistas[DB_1$Antes_turistas=="hab_a_aumentado_de_forma_dr_stica"]<- 5
DB_1$Antes_turistas[DB_1$Antes_turistas=="Había aumentado de forma drástica."]<- 5
DB_1$Antes_turistas[DB_1$Antes_turistas=="Havia augmentat de forma dràstica."]<- 5

#Aumento
DB_1$Antes_turistas[DB_1$Antes_turistas=="aumentado"]<- 4
DB_1$Antes_turistas[DB_1$Antes_turistas=="Havia augmentat."]<- 4
DB_1$Antes_turistas[DB_1$Antes_turistas=="Había aumentado."]<- 4
DB_1$Antes_turistas[DB_1$Antes_turistas=="hab_a_aumentado"]<- 4
#Mantenido
DB_1$Antes_turistas[DB_1$Antes_turistas=="mantenido"]<- 3
DB_1$Antes_turistas[DB_1$Antes_turistas=="S'havia mantingut igual."]<- 3
DB_1$Antes_turistas[DB_1$Antes_turistas=="Se había mantenido igual."]<- 3
DB_1$Antes_turistas[DB_1$Antes_turistas=="se_hab_a_mantenido_igual"]<- 3
#Disminución
DB_1$Antes_turistas[DB_1$Antes_turistas=="disminuido"]<- 2
DB_1$Antes_turistas[DB_1$Antes_turistas=="Había disminuido."]<- 2
DB_1$Antes_turistas[DB_1$Antes_turistas=="Havia disminuït."]<- 2
DB_1$Antes_turistas[DB_1$Antes_turistas=="hab_a_disminuido"]<- 2
#Disminución drastica
DB_1$Antes_turistas[DB_1$Antes_turistas=="disminuido_drastica"]<- 1
DB_1$Antes_turistas[DB_1$Antes_turistas=="Había disminuido de forma drástica."]<- 1
DB_1$Antes_turistas[DB_1$Antes_turistas=="Havia disminuït de forma dràstica."]<- 1
DB_1$Antes_turistas[DB_1$Antes_turistas=="hab_a_disminuido_de_forma_dr_stica"]<- 1
#No aplica
DB_1$Antes_turistas[DB_1$Antes_turistas=="No sabe/ No contesta"]<- 0
DB_1$Antes_turistas[DB_1$Antes_turistas=="No sap/ No contesta"]<- 0
DB_1$Antes_turistas[DB_1$Antes_turistas=="NA"]<- 0

DB_1$Antes_turistas <- as.numeric(DB_1$Antes_turistas)
is.na(DB_1$Antes_turistas) 
## Hoteles
table(DB_1$Antes_hoteles)
#Aumento drastico
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="aumentado_drastica"]<- 5
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="Ha augmentat de forma dràstica."]<- 5
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="Ha aumentado de forma drástica."]<- 5
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="hab_a_aumentado_de_forma_dr_stica"]<- 5
#Aumento
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="aumentado"]<- 4
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="Ha augmentat."]<- 4
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="Ha aumentado."]<- 4
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="hab_a_aumentado"]<- 4
#Mantenido
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="mantenido"]<- 3
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="S'ha mantingut igual."]<- 3
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="Se ha mantenido igual."]<- 3
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="se_hab_a_mantenido_igual"]<- 3
#Disminución
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="disminuido"]<- 2
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="Ha disminuido."]<- 2
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="Ha disminuït."]<- 2
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="hab_a_disminuido"]<- 2
#Disminución drastica
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="disminuido_drastica"]<- 1
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="Ha disminuido de forma drástica."]<- 1
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="Ha disminuït de forma dràstica."]<- 1
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="hab_a_disminuido_de_forma_dr_stica"]<- 1
#No aplica
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="No sabe/ No contesta"]<- 0
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="No sap/ No contesta"]<- 0
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="no_sabe__no_contesta"]<- 0
DB_1$Antes_hoteles[DB_1$Antes_hoteles=="NA"]<- 0

is.na(DB_1$Antes_hoteles)
DB_1$Antes_hoteles <- as.numeric(DB_1$Antes_hoteles)

DB_1 <- DB_1 %>% dplyr::mutate(Antes_turismo = (Antes_turistas + Antes_hoteles))

table(DB_1$Antes_turismo)

###### TURISMO AHORA #####

##Turistas
table(DB_1$Ahora_turistas)
#Aumento drastico
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="aumentado_drastica"]<- 5
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="Ha augmentat de forma dràstica."]<- 5
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="Ha aumentado de forma drástica."]<- 5
#Aumento
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="aumentado"]<- 4
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="Ha augmentat."]<- 4
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="Ha aumentado."]<- 4
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="hab_a_aumentado"]<- 4
#Mantenido
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="mantenido"]<- 3
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="S'ha mantingut igual."]<- 3
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="Se ha mantenido igual."]<- 3
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="se_hab_a_mantenido_igual"]<- 3
#Disminución
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="disminuido"]<- 2
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="Ha disminuido."]<- 2
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="Ha disminuït."]<- 2
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="hab_a_disminuido"]<- 2
#Disminución drastica
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="disminuido_drastica"]<- 1
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="Ha disminuido de forma drástica."]<- 1
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="Ha disminuït de forma dràstica"]<- 1
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="hab_a_disminuido_de_forma_dr_stica"]<- 1
#No aplica
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="No sabe/ No contesta"]<- 0
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="No sap/ No contesta"]<- 0
DB_1$Ahora_turistas[DB_1$Ahora_turistas=="no_sabe__no_contesta"]<- 0

DB_1$Ahora_turistas <- as.numeric(DB_1$Ahora_turistas)

## Hoteles
table(DB_1$Ahora_hoteles)
#Aumento drastico
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="aumentado_drastica"]<- 5
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="Ha augmentat de forma dràstica."]<- 5
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="Ha aumentado de forma drástica."]<- 5
#Aumento
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="aumentado"]<- 4
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="Ha augmentat."]<- 4
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="Ha aumentado."]<- 4
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="hab_a_aumentado"]<- 4
#Mantenido
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="mantenido"]<- 3
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="S'ha mantingut igual."]<- 3
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="Se ha mantenido igual."]<- 3
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="se_hab_a_mantenido_igual"]<- 3
#Disminución
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="disminuido"]<- 2
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="Ha disminuido."]<- 2
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="Ha disminuït."]<- 2
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="hab_a_disminuido"]<- 2
#Disminución drastica
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="disminuido_drastica"]<- 1
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="Ha disminuido de forma drástica."]<- 1
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="Ha disminuït de forma dràstica"]<- 1
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="hab_a_disminuido_de_forma_dr_stica"]<- 1
#No aplica
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="No sabe/ No contesta"]<- 0
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="No sap/ No contesta"]<- 0
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="no_sabe__no_contesta"]<- 0
DB_1$Ahora_hoteles[DB_1$Ahora_hoteles=="NA"]<- 0

DB_1$Ahora_hoteles <- as.numeric(DB_1$Ahora_hoteles)

DB_1 <- DB_1 %>% dplyr::mutate(Ahora_turismo = (Ahora_turistas + Ahora_hoteles))

DB_3 <- DB_1

##############################################
########### VARIABLES DE SALUD ############
## SALUD GENERAL
# Antes
table(DB_1$Antes_salud)
# Excelente
DB_1$Antes_salud[DB_1$Antes_salud=="Excel·lent"]<- "Excelente"
DB_1$Antes_salud[DB_1$Antes_salud=="excelente"]<- "Excelente"
# Muy buena
DB_1$Antes_salud[DB_1$Antes_salud=="Muy Buena"]<- "Muy buena"
DB_1$Antes_salud[DB_1$Antes_salud=="muy_buena"]<- "Muy buena"
DB_1$Antes_salud[DB_1$Antes_salud=="Molt Bona"]<- "Muy buena"
# Buena
DB_1$Antes_salud[DB_1$Antes_salud=="buena"]<- "Buena"
DB_1$Antes_salud[DB_1$Antes_salud=="Bona"]<- "Buena"
# Regular
DB_1$Antes_salud[DB_1$Antes_salud=="regular"]<- "Regular"
# Mala
DB_1$Antes_salud[DB_1$Antes_salud=="mala"]<- "Mala"
DB_1$Antes_salud[DB_1$Antes_salud=="Dolenta"]<- "Mala"
# No aplica
DB_1$Antes_salud[DB_1$Antes_salud=="No contesta / No aplica"]<- NA
DB_1$Antes_salud[DB_1$Antes_salud=="no_contesta___no_aplica"]<- NA

# la dividimos en buena y mala
DB_1$Antes_salud2[DB_1$Antes_salud=="Muy buena"]<- "0"
DB_1$Antes_salud2[DB_1$Antes_salud=="Excelente"]<- "0"
DB_1$Antes_salud2[DB_1$Antes_salud=="Buena"]<- "0"
DB_1$Antes_salud2[DB_1$Antes_salud=="Regular"]<- "1"
DB_1$Antes_salud2[DB_1$Antes_salud=="Mala"]<- "1"

summarytools::freq(DB_1$Antes_salud2)
# AHORA
table(DB_1$Ahora_salud)
# Excelente
DB_1$Ahora_salud[DB_1$Ahora_salud=="Excel·lent"]<- "Excelente"
DB_1$Ahora_salud[DB_1$Ahora_salud=="excelente"]<- "Excelente"
# Muy buena
DB_1$Ahora_salud[DB_1$Ahora_salud=="Muy Buena"]<- "Muy buena"
DB_1$Ahora_salud[DB_1$Ahora_salud=="muy_buena"]<- "Muy buena"
DB_1$Ahora_salud[DB_1$Ahora_salud=="Molt Bona"]<- "Muy buena"
# Buena
DB_1$Ahora_salud[DB_1$Ahora_salud=="buena"]<- "Buena"
DB_1$Ahora_salud[DB_1$Ahora_salud=="Bona"]<- "Buena"
# Regular
DB_1$Ahora_salud[DB_1$Ahora_salud=="regular"]<- "Regular"
# Mala
DB_1$Ahora_salud[DB_1$Ahora_salud=="mala"]<- "Mala"
DB_1$Ahora_salud[DB_1$Ahora_salud=="Dolenta"]<- "Mala"
# No aplica
DB_1$Ahora_salud[DB_1$Ahora_salud=="No contesta / No aplica"]<- NA
DB_1$Ahora_salud[DB_1$Ahora_salud=="no_contesta___no_aplica"]<- NA

# dividimos en buena y mala salud
DB_1$Ahora_salud2[DB_1$Ahora_salud=="Muy buena"]<- "0"
DB_1$Ahora_salud2[DB_1$Ahora_salud=="Excelente"]<- "0"
DB_1$Ahora_salud2[DB_1$Ahora_salud=="Buena"]<- "0"
DB_1$Ahora_salud2[DB_1$Ahora_salud=="Regular"]<- "1"
DB_1$Ahora_salud2[DB_1$Ahora_salud=="Mala"]<- "1"

summarytools::freq(DB_1$Ahora_salud2)

DB_5 <- DB_1
DB_1 <- DB_5

## SALUD MENTAL
# Primero transformamos respuestas según cada pregunta

# ANTES
#Concentrarse 
table(DB_1$Antes_concentrarse)
#Siempre
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="0 (Sempre)"]<- "0"
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="0 (Siempre)"]<- "0"
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="3 (Mai)"]<- "3"
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="3 (Nunca)"]<- "3"
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="3__nunca"]<- "3"

#NA
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="No contesta / No aplica"]<- "NA"
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="no_contesta___no_aplica"]<- "NA"

#Perder el sueño 
table(DB_1$Antes_sueno)
#Siempre
DB_1$Antes_sueno[DB_1$Antes_sueno=="0 (Sempre)"]<- "0"
DB_1$Antes_sueno[DB_1$Antes_sueno=="0 (Siempre)"]<- "0"
DB_1$Antes_sueno[DB_1$Antes_sueno=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_sueno[DB_1$Antes_sueno=="3 (Mai)"]<- "3"
DB_1$Antes_sueno[DB_1$Antes_sueno=="3 (Nunca)"]<- "3"
DB_1$Antes_sueno[DB_1$Antes_sueno=="3__nunca"]<- "3"
#NA
DB_1$Antes_sueno[DB_1$Antes_sueno=="No contesta / No aplica"]<- "NA"
DB_1$Antes_sueno[DB_1$Antes_sueno=="no_contesta___no_aplica"]<- "NA"

#Papel útil en la vida
table(DB_1$Antes_papel)
#Siempre
DB_1$Antes_papel[DB_1$Antes_papel=="0 (Sempre)"]<- "0"
DB_1$Antes_papel[DB_1$Antes_papel=="0 (Siempre)"]<- "0"
DB_1$Antes_papel[DB_1$Antes_papel=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_papel[DB_1$Antes_papel=="3 (Mai)"]<- "3"
DB_1$Antes_papel[DB_1$Antes_papel=="3 (Nunca)"]<- "3"
DB_1$Antes_papel[DB_1$Antes_papel=="3__nunca"]<- "3"
#NA
DB_1$Antes_papel[DB_1$Antes_papel=="No contesta / No aplica"]<- "NA"
DB_1$Antes_papel[DB_1$Antes_papel=="no_contesta___no_aplica"]<- "NA"

#Tomar decisiones 
table(DB_1$Antes_decisiones)
#Siempre
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="0 (Sempre)"]<- "0"
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="0 (Siempre)"]<- "0"
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="3 (Mai)"]<- "3"
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="3 (Nunca)"]<- "3"
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="3__nunca"]<- "3"
#NA
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="No contesta / No aplica"]<- "NA"
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="no_contesta___no_aplica"]<- "NA"

#Agobiada y en tensión 
table(DB_1$Antes_tension)
#Siempre
DB_1$Antes_tension[DB_1$Antes_tension=="0 (Sempre)"]<- "0"
DB_1$Antes_tension[DB_1$Antes_tension=="0 (Siempre)"]<- "0"
DB_1$Antes_tension[DB_1$Antes_tension=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_tension[DB_1$Antes_tension=="3 (Mai)"]<- "3"
DB_1$Antes_tension[DB_1$Antes_tension=="3 (Nunca)"]<- "3"
DB_1$Antes_tension[DB_1$Antes_tension=="3__nunca"]<- "3"
#NA
DB_1$Antes_tension[DB_1$Antes_tension=="No contesta / No aplica"]<- "NA"
DB_1$Antes_tension[DB_1$Antes_tension=="no_contesta___no_aplica"]<- "NA"

#Superar las dificultades
table(DB_1$Antes_superar)
#Siempre
DB_1$Antes_superar[DB_1$Antes_superar=="0 (Sempre)"]<- "0"
DB_1$Antes_superar[DB_1$Antes_superar=="0 (Siempre)"]<- "0"
DB_1$Antes_superar[DB_1$Antes_superar=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_superar[DB_1$Antes_superar=="3 (Mai)"]<- "3"
DB_1$Antes_superar[DB_1$Antes_superar=="3 (Nunca)"]<- "3"
DB_1$Antes_superar[DB_1$Antes_superar=="3__nunca"]<- "3"
#NA
DB_1$Antes_superar[DB_1$Antes_superar=="No contesta / No aplica"]<- "NA"
DB_1$Antes_superar[DB_1$Antes_superar=="no_contesta___no_aplica"]<- "NA"

#Disfrutar las actividades
table(DB_1$Antes_disfrutar)
#Siempre
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="0 (Sempre)"]<- "0"
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="0 (Siempre)"]<- "0"
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="3 (Mai)"]<- "3"
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="3 (Nunca)"]<- "3"
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="3__nunca"]<- "3"
#NA
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="No contesta / No aplica"]<- "NA"
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="no_contesta___no_aplica"]<- "NA"

#Problemas
table(DB_1$Antes_problemas)
#Siempre
DB_1$Antes_problemas[DB_1$Antes_problemas=="0 (Sempre)"]<- "0"
DB_1$Antes_problemas[DB_1$Antes_problemas=="0 (Siempre)"]<- "0"
DB_1$Antes_problemas[DB_1$Antes_problemas=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_problemas[DB_1$Antes_problemas=="3 (Mai)"]<- "3"
DB_1$Antes_problemas[DB_1$Antes_problemas=="3 (Nunca)"]<- "3"
DB_1$Antes_problemas[DB_1$Antes_problemas=="3__nunca"]<- "3"
#NA
DB_1$Antes_problemas[DB_1$Antes_problemas=="No contesta / No aplica"]<- "NA"
DB_1$Antes_problemas[DB_1$Antes_problemas=="no_contesta___no_aplica"]<- "NA"

#Sentirse deprimida
table(DB_1$Antes_deprimida)
#Siempre
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="0 (Sempre)"]<- "0"
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="0 (Siempre)"]<- "0"
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="3 (Mai)"]<- "3"
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="3 (Nunca)"]<- "3"
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="3__nunca"]<- "3"
#NA
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="No contesta / No aplica"]<- "NA"
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="no_contesta___no_aplica"]<- "NA"

#Perder confianza
table(DB_1$Antes_confianza)
#Siempre
DB_1$Antes_confianza[DB_1$Antes_confianza=="0 (Sempre)"]<- "0"
DB_1$Antes_confianza[DB_1$Antes_confianza=="0 (Siempre)"]<- "0"
DB_1$Antes_confianza[DB_1$Antes_confianza=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_confianza[DB_1$Antes_confianza=="3 (Mai)"]<- "3"
DB_1$Antes_confianza[DB_1$Antes_confianza=="3 (Nunca)"]<- "3"
DB_1$Antes_confianza[DB_1$Antes_confianza=="3__nunca"]<- "3"
#NA
DB_1$Antes_confianza[DB_1$Antes_confianza=="No contesta / No aplica"]<- "NA"
DB_1$Antes_confianza[DB_1$Antes_confianza=="no_contesta___no_aplica"]<- "NA"

#Valer para nada
table(DB_1$Antes_valer)
#Siempre
DB_1$Antes_valer[DB_1$Antes_valer=="0 (Sempre)"]<- "0"
DB_1$Antes_valer[DB_1$Antes_valer=="0 (Siempre)"]<- "0"
DB_1$Antes_valer[DB_1$Antes_valer=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_valer[DB_1$Antes_valer=="3 (Mai)"]<- "3"
DB_1$Antes_valer[DB_1$Antes_valer=="3 (Nunca)"]<- "3"
DB_1$Antes_valer[DB_1$Antes_valer=="3__nunca"]<- "3"
#NA
DB_1$Antes_valer[DB_1$Antes_valer=="No contesta / No aplica"]<- "NA"
DB_1$Antes_valer[DB_1$Antes_valer=="no_contesta___no_aplica"]<- "NA"

#Considerablemente feliz
table(DB_1$Antes_feliz)
#Siempre
DB_1$Antes_feliz[DB_1$Antes_feliz=="0 (Sempre)"]<- "0"
DB_1$Antes_feliz[DB_1$Antes_feliz=="0 (Siempre)"]<- "0"
DB_1$Antes_feliz[DB_1$Antes_feliz=="0__siempre"]<- "0"
#Nunca
DB_1$Antes_feliz[DB_1$Antes_feliz=="3 (Mai)"]<- "3"
DB_1$Antes_feliz[DB_1$Antes_feliz=="3 (Nunca)"]<- "3"
DB_1$Antes_feliz[DB_1$Antes_feliz=="3__nunca"]<- "3"
#NA
DB_1$Antes_feliz[DB_1$Antes_feliz=="No contesta / No aplica"]<- "NA"
DB_1$Antes_feliz[DB_1$Antes_feliz=="no_contesta___no_aplica"]<- "NA"

#### CONVERTIMOS LAS VARIABLES DE SALUD MENTAL ####

# Primero transformamos respuestas según cada pregunta
# si la pregunta es positiva, 0 y 1 (siempre, casi siempre) son un 0; 2 y 3 (muy poco, nunca) son un 1
# si la preugnta es negativa, 0 y 1 (siempre, casi siempre) son un 1; 2 y 3 (muy poco, nunca) son un 0

#* concentrarse = positivo 
table(DB_1$Antes_concentrarse)
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="1"]<- "0"
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="2"]<- "1"
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="3"]<- "1"
DB_1$Antes_concentrarse[DB_1$Antes_concentrarse=="NA"]<- NA

DB_1$Antes_concentrarse <- as.numeric(DB_1$Antes_concentrarse)

#* sueño = negativo
table(DB_1$Antes_sueno)
DB_1$Antes_sueno[DB_1$Antes_sueno=="0"]<- "1"
DB_1$Antes_sueno[DB_1$Antes_sueno=="2"]<- "0"
DB_1$Antes_sueno[DB_1$Antes_sueno=="3"]<- "0"
DB_1$Antes_sueno[DB_1$Antes_sueno=="NA"]<- NA

DB_1$Antes_sueno <- as.numeric(DB_1$Antes_sueno)

#* papel = positivo
table(DB_1$Antes_papel)
DB_1$Antes_papel[DB_1$Antes_papel=="1"]<- "0"
DB_1$Antes_papel[DB_1$Antes_papel=="2"]<- "1"
DB_1$Antes_papel[DB_1$Antes_papel=="3"]<- "1"
DB_1$Antes_papel[DB_1$Antes_papel=="NA"]<- NA

DB_1$Antes_papel <- as.numeric(DB_1$Antes_papel)

#* decisiones = positivo
table(DB_1$Antes_decisiones)
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="1"]<- "0"
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="2"]<- "1"
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="3"]<- "1"
DB_1$Antes_decisiones[DB_1$Antes_decisiones=="NA"]<- NA

DB_1$Antes_decisiones <- as.numeric(DB_1$Antes_decisiones)

#* tension = negativo
table(DB_1$Antes_tension)
DB_1$Antes_tension[DB_1$Antes_tension=="0"]<- "1"
DB_1$Antes_tension[DB_1$Antes_tension=="2"]<- "0"
DB_1$Antes_tension[DB_1$Antes_tension=="3"]<- "0"
DB_1$Antes_tension[DB_1$Antes_tension=="NA"]<- NA

DB_1$Antes_tension <- as.numeric(DB_1$Antes_tension)

#* superar = negativo
table(DB_1$Antes_superar)
DB_1$Antes_superar[DB_1$Antes_superar=="0"]<- "1"
DB_1$Antes_superar[DB_1$Antes_superar=="2"]<- "0"
DB_1$Antes_superar[DB_1$Antes_superar=="3"]<- "0"
DB_1$Antes_superar[DB_1$Antes_superar=="NA"]<- NA

DB_1$Antes_superar <- as.numeric(DB_1$Antes_superar)

#* disfrutar = positivo
table(DB_1$Antes_disfrutar)
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="1"]<- "0"
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="2"]<- "1"
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="3"]<- "1"
DB_1$Antes_disfrutar[DB_1$Antes_disfrutar=="NA"]<- NA

DB_1$Antes_disfrutar <- as.numeric(DB_1$Antes_disfrutar)

#* problemas = positivo
table(DB_1$Antes_problemas)
DB_1$Antes_problemas[DB_1$Antes_problemas=="1"]<- "0"
DB_1$Antes_problemas[DB_1$Antes_problemas=="2"]<- "1"
DB_1$Antes_problemas[DB_1$Antes_problemas=="3"]<- "1"
DB_1$Antes_problemas[DB_1$Antes_problemas=="NA"]<- NA

DB_1$Antes_problemas <- as.numeric(DB_1$Antes_problemas)

#* deprimida = negativo
table(DB_1$Antes_deprimida)
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="0"]<- "1"
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="2"]<- "0"
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="3"]<- "0"
DB_1$Antes_deprimida[DB_1$Antes_deprimida=="NA"]<- NA

DB_1$Antes_deprimida <- as.numeric(DB_1$Antes_deprimida)

#* confianza = negativo
table(DB_1$Antes_confianza)
DB_1$Antes_confianza[DB_1$Antes_confianza=="0"]<- "1"
DB_1$Antes_confianza[DB_1$Antes_confianza=="2"]<- "0"
DB_1$Antes_confianza[DB_1$Antes_confianza=="3"]<- "0"
DB_1$Antes_confianza[DB_1$Antes_confianza=="NA"]<- NA

DB_1$Antes_confianza <- as.numeric(DB_1$Antes_confianza)

#* valer = negativo
table(DB_1$Antes_valer)
DB_1$Antes_valer[DB_1$Antes_valer=="0"]<- "1"
DB_1$Antes_valer[DB_1$Antes_valer=="2"]<- "0"
DB_1$Antes_valer[DB_1$Antes_valer=="3"]<- "0"
DB_1$Antes_valer[DB_1$Antes_valer=="NA"]<- NA

DB_1$Antes_valer <- as.numeric(DB_1$Antes_valer)

#* feliz = positivo
table(DB_1$Antes_feliz)
DB_1$Antes_feliz[DB_1$Antes_feliz=="1"]<- "0"
DB_1$Antes_feliz[DB_1$Antes_feliz=="2"]<- "1"
DB_1$Antes_feliz[DB_1$Antes_feliz=="3"]<- "1"
DB_1$Antes_feliz[DB_1$Antes_feliz=="NA"]<- NA

DB_1$Antes_feliz <- as.numeric(DB_1$Antes_feliz)

library(dbplyr)
DB_1 <- DB_1 %>% dplyr::mutate(Antes_GHQ = (Antes_concentrarse + Antes_papel + Antes_decisiones + Antes_disfrutar + Antes_problemas + Antes_feliz + Antes_sueno + Antes_tension + Antes_superar + Antes_deprimida + Antes_confianza + Antes_valer))
head(DB_1[,c("Concentrarse","Papel", "Decisiones", "Disfrutar", "Problemas", "Feliz", "Sueno", "Tension", "Superar", "Deprimida", "Confianza", "Valer")]) #we do this to check that it has worked well
summary(DB_1$Antes_GHQ)
boxplot(DB_1$Antes_GHQ)

DB_1$Antes_GHQ2 <- cut(DB_1$Antes_GHQ, breaks = c(0, 2, 12), include.lowest = TRUE, labels = c("0", "1")) 
describe(DB_1$Antes_GHQ2)
table(DB_1$Antes_GHQ2)

# AHORA
#Concentrarse
table(DB_1$Ahora_concentrarse)
#Siempre
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="0 (Sempre)"]<- "0"
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="0 (Siempre)"]<- "0"
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="3 (Mai)"]<- "3"
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="3 (Nunca)"]<- "3"
#NA
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="no_contesta___no_aplica"]<- "NA"

#Perder el sueño
table(DB_1$Ahora_sueno)
#Siempre
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="0 (Sempre)"]<- "0"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="0 (Siempre)"]<- "0"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="3 (Mai)"]<- "3"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="3 (Nunca)"]<- "3"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="3__nunca"]<- "3"
#NA
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="no_contesta___no_aplica"]<- "NA"

#Papel útil en la vida
table(DB_1$Ahora_papel)
#Siempre
DB_1$Ahora_papel[DB_1$Ahora_papel=="0 (Sempre)"]<- "0"
DB_1$Ahora_papel[DB_1$Ahora_papel=="0 (Siempre)"]<- "0"
DB_1$Ahora_papel[DB_1$Ahora_papel=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_papel[DB_1$Ahora_papel=="3 (Mai)"]<- "3"
DB_1$Ahora_papel[DB_1$Ahora_papel=="3 (Nunca)"]<- "3"
DB_1$Ahora_papel[DB_1$Ahora_papel=="3__nunca"]<- "3"
#NA
DB_1$Ahora_papel[DB_1$Ahora_papel=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_papel[DB_1$Ahora_papel=="no_contesta___no_aplica"]<- "NA"

#Tomar decisiones
table(DB_1$Ahora_decisiones)
#Siempre
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="0 (Sempre)"]<- "0"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="0 (Siempre)"]<- "0"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="3 (Mai)"]<- "3"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="3 (Nunca)"]<- "3"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="3__nunca"]<- "3"
#NA
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="no_contesta___no_aplica"]<- "NA"

#Agobiada y en tensión
table(DB_1$Ahora_tension)
#Siempre
DB_1$Ahora_tension[DB_1$Ahora_tension=="0 (Sempre)"]<- "0"
DB_1$Ahora_tension[DB_1$Ahora_tension=="0 (Siempre)"]<- "0"
DB_1$Ahora_tension[DB_1$Ahora_tension=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_tension[DB_1$Ahora_tension=="3 (Mai)"]<- "3"
DB_1$Ahora_tension[DB_1$Ahora_tension=="3 (Nunca)"]<- "3"
DB_1$Ahora_tension[DB_1$Ahora_tension=="3__nunca"]<- "3"
#NA
DB_1$Ahora_tension[DB_1$Ahora_tension=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_tension[DB_1$Ahora_tension=="no_contesta___no_aplica"]<- "NA"

#Superar las dificultades
table(DB_1$Ahora_superar)
#Siempre
DB_1$Ahora_superar[DB_1$Ahora_superar=="0 (Sempre)"]<- "0"
DB_1$Ahora_superar[DB_1$Ahora_superar=="0 (Siempre)"]<- "0"
DB_1$Ahora_superar[DB_1$Ahora_superar=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_superar[DB_1$Ahora_superar=="3 (Mai)"]<- "3"
DB_1$Ahora_superar[DB_1$Ahora_superar=="3 (Nunca)"]<- "3"
DB_1$Ahora_superar[DB_1$Ahora_superar=="3__nunca"]<- "3"
#NA
DB_1$Ahora_superar[DB_1$Ahora_superar=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_superar[DB_1$Ahora_superar=="no_contesta___no_aplica"]<- "NA"

#Disfrutar las actividades
table(DB_1$Ahora_disfrutar)
#Siempre
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="0 (Sempre)"]<- "0"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="0 (Siempre)"]<- "0"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="3 (Mai)"]<- "3"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="3 (Nunca)"]<- "3"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="3__nunca"]<- "3"
#NA
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="no_contesta___no_aplica"]<- "NA"

#Problemas
table(DB_1$Ahora_problemas)
#Siempre
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="0 (Sempre)"]<- "0"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="0 (Siempre)"]<- "0"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="3 (Mai)"]<- "3"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="3 (Nunca)"]<- "3"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="3__nunca"]<- "3"
#NA
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="no_contesta___no_aplica"]<- "NA"

#Sentirse deprimida
table(DB_1$Ahora_deprimida)
#Siempre
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="0 (Sempre)"]<- "0"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="0 (Siempre)"]<- "0"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="3 (Mai)"]<- "3"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="3 (Nunca)"]<- "3"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="3__nunca"]<- "3"
#NA
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="no_contesta___no_aplica"]<- "NA"

#Perder confianza
table(DB_1$Ahora_confianza)
#Siempre
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="0 (Sempre)"]<- "0"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="0 (Siempre)"]<- "0"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="3 (Mai)"]<- "3"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="3 (Nunca)"]<- "3"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="3__nunca"]<- "3"
#NA
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="no_contesta___no_aplica"]<- "NA"

#Valer para nada
table(DB_1$Ahora_valer)
#Siempre
DB_1$Ahora_valer[DB_1$Ahora_valer=="0 (Sempre)"]<- "0"
DB_1$Ahora_valer[DB_1$Ahora_valer=="0 (Siempre)"]<- "0"
DB_1$Ahora_valer[DB_1$Ahora_valer=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_valer[DB_1$Ahora_valer=="3 (Mai)"]<- "3"
DB_1$Ahora_valer[DB_1$Ahora_valer=="3 (Nunca)"]<- "3"
DB_1$Ahora_valer[DB_1$Ahora_valer=="3__nunca"]<- "3"
#NA
DB_1$Ahora_valer[DB_1$Ahora_valer=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_valer[DB_1$Ahora_valer=="no_contesta___no_aplica"]<- "NA"

#Considerablemente feliz
table(DB_1$Ahora_feliz)
#Siempre
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="0 (Sempre)"]<- "0"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="0 (Siempre)"]<- "0"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="0__siempre"]<- "0"
#Nunca
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="3 (Mai)"]<- "3"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="3 (Nunca)"]<- "3"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="3__nunca"]<- "3"
#NA
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="no_contesta___no_aplica"]<- "NA"

#* concentrarse = positivo 
table(DB_1$Ahora_concentrarse)
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="1"]<- "0"
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="2"]<- "1"
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="3"]<- "1"
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="NA"]<- NA

DB_1$Ahora_concentrarse <- as.numeric(DB_1$Ahora_concentrarse)

#* sueño = negativo
table(DB_1$Ahora_sueno)
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="0"]<- "1"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="2"]<- "0"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="3"]<- "0"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="NA"]<- NA

DB_1$Ahora_sueno <- as.numeric(DB_1$Ahora_sueno)

#* papel = positivo
table(DB_1$Ahora_papel)
DB_1$Ahora_papel[DB_1$Ahora_papel=="1"]<- "0"
DB_1$Ahora_papel[DB_1$Ahora_papel=="2"]<- "1"
DB_1$Ahora_papel[DB_1$Ahora_papel=="3"]<- "1"
DB_1$Ahora_papel[DB_1$Ahora_papel=="NA"]<- NA

DB_1$Ahora_papel <- as.numeric(DB_1$Ahora_papel)

#* decisiones = positivo
table(DB_1$Ahora_decisiones)
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="1"]<- "0"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="2"]<- "1"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="3"]<- "1"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="NA"]<- NA

DB_1$Ahora_decisiones <- as.numeric(DB_1$Ahora_decisiones)

#* tension = negativo
table(DB_1$Ahora_tension)
DB_1$Ahora_tension[DB_1$Ahora_tension=="0"]<- "1"
DB_1$Ahora_tension[DB_1$Ahora_tension=="2"]<- "0"
DB_1$Ahora_tension[DB_1$Ahora_tension=="3"]<- "0"
DB_1$Ahora_tension[DB_1$Ahora_tension=="NA"]<- NA

DB_1$Ahora_tension <- as.numeric(DB_1$Ahora_tension)

#* superar = negativo
table(DB_1$Ahora_superar)
DB_1$Ahora_superar[DB_1$Ahora_superar=="0"]<- "1"
DB_1$Ahora_superar[DB_1$Ahora_superar=="2"]<- "0"
DB_1$Ahora_superar[DB_1$Ahora_superar=="3"]<- "0"
DB_1$Ahora_superar[DB_1$Ahora_superar=="NA"]<- NA

DB_1$Ahora_superar <- as.numeric(DB_1$Ahora_superar)

#* disfrutar = positivo
table(DB_1$Ahora_disfrutar)
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="1"]<- "0"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="2"]<- "1"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="3"]<- "1"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="NA"]<- NA

DB_1$Ahora_disfrutar <- as.numeric(DB_1$Ahora_disfrutar)

#* problemas = positivo
table(DB_1$Ahora_problemas)
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="1"]<- "0"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="2"]<- "1"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="3"]<- "1"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="NA"]<- NA

DB_1$Ahora_problemas <- as.numeric(DB_1$Ahora_problemas)

#* deprimida = negativo
table(DB_1$Ahora_deprimida)
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="0"]<- "1"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="2"]<- "0"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="3"]<- "0"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="NA"]<- NA

DB_1$Ahora_deprimida <- as.numeric(DB_1$Ahora_deprimida)

#* confianza = negativo
table(DB_1$Ahora_confianza)
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="0"]<- "1"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="2"]<- "0"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="3"]<- "0"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="NA"]<- NA

DB_1$Ahora_confianza <- as.numeric(DB_1$Ahora_confianza)

#* valer = negativo
table(DB_1$Ahora_valer)
DB_1$Ahora_valer[DB_1$Ahora_valer=="0"]<- "1"
DB_1$Ahora_valer[DB_1$Ahora_valer=="2"]<- "0"
DB_1$Ahora_valer[DB_1$Ahora_valer=="3"]<- "0"
DB_1$Ahora_valer[DB_1$Ahora_valer=="NA"]<- NA

DB_1$Ahora_valer <- as.numeric(DB_1$Ahora_valer)

#* feliz = positivo
table(DB_1$Ahora_feliz)
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="1"]<- "0"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="2"]<- "1"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="3"]<- "1"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="NA"]<- NA

DB_1$Ahora_feliz <- as.numeric(DB_1$Ahora_feliz)

## comprobamos
summary(DB_1)

DB_1_1 <- DB_1
# En tercer lugar, sumamos todas en una única variable
library(dbplyr)
DB_1 <- DB_1 %>% dplyr::mutate(Ahora_GHQ = (Ahora_concentrarse + Ahora_papel + Ahora_decisiones + Ahora_disfrutar + Ahora_problemas + Ahora_feliz + Ahora_sueno + Ahora_tension + Ahora_superar + Ahora_deprimida + Ahora_confianza + Ahora_valer))
head(DB_1[,c("Concentrarse","Papel", "Decisiones", "Disfrutar", "Problemas", "Feliz", "Sueno", "Tension", "Superar", "Deprimida", "Confianza", "Valer")]) #we do this to check that it has worked well
summary(DB_1$Ahora_GHQ)
boxplot(DB_1$Ahora_GHQ)

DB_1$Ahora_GHQ2 <- cut(DB_1$Ahora_GHQ, breaks = c(0, 2, 12), include.lowest = TRUE, labels = c("0", "1")) 
describe(DB_1$Ahora_GHQ2)
table(DB_1$Ahora_GHQ2)


####### CALIDAD DEL SUEÑO

# ANTES
table(DB_1$Antes_dormir)
DB_1$Antes_dormir[DB_1$Antes_dormir=="don_t_know___no_answer"]<- NA
DB_1$Antes_dormir[DB_1$Antes_dormir=="No sabe / No contesta"]<- NA
DB_1$Antes_dormir[DB_1$Antes_dormir=="No sap / No contesta"]<- NA

DB_1$Antes_dormir <- as.numeric(DB_1$Antes_dormir)

# AHORA
table(DB_1$Ahora_dormir)
DB_1$Ahora_dormir[DB_1$Ahora_dormir=="don_t_know___no_answer"]<- NA
DB_1$Ahora_dormir[DB_1$Ahora_dormir=="No sabe / No contesta"]<- NA
DB_1$Ahora_dormir[DB_1$Ahora_dormir=="No sap / No contesta"]<- NA

DB_1$Ahora_dormir <- as.numeric(DB_1$Ahora_dormir)

DB_FPBCN <- DB_1

## GÉNERO
table(DB_1$Genero)
DB_1$Genero[DB_1$Genero=="Femení"]<- "Femenino"
DB_1$Genero[DB_1$Genero=="femenino"]<- "Femenino"
DB_1$Genero[DB_1$Genero=="No binari/altre: (especificar)_____________"]<- "No binario"
DB_1$Genero[DB_1$Genero=="No binario/otro: (especificar)_____________"]<- "No binario"
DB_1$Genero[DB_1$Genero=="No contesta / No aplica"]<- "NA"

DB1_7 <- DB_1

## NACIONALIDAD
table(DB_1$Nacimiento)
DB_1$Nacimiento[DB_1$Nacimiento=="_ndia"]<- "India"
DB_1$Nacimiento[DB_1$Nacimiento=="alemania"]<- "Alemania"
DB_1$Nacimiento[DB_1$Nacimiento=="bangladesh"]<- "Bangladesh "
DB_1$Nacimiento[DB_1$Nacimiento=="belgica"]<- "Bélgica"
DB_1$Nacimiento[DB_1$Nacimiento=="Belgica"]<- "Bélgica"
DB_1$Nacimiento[DB_1$Nacimiento=="Canada"]<- "Canadá"
DB_1$Nacimiento[DB_1$Nacimiento=="Chile\r\n"]<- "Chile"
DB_1$Nacimiento[DB_1$Nacimiento=="china"]<- "China"
DB_1$Nacimiento[DB_1$Nacimiento=="Clara de marfil"]<- "Costa de Marfil"
DB_1$Nacimiento[DB_1$Nacimiento=="Columbia"]<- "Colombia"
DB_1$Nacimiento[DB_1$Nacimiento=="Costa rica"]<- "Costa Rica"
DB_1$Nacimiento[DB_1$Nacimiento=="El salvador"]<- "El Salvador"
DB_1$Nacimiento[DB_1$Nacimiento=="Els Estats Units"]<- "EE.UU."
DB_1$Nacimiento[DB_1$Nacimiento=="espana"]<- "España"
DB_1$Nacimiento[DB_1$Nacimiento=="Espanya"]<- "España"
DB_1$Nacimiento[DB_1$Nacimiento=="Filipinas\r\n"]<- "Filipinas"
DB_1$Nacimiento[DB_1$Nacimiento=="França"]<- "Francia"
DB_1$Nacimiento[DB_1$Nacimiento=="francia"]<- "Francia"
DB_1$Nacimiento[DB_1$Nacimiento=="holanda"]<- "Holanda"
DB_1$Nacimiento[DB_1$Nacimiento=="Hungria"]<- "Hungría"
DB_1$Nacimiento[DB_1$Nacimiento=="hungria"]<- "Hungría"
DB_1$Nacimiento[DB_1$Nacimiento=="italia"]<- "Italia"
DB_1$Nacimiento[DB_1$Nacimiento=="Italia España"]<- "España"
DB_1$Nacimiento[DB_1$Nacimiento=="Los Estados Unidos"]<- "EE.UU."
DB_1$Nacimiento[DB_1$Nacimiento=="los_estados_unidos"]<- "EE.UU."
DB_1$Nacimiento[DB_1$Nacimiento=="estados_unidos"]<- "EE.UU."
DB_1$Nacimiento[DB_1$Nacimiento=="marruecos"]<- "Marruecos"
DB_1$Nacimiento[DB_1$Nacimiento=="mexico"]<- "México"
DB_1$Nacimiento[DB_1$Nacimiento=="Mexico"]<- "México"
DB_1$Nacimiento[DB_1$Nacimiento=="No contesta / No aplica"]<- "NA"
DB_1$Nacimiento[DB_1$Nacimiento=="pakistan"]<- "Pakistán"
DB_1$Nacimiento[DB_1$Nacimiento=="pakist_n"]<- "Pakistán"
DB_1$Nacimiento[DB_1$Nacimiento=="Reino Unido España"]<- "España"
DB_1$Nacimiento[DB_1$Nacimiento=="reino_unido"]<- "Reino Unido"
DB_1$Nacimiento[DB_1$Nacimiento=="rep_dominicana"]<- "República Dominicana"
DB_1$Nacimiento[DB_1$Nacimiento=="Republica Dominicana"]<- "República Dominicana"
DB_1$Nacimiento[DB_1$Nacimiento=="Republica Checa"]<- "República Checa"
DB_1$Nacimiento[DB_1$Nacimiento=="suiza"]<- "Suiza"
DB_1$Nacimiento[DB_1$Nacimiento=="Índia"]<- "India"
DB_1$Nacimiento[DB_1$Nacimiento=="india"]<- "India"
DB_1$Nacimiento[DB_1$Nacimiento=="Russia"]<- "Rusia"
DB_1$Nacimiento[DB_1$Nacimiento=="rusia"]<- "Rusia"
DB_1$Nacimiento[DB_1$Nacimiento=="Ucraina"]<- "Ucrania"
DB_1$Nacimiento[DB_1$Nacimiento=="turquia"]<- "Turquia"

DB1_8<-DB_1

#cambiar los otro
DB_1$ID[DB_1$Nacimiento=="otro__especificar"]


#esto nos dice qué id (es decir, que participantes) respondieron "otro" en la pregunta nacimiento

#Cuando los tienes identificados (ID), miras qué han respondido en Nacimiento_otro.
#La respuesta que hayan puesto va a sustituir a la de "otro" en Nacimiento

DB_1$Nacimiento_otro[DB_1$ID=="110065915"]
DB_1$Nacimiento[DB_1$ID=="110065915"]<-"Holanda"

DB_1$Nacimiento_otro[DB_1$ID=="101830971"] 
DB_1$Nacimiento[DB_1$ID=="101830971"] <-"Hungría"

DB_1$Nacimiento_otro[DB_1$ID=="102218486"]  
DB_1$Nacimiento[DB_1$ID=="102218486"]  <-"Turquia"

DB_1$Nacimiento_otro[DB_1$ID=="104231132"] 
DB_1$Nacimiento[DB_1$ID=="104231132"]<- "Holanda"

DB_1$Nacimiento_otro[DB_1$ID=="104514967"] 
DB_1$Nacimiento[DB_1$ID=="104514967"] <-"Holanda"

DB_1$Nacimiento_otro[DB_1$ID=="104858668"]  
DB_1$Nacimiento[DB_1$ID=="104858668"] <-"Rusia"

DB_1$Nacimiento_otro[DB_1$ID=="104858694"] 
DB_1$Nacimiento[DB_1$ID=="104858694"]<-"Alemania"

DB_1$Nacimiento_otro[DB_1$ID=="113455369"] 
DB_1$Nacimiento[DB_1$ID=="113455369"]<- "Suiza"

DB_1$Nacimiento_otro[DB_1$ID=="119003308"]
DB_1$Nacimiento[DB_1$ID=="119003308"]<-"Filipinas"

DB_1$Nacimiento_otro[DB_1$ID=="126962981"] 
DB_1$Nacimiento[DB_1$ID=="126962981"] <-"Alemania"

## CHECK
table(DB_1$Nacimiento)

## ESTUDIOS
table(DB_1$Estudios)
#Ninguno
DB_1$Estudios[DB_1$Estudios=="no_s__leer_ni_escribir"]<- "Ninguno"
DB_1$Estudios[DB_1$Estudios=="No sé leer ni escribir."]<- "Ninguno"

#Primarios incompletos
DB_1$Estudios[DB_1$Estudios=="Primaris incomplets: sé llegir i escriure sense haver finalitzat l'educació primària."]<- "Primarios incompletos"
DB_1$Estudios[DB_1$Estudios=="primarios_incom"]<- "Primarios incompletos"
DB_1$Estudios[DB_1$Estudios=="Primarios incompletos: sé leer ni escribir sin haber finalizado la educación primaria."]<- "Primarios incompletos"
#Primarios
DB_1$Estudios[DB_1$Estudios=="primarios"]<- "Primarios"
DB_1$Estudios[DB_1$Estudios=="Primarios completos: primaria LOGSE completa o cinco cursos aprobados d'EGB."]<- "Primarios"
DB_1$Estudios[DB_1$Estudios=="Primaris complets: primària LOGSE completa o cinc cursos aprovats d'EGB."]<- "Primarios"
#Secundarios
DB_1$Estudios[DB_1$Estudios=="secundarios"]<- "Secundarios"
DB_1$Estudios[DB_1$Estudios=="Primera etapa d'educació secundaria: graduat escolar, batxillerat elemental, EGB o ESO completa."]<- "Secundarios"
DB_1$Estudios[DB_1$Estudios=="Primera etapa de educación secundaria: graduado escolar, bachillerato elemental, EGB o ESO completa."]<- "Secundarios"
#Bachillerato
DB_1$Estudios[DB_1$Estudios=="bachillerato"]<- "Bachillerato"
DB_1$Estudios[DB_1$Estudios=="Bachillerato: Bachillerato superior, BUP, bachillerato plan nuevo, PREU o COU."]<- "Bachillerato"
DB_1$Estudios[DB_1$Estudios=="bachillerato__bachillerato_superior__bup"]<- "Bachillerato"
DB_1$Estudios[DB_1$Estudios=="Batxillerat: Batxillerat superior, BUP, batxillerat pla nou, PREU o COU."]<- "Bachillerato"
#FP medio
DB_1$Estudios[DB_1$Estudios=="fp_de_grado_medio__oficial_a_industrial_"]<- "FP medio"
DB_1$Estudios[DB_1$Estudios=="FP de grau mitjà: Oficialia industrial, FPI, cicles formatius de grau mitjà."]<- "FP medio"
DB_1$Estudios[DB_1$Estudios=="FP de grado medio: Oficialía industrial, FPI, ciclos formativos de grado medio."]<- "FP medio"
#FP superior
DB_1$Estudios[DB_1$Estudios=="fp_superior"]<- "FP superior"
DB_1$Estudios[DB_1$Estudios=="FP de grado superior: maestría industrial, FPII, ciclos formativos de grado superior"]<- "FP superior"
DB_1$Estudios[DB_1$Estudios=="FP de grau superior: mestria industrial, FPII, cicles formatius de grau superior"]<- "FP superior"
#Universitarios primer ciclo
DB_1$Estudios[DB_1$Estudios=="universitarios_pc"]<- "Universitarios PC"
DB_1$Estudios[DB_1$Estudios=="Enseñanzas universitarias de primer ciclo: diplomatura universitaria, arquitectura e ingeniería técnica"]<- "Universitarios PC"
DB_1$Estudios[DB_1$Estudios=="Ensenyança universitària de primer cicle: diplomatura universitària, arquitectura enginyeria tècnica"]<- "Universitarios PC"
DB_1$Estudios[DB_1$Estudios=="ense_anzas_universitarias_de_primer_cicl"]<- "Universitarios PC"
#Universitarios segundo ciclo
DB_1$Estudios[DB_1$Estudios=="universitarios_sc"]<- "Universitarios SC"
DB_1$Estudios[DB_1$Estudios=="ense_anzas_universitarias_de_segundo_cic"]<- "Universitarios SC"
DB_1$Estudios[DB_1$Estudios=="Ensenyança universitària de segon cicle: grau, llicenciatura, arquitectura y enginyeria"]<- "Universitarios SC"
DB_1$Estudios[DB_1$Estudios=="Enseñanzas universitarias de segundo ciclo: grado, licenciatura, arquitectura y ingeniería"]<- "Universitarios SC"
#Universitarios posgrado
DB_1$Estudios[DB_1$Estudios=="universitarios_pg"]<- "Universitarios PG"
DB_1$Estudios[DB_1$Estudios=="Estudios universitarios de doctorado, postgrado, máster, MIR o análogo."]<- "Universitarios PG"
DB_1$Estudios[DB_1$Estudios=="estudios_universitarios_de_doctorado__po"]<- "Universitarios PG"
DB_1$Estudios[DB_1$Estudios=="Estudis universitaris de doctorat, postgrau, màster, MIR o anàloga."]<- "Universitarios PG"
#No aplica
DB_1$Estudios[DB_1$Estudios=="No contesta / No aplica"]<- "NA"

## CHECK
table(DB_1$Estudios)

##### SITUACIÓN LABORAL
# ANTES
table(DB_1$Antes_laboral)
#* Trabajo a tiempo completo
DB_1$Antes_laboral[DB_1$Antes_laboral=="trabajando_a_tiempo_completo"]<- "Trabajo completo"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Trabajando a tiempo completo"]<- "Trabajo completo"
DB_1$Antes_laboral[DB_1$Antes_laboral=="trabajo_completo"]<- "Trabajo completo"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Treballant a temps complet"]<- "Trabajo completo"
#* Trabajo a tiempo parcial
DB_1$Antes_laboral[DB_1$Antes_laboral=="trabajando_a_tiempo_parcial"]<- "Trabajo parcial"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Trabajando a tiempo parcial"]<- "Trabajo parcial"
DB_1$Antes_laboral[DB_1$Antes_laboral=="trabajo_parcial"]<- "Trabajo parcial"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Treballant a temps parcial"]<- "Trabajo parcial"
#* Trabajos casuales
DB_1$Antes_laboral[DB_1$Antes_laboral=="Trabajos casuales"]<- "Trabajo casual"
DB_1$Antes_laboral[DB_1$Antes_laboral=="trabajo_casual"]<- "Trabajo casual"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Treballs casuals"]<- "Trabajo casual"
#* Cuidando de la familia
DB_1$Antes_laboral[DB_1$Antes_laboral=="cuidando"]<- "Cuidando"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Cuidando de la casa/familia"]<- "Cuidando"
DB_1$Antes_laboral[DB_1$Antes_laboral=="cuidando_de_la_casa_familia"]<- "Cuidando"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Tenint cura de la casa/família"]<- "Cuidando"
#* Desempleada
DB_1$Antes_laboral[DB_1$Antes_laboral=="Aturat"]<- "Desempleada"
DB_1$Antes_laboral[DB_1$Antes_laboral=="desempleado"]<- "Desempleada"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Desempleado"]<- "Desempleada"
#* Jubilada
DB_1$Antes_laboral[DB_1$Antes_laboral=="Jubilat"]<- "Jubilada"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Jubilado"]<- "Jubilada"
DB_1$Antes_laboral[DB_1$Antes_laboral=="jubilado"]<- "Jubilada"
#* Estudiante
DB_1$Antes_laboral[DB_1$Antes_laboral=="estudiante"]<- "Estudiante"
DB_1$Antes_laboral[DB_1$Antes_laboral=="en_pr_cticas_estudiante"]<- "Estudiante"
DB_1$Antes_laboral[DB_1$Antes_laboral=="En pràctiques/estudiant"]<- "Estudiante"
DB_1$Antes_laboral[DB_1$Antes_laboral=="En prácticas/estudiante"]<- "Estudiante"
#* Incapaz de trabajar por motivos de salud
DB_1$Antes_laboral[DB_1$Antes_laboral=="incapaz_salud"]<- "Incapaz por salud"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Incapaz de trabajar por razones de salud"]<- "Incapaz por salud"
DB_1$Antes_laboral[DB_1$Antes_laboral=="Incapaç de treballar por motius de salut"]<- "Incapaz por salud"
#* No contesta
DB_1$Antes_laboral[DB_1$Antes_laboral=="no_contesta___no_aplica"]<- "NA"
DB_1$Antes_laboral[DB_1$Antes_laboral=="No contesta / No aplica"]<- "NA"
# check 
table(DB_1$Antes_laboral)

# AHORA
table(DB_1$Ahora_laboral)
#* Trabajo a tiempo completo
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="trabajando_a_tiempo_completo"]<- "Trabajo completo"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Trabajando a tiempo completo"]<- "Trabajo completo"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="trabajo_completo"]<- "Trabajo completo"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Treballant a temps complet"]<- "Trabajo completo"
#* Trabajo a tiempo parcial
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="trabajando_a_tiempo_parcial"]<- "Trabajo parcial"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Trabajando a tiempo parcial"]<- "Trabajo parcial"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="trabajo_parcial"]<- "Trabajo parcial"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Treballant a temps parcial"]<- "Trabajo parcial"
#* Trabajos casuales
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Trabajos casuales"]<- "Trabajo casual"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="trabajo_casual"]<- "Trabajo casual"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Treballs casuals"]<- "Trabajo casual"
#* Cuidando de la familia
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="cuidando"]<- "Cuidando"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Cuidando de la casa/familia"]<- "Cuidando"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="cuidando_de_la_casa_familia"]<- "Cuidando"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Tenint cura de la casa/família"]<- "Cuidando"
#* Desempleada
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Aturat"]<- "Desempleada"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="desempleado"]<- "Desempleada"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Desempleado"]<- "Desempleada"
#* Jubilada
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Jubilat"]<- "Jubilada"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Jubilado"]<- "Jubilada"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="jubilado"]<- "Jubilada"
#* Estudiante
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="estudiante"]<- "Estudiante"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="en_pr_cticas_estudiante"]<- "Estudiante"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="En pràctiques/estudiant"]<- "Estudiante"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="En prácticas/estudiante"]<- "Estudiante"
#* Incapaz de trabajar por motivos de salud
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="incapaz_salud"]<- "Incapaz por salud"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Incapaz de trabajar por razones de salud"]<- "Incapaz por salud"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="Incapaç de treballar por motius de salut"]<- "Incapaz por salud"
#* No contesta
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="no_contesta___no_aplica"]<- "NA"

## CHECK
table(DB_1$Ahora_laboral)


##### SITUACIÓN ECONÓMICA

# ANTES
table(DB_1$Antes_monetaria)
#*Comodamente
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="comodamente"]<- "Cómodamente"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="viv_a_holgadamente"]<- "Cómodamente"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="Vive cómodamente"]<- "Cómodamente"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="Vivia còmodament"]<- "Cómodamente"
#*Lo justo
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="option_2"]<- "Lo justo"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="Tenía lo justo para vivir"]<- "Lo justo"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="tiene_lo_justo_para_vivir"]<- "Lo justo"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="Tenia el just per viure"]<- "Lo justo"

#*No llega
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="no_llega"]<- "No llega"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="option_1"]<- "Lo justo"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="No podia arribar a final de mes"]<- "Lo justo"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="No podía llegar a fin de mes"]<- "Lo justo"
#*NA
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="no_sabe_no_contesta"]<- "NA"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="No sap/no contesta"]<- "NA"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="No sabe/no contesta"]<- "NA"
DB_1$Antes_monetaria[DB_1$Antes_monetaria=="NA"] <- NA

summarytools::freq(DB_1$Antes_monetaria)

## check
table(DB_1$Antes_monetaria)

# AHORA
table(DB_1$Ahora_monetaria)
#*Comodamente
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="comodamente"]<- "Cómodamente"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="viv_a_holgadamente"]<- "Cómodamente"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="Vive cómodamente"]<- "Cómodamente"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="Vivia còmodament"]<- "Cómodamente"
#*Lo justo
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="justo"]<- "Lo justo"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="Tiene lo justo para vivir"]<- "Lo justo"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="tiene_lo_justo_para_vivir"]<- "Lo justo"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="Té el  just per viure"]<- "Lo justo"

#*No llega
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="no_llega"]<- "No llega"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="no_puede_llegar_a_fin_de_mes"]<- "Lo justo"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="No pot arribar a final de mes"]<- "Lo justo"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="No puede llegar a fin de mes"]<- "Lo justo"
#*NA
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="no_hay_informaci_n_no_sabe"]<- "NA"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="No hi ha informació/no sap"]<- "NA"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="No hay información/no sabe"]<- "NA"
DB_1$Ahora_monetaria[DB_1$Ahora_monetaria=="NA"] <- NA
## CHECK
table(DB_1$Ahora_monetaria)

####******************************########
####2. Chequeamos que las variables sean del tipo que queremos  ####
####******************************#######
summary(DB_1) 


DB_1$Edad <- as.numeric(DB_1$Edad)
summary(DB_1)

DB_FPBCN <- DB_1

###### CATEGORIZAMOS LAS VARIABLES SOCIOEDEMOGRÁFICAS ######
describe(DB_1)
# NACIONALIDAD
describe(DB_1$Nacimiento)
summary(DB_1$Nacimiento)
table(DB_1$Nacimiento)

DB_1$Nacimiento_2[DB_1$Nacimiento=="España"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Alemania"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Australia"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Austria"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Bélgica"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Bulgaria"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Canadá"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="EE.UU."] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Francia"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Georgia"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Holanda"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Hungría"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Israel"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Italia"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Japon"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Lituania"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Noruega"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Polonia"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Portugal"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Reino Unido"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="República Checa"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Rumania"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Rusia"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Suecia"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Suiza"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Ucrania"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Albania"] <- "Global North"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Turquia"] <- "Global North"


DB_1$Nacimiento_2[DB_1$Nacimiento=="Argelia"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Argentina"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Bangladesh"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Bolivia"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Brasil"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Chile"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="China"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Colombia"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Costa de Marfil"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Costa Rica"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Cuba"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Ecuador"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="El Salvador"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Filipinas"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Guinea Ecuatorial"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Honduras"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="India"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Libano"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Marruecos"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="México"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Nicaragua"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Pakistán"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Paraguay"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Perú"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Puerto Rico"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="República Dominicana"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Senegal"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Taiwan"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Uruguay"] <- "Global South"
DB_1$Nacimiento_2[DB_1$Nacimiento=="Venezuela"] <- "Global South"



table(DB_1$Nacimiento_2)
is.na(DB_1$Nacimiento_2)
summarytools::freq(DB_1$Nacimiento_2)

### TRANSFORMAR LA VARIABLE DE ESTUDIOS EN UNIVERSITARIOS Y NO UNIVERSITARIOS
describe(DB_1$Estudios)
summarytools::freq(DB_1$Estudios)
table(DB_1$Estudios)

DB_1$Estudios2[DB_1$Estudios=="Universitarios PG"] <- "Universitarios"
DB_1$Estudios2[DB_1$Estudios=="Universitarios SC"] <- "Universitarios"
DB_1$Estudios2[DB_1$Estudios=="Universitarios PC"] <- "Universitarios"

DB_1$Estudios2[DB_1$Estudios=="Bachillerato"] <- "No universitarios"
DB_1$Estudios2[DB_1$Estudios=="FP superior"] <- "No universitarios"
DB_1$Estudios2[DB_1$Estudios=="FP medio"] <- "No universitarios"
DB_1$Estudios2[DB_1$Estudios=="Secundarios"] <- "No universitarios"
DB_1$Estudios2[DB_1$Estudios=="Primarios"] <- "No universitarios"
DB_1$Estudios2[DB_1$Estudios=="Primarios incompletos"] <- "No universitarios"
DB_1$Estudios2[DB_1$Estudios=="Ninguno"] <- "No universitarios"

summarytools::freq(DB_1$Estudios2)
table(DB_1$Estudios2)

### TRANSFORMAR LAS VARIABLES DE SITUACIÓN ECONÓMICA EN BINARIAS
DB_1$Antes_monetaria2[DB_1$Antes_monetaria=="Cómodamente"] <- "0"
DB_1$Antes_monetaria2[DB_1$Antes_monetaria=="No llega"] <- "1"
DB_1$Antes_monetaria2[DB_1$Antes_monetaria=="Lo justo"] <- "1"
table(DB_1$Antes_monetaria2)
summarytools::freq(DB_1$Antes_monetaria2)

DB_1$Ahora_monetaria2[DB_1$Ahora_monetaria=="Cómodamente"] <- "0"
DB_1$Ahora_monetaria2[DB_1$Ahora_monetaria=="No llega"] <- "1"
DB_1$Ahora_monetaria2[DB_1$Ahora_monetaria=="Lo justo"] <- "1"

summarytools::freq(DB_1$Ahora_monetaria2)

DB_2_2 <- DB_1

### TRANSFORMAR LAS VARIABLES DE SITUACIÓN LABORAL EN BINARIAS
DB_1$Antes_laboral2[DB_1$Antes_laboral=="Trabajo completo"] <- "0"
DB_1$Antes_laboral2[DB_1$Antes_laboral=="Trabajo parcial"] <- "0"
DB_1$Antes_laboral2[DB_1$Antes_laboral=="Trabajo casual"] <- "1"
DB_1$Antes_laboral2[DB_1$Antes_laboral=="Desempleada"] <- "1"
DB_1$Antes_laboral2[DB_1$Antes_laboral=="Cuidando"] <- "1"
DB_1$Antes_laboral2[DB_1$Antes_laboral=="Estudiante"] <- "1"
DB_1$Antes_laboral2[DB_1$Antes_laboral=="Incapaz por salud"] <- "1"
DB_1$Antes_laboral2[DB_1$Antes_laboral=="Jubilada"] <- "1"

summarytools::freq(DB_1$Antes_laboral2)

DB_1$Ahora_laboral2[DB_1$Ahora_laboral=="Trabajo completo"] <- "0"
DB_1$Ahora_laboral2[DB_1$Ahora_laboral=="Trabajo parcial"] <- "0"
DB_1$Ahora_laboral2[DB_1$Ahora_laboral=="Trabajo casual"] <- "1"
DB_1$Ahora_laboral2[DB_1$Ahora_laboral=="Desempleada"] <- "1"
DB_1$Ahora_laboral2[DB_1$Ahora_laboral=="Cuidando"] <- "1"
DB_1$Ahora_laboral2[DB_1$Ahora_laboral=="Estudiante"] <- "1"
DB_1$Ahora_laboral2[DB_1$Ahora_laboral=="Incapaz por salud"] <- "1"
DB_1$Ahora_laboral2[DB_1$Ahora_laboral=="Jubilada"] <- "1"

summarytools::freq(DB_1$Ahora_laboral2)

# VARIABLES DE CUIDADOS
# antes
table(DB_1$Antes_cuidado)
DB_1$Antes_cuidado[DB_1$Antes_cuidado == "si"] <- "Si"
DB_1$Antes_cuidado[DB_1$Antes_cuidado == "no"] <- "No"
DB_1$Antes_cuidado[DB_1$Antes_cuidado == "NA"] <- NA
DB_1$Antes_cuidado[DB_1$Antes_cuidado == "no_contesta___no_aplica"] <- NA
DB_1$Antes_cuidado[DB_1$Antes_cuidado == "No contesta / No aplica"] <- NA

table(DB_1$Antes_cuidado)
summarytools::freq(DB_1$Antes_cuidado)

# ahora
table(DB_1$Ahora_cuidado)

DB_1$Ahora_cuidado[DB_1$Ahora_cuidado == "si"] <- "Si"
DB_1$Ahora_cuidado[DB_1$Ahora_cuidado == "no"] <- "No"
DB_1$Ahora_cuidado[DB_1$Ahora_cuidado == "NA"] <- NA
DB_1$Ahora_cuidado[DB_1$Ahora_cuidado == "no_contesta___no_aplica"] <- NA
DB_1$Ahora_cuidado[DB_1$Ahora_cuidado == "No contesta / No aplica"] <- NA
summarytools::freq(DB_1$Ahora_cuidado)

table(DB_1$Ahora_cuidado)
summarytools::freq(DB_1$Ahora_cuidado)

## VISITAS ##

# SUPERILLA Y PLAYA
table(DB_1$Cambios_visitas_A)
summarytools::freq(DB_1$Cambios_visitas_A)

DB_1$Visitas_A[DB_1$Cambios_visitas_A == "Aumentado mucho"] <- "1"
DB_1$Visitas_A[DB_1$Cambios_visitas_A == "Aumentado"] <- "1"
DB_1$Visitas_A[DB_1$Cambios_visitas_A == "No cambio"] <- "1"
DB_1$Visitas_A[DB_1$Cambios_visitas_A == "Disminuido"] <- "0"
DB_1$Visitas_A[DB_1$Cambios_visitas_A == "Disminuido mucho"] <- "0"

table(DB_1$Visitas_A)
summarytools::freq(DB_1$Visitas_A)


# ESPACIOS A MENOS DE 15 MIN
table(DB_1$Cambios_visitas_B)
summarytools::freq(DB_1$Cambios_visitas_B)

DB_1$Visitas_B[DB_1$Cambios_visitas_B == "Aumentado mucho"] <- "1"
DB_1$Visitas_B[DB_1$Cambios_visitas_B == "Aumentado"] <- "1"
DB_1$Visitas_B[DB_1$Cambios_visitas_B == "No cambio"] <- "1"
DB_1$Visitas_B[DB_1$Cambios_visitas_B == "Disminuido"] <- "0"
DB_1$Visitas_B[DB_1$Cambios_visitas_B == "Disminuido mucho"] <- "0"

table(DB_1$Visitas_B)
summarytools::freq(DB_1$Visitas_B)

# ESPACIOS A MÁS DE 15 MIN
table(DB_1$Cambios_visitas_C)
summarytools::freq(DB_1$Cambios_visitas_C)

DB_1$Visitas_C[DB_1$Cambios_visitas_C == "Aumentado mucho"] <- "1"
DB_1$Visitas_C[DB_1$Cambios_visitas_C == "Aumentado"] <- "1"
DB_1$Visitas_C[DB_1$Cambios_visitas_C == "No cambio"] <- "1"
DB_1$Visitas_C[DB_1$Cambios_visitas_C == "Disminuido"] <- "0"
DB_1$Visitas_C[DB_1$Cambios_visitas_C == "Disminuido mucho"] <- "0"

table(DB_1$Visitas_C)
summarytools::freq(DB_1$Visitas_C)

# ESPACIOS EN TORNO A LA CIUDAD
table(DB_1$Cambios_visitas_D)
summarytools::freq(DB_1$Cambios_visitas_D)

DB_1$Visitas_D[DB_1$Cambios_visitas_D == "Aumentado mucho"] <- "1"
DB_1$Visitas_D[DB_1$Cambios_visitas_D == "Aumentado"] <- "1"
DB_1$Visitas_D[DB_1$Cambios_visitas_D == "No cambio"] <- "1"
DB_1$Visitas_D[DB_1$Cambios_visitas_D == "Disminuido"] <- "0"
DB_1$Visitas_D[DB_1$Cambios_visitas_D == "Disminuido mucho"] <- "0"

table(DB_1$Visitas_D)
summarytools::freq(DB_1$Visitas_D)


## TIEMPO ##

# SUPERILLA Y PLAYA
table(DB_1$Cambios_tiempo_A)
summarytools::freq(DB_1$Cambios_tiempo_A)

DB_1$Tiempo_A[DB_1$Cambios_tiempo_A == "Aumentado mucho"] <- "1"
DB_1$Tiempo_A[DB_1$Cambios_tiempo_A == "Aumentado"] <- "1"
DB_1$Tiempo_A[DB_1$Cambios_tiempo_A == "No cambio"] <- "1"
DB_1$Tiempo_A[DB_1$Cambios_tiempo_A == "Disminuido"] <- "0"
DB_1$Tiempo_A[DB_1$Cambios_tiempo_A == "Disminuido mucho"] <- "0"

table(DB_1$Tiempo_A)
summarytools::freq(DB_1$Tiempo_A)

# ESPACIOS A MENOS DE 15 MIN
table(DB_1$Cambios_tiempo_B)
summarytools::freq(DB_1$Cambios_tiempo_B)

DB_1$Tiempo_B[DB_1$Cambios_tiempo_B == "Aumentado mucho"] <- "1"
DB_1$Tiempo_B[DB_1$Cambios_tiempo_B == "Aumentado"] <- "1"
DB_1$Tiempo_B[DB_1$Cambios_tiempo_B == "No cambio"] <- "1"
DB_1$Tiempo_B[DB_1$Cambios_tiempo_B == "Disminuido"] <- "0"
DB_1$Tiempo_B[DB_1$Cambios_tiempo_B == "Disminuido mucho"] <- "0"

table(DB_1$Tiempo_B)
summarytools::freq(DB_1$Tiempo_B)

# ESPACIOS A MÁS DE 15 MIN
table(DB_1$Cambios_tiempo_C)
summarytools::freq(DB_1$Cambios_tiempo_C)

DB_1$Tiempo_C[DB_1$Cambios_tiempo_C == "Aumentado mucho"] <- "1"
DB_1$Tiempo_C[DB_1$Cambios_tiempo_C == "Aumentado"] <- "1"
DB_1$Tiempo_C[DB_1$Cambios_tiempo_C == "No cambio"] <- "1"
DB_1$Tiempo_C[DB_1$Cambios_tiempo_C == "Disminuido"] <- "0"
DB_1$Tiempo_C[DB_1$Cambios_tiempo_C == "Disminuido mucho"] <- "0"

table(DB_1$Tiempo_C)
summarytools::freq(DB_1$Tiempo_C)

# ESPACIOS EN TORNO A LA CIUDAD
table(DB_1$Cambios_tiempo_D)
summarytools::freq(DB_1$Cambios_tiempo_D)

DB_1$Tiempo_D[DB_1$Cambios_tiempo_D == "Aumentado mucho"] <- "1"
DB_1$Tiempo_D[DB_1$Cambios_tiempo_D == "Aumentado"] <- "1"
DB_1$Tiempo_D[DB_1$Cambios_tiempo_D == "No cambio"] <- "1"
DB_1$Tiempo_D[DB_1$Cambios_tiempo_D == "Disminuido"] <- "0"
DB_1$Tiempo_D[DB_1$Cambios_tiempo_D == "Disminuido mucho"] <- "0"

table(DB_1$Tiempo_D)
summarytools::freq(DB_1$Tiempo_D)

DB_FPBCN <- DB_1
DB_2 <- DB_1

# CATEGORIZACIÓN DE LOS ESPACIOS A = SUPERILLA/PLAYA, B = MENOS DE 15 MIN, C = MÁS DE 15 MIN, D = LÍMITES DE LA CIUDAD

# SUPERILLA/PLAYA
DB_2 <- DB_2 %>%  dplyr::mutate(Cambios_A = case_when( 
  Visitas_A == "0" & Tiempo_A == "0" ~ "A",
  Visitas_A == "1" & Tiempo_A == "1" ~ "B",
  Visitas_A == "0" & Tiempo_A == "1" ~ "C",
  Visitas_A == "1" & Tiempo_A == "0" ~ "C",
  is.na(Visitas_A) & Tiempo_A == "1"  ~ "B",
  Visitas_A == "1"  & is.na(Tiempo_A)  ~ "B",
  is.na(Visitas_A) & Tiempo_A == "0"  ~ "A",
  Visitas_A == "0"  & is.na(Tiempo_A)  ~ "A",
))
table(DB_2$Cambios_A)
summarytools::freq(DB_2$Cambios_A)

# ESPACIOS < 15 MIN
DB_2 <- DB_2 %>%  dplyr::mutate(Cambios_B = case_when( 
  Visitas_B == "0" & Tiempo_B == "0" ~ "A",
  Visitas_B == "1" & Tiempo_B == "1" ~ "B",
  Visitas_B == "0" & Tiempo_B == "1" ~ "C",
  Visitas_B == "1" & Tiempo_B == "0" ~ "C",
  is.na(Visitas_B) & Tiempo_B == "1"  ~ "B",
  Visitas_B == "1"  & is.na(Tiempo_B)  ~ "B",
  is.na(Visitas_B) & Tiempo_B == "0"  ~ "A",
  Visitas_B == "0"  & is.na(Tiempo_B)  ~ "A",
))
table(DB_2$Cambios_B)
summarytools::freq(DB_2$Cambios_B)

# ESPACIOS > 15 MIN
DB_2 <- DB_2 %>%  dplyr::mutate(Cambios_C = case_when( 
  Visitas_C == "0" & Tiempo_C == "0" ~ "A",
  Visitas_C == "1" & Tiempo_C == "1" ~ "B",
  Visitas_C == "0" & Tiempo_C == "1" ~ "C",
  Visitas_C == "1" & Tiempo_C == "0" ~ "C",
  is.na(Visitas_C) & Tiempo_C == "1"  ~ "B",
  Visitas_C == "1"  & is.na(Tiempo_C)  ~ "B",
  is.na(Visitas_C) & Tiempo_C == "0"  ~ "A",
  Visitas_C == "0"  & is.na(Tiempo_C)  ~ "A",
))
table(DB_2$Cambios_C)
summarytools::freq(DB_2$Cambios_C)

# ALREDEDORES DE LA CIUDAD
DB_2 <- DB_2 %>%  dplyr::mutate(Cambios_D = case_when( 
  Visitas_D == "0" & Tiempo_D == "0" ~ "A",
  Visitas_D == "1" & Tiempo_D == "1" ~ "B",
  Visitas_D == "0" & Tiempo_D == "1" ~ "C",
  Visitas_D == "1" & Tiempo_D == "0" ~ "C",
  is.na(Visitas_D) & Tiempo_D == "1"  ~ "B",
  Visitas_D == "1"  & is.na(Tiempo_D)  ~ "B",
  is.na(Visitas_D) & Tiempo_D == "0"  ~ "A",
  Visitas_D == "0"  & is.na(Tiempo_D)  ~ "A",
))
table(DB_2$Cambios_D)
summarytools::freq(DB_2$Cambios_D)

### CREAR VARIABLE QUE UNE ESPACIOS A Y B ###
# A = SÓLO REPORTAN DISMINUCIÓN DEL USO DE LA SUPERILLA Y LOS ESPACIOS <15 MIN
# B = SÓLO REPORTAN AUMENTO/MANTENIMIENTO DEL USO DE LA SUPERILLA Y LOS ESPACIOS < 15 MIN
# C = EN UN ESPACIO REPORTAN AUMENTO/MANTENIMIENTO Y EN EL OTRO DISMINUCIÓN

DB_2 <- DB_2 %>%  dplyr::mutate(Cambios_AB = case_when( 
  Cambios_B == "A" & Cambios_A == "A" ~ "A",
  Cambios_B == "B" & Cambios_A == "B" ~ "B",
  Cambios_B == "C" & Cambios_A == "C" ~ "C",
  Cambios_B == "A" & Cambios_A == "B" ~ "C",
  Cambios_B == "B" & Cambios_A == "A" ~ "C",
  is.na(Cambios_B) & Cambios_A == "B"  ~ "B",
  Cambios_B == "B"  & is.na(Cambios_A)  ~ "B",
  is.na(Cambios_B) & Cambios_A == "A"  ~ "A",
  Cambios_B == "A"  & is.na(Cambios_A)  ~ "A",
  TRUE ~ "C"
))
table(DB_2$Cambios_AB)
summarytools::freq(DB_2$Cambios_AB)

#VAMOS A CONSTUIR LA OTRA PARCIAL (CD) PARA UNIRLA DESPUÉS A LA AB
DB_2 <- DB_2 %>%  dplyr::mutate(Cambios_CD = case_when( 
  Cambios_C == "A" & Cambios_D == "A" ~ "A",
  Cambios_C == "B" & Cambios_D == "B" ~ "B",
  Cambios_C == "C" & Cambios_D == "C" ~ "C",
  Cambios_C == "A" & Cambios_D == "B" ~ "C",
  Cambios_C == "B" & Cambios_D == "A" ~ "C",
  is.na(Cambios_C) & Cambios_D == "B"  ~ "B",
  Cambios_C == "B"  & is.na(Cambios_D)  ~ "B",
  is.na(Cambios_C) & Cambios_D == "A"  ~ "A",
  Cambios_C == "A"  & is.na(Cambios_D)  ~ "A",
  TRUE ~ "C"
))

table(DB_2$Cambios_CD)
summarytools::freq(DB_2$Cambios_CD)

DB_2 <- DB_2 %>%  dplyr::mutate(Cambios_ABC = case_when( 
  Cambios_C == "A" & Cambios_AB == "A" ~ "A",
  Cambios_C == "B" & Cambios_AB == "B" ~ "B",
  Cambios_C == "C" & Cambios_AB == "C" ~ "C",
  Cambios_C == "A" & Cambios_AB == "B" ~ "C",
  Cambios_C == "B" & Cambios_AB == "A" ~ "C",
  TRUE ~ "C"
))
summarytools::freq(DB_2$Cambios_ABC)

DB_2 <- DB_2 %>%  dplyr::mutate(Cambios_ABCD = case_when( 
  Cambios_CD == "A" & Cambios_AB == "A" ~ "A",
  Cambios_CD == "B" & Cambios_AB == "B" ~ "B",
  Cambios_CD == "C" & Cambios_AB == "C" ~ "C",
  Cambios_CD == "A" & Cambios_AB == "B" ~ "C",
  Cambios_CD == "B" & Cambios_AB == "A" ~ "C",
  TRUE ~ "C"
))
#he quitado las líneas sobre is.na porque ya no hay NA en las variables AB y CD

table(DB_2$Cambios_ABCD)
summarytools::freq(DB_2$Cambios_ABCD)

## CONSTRUIR LAS VARIABLES DE CAMBIO DE GENTRIFICACIÓN Y TURISTIFICACIÓN
# Restamos los valores pre a los post para ver la diferencia 

# GENTRIFICACIÓN

DB_1 <- DB_1 %>% dplyr::mutate(Dif_NCGS = (Ahora_NCGS - Antes_NCGS))
head(DB_1[,c("Dif_NCGS", "Antes_NCGS", "Ahora_NCGS")])
       
DB_1$Dif_NCGS_2 <-cut(DB_1$Dif_NCGS, breaks = c(-10, -0.0000001, 0, 10), labels = c("Reduced", "No change", "Increased")) 
summarytools::freq(DB_1$Dif_NCGS_2)

# TURISTIFICACIÓN
summary(DB_1$Ahora_turismo)
DB_1 <- DB_1 %>% dplyr::mutate(Dif_Turismo = (Ahora_turismo - Antes_turismo))
head(DB_1[,c("Dif_Turismo", "Ahora_turismo", "Antes_turismo")])

DB_1$Dif_Turismo_2 <-cut(DB_1$Dif_Turismo, breaks = c(-10, -0.0000001, 0, 10), labels = c("Reduced", "No change", "Increased")) 
summarytools::freq(DB_1$Dif_Turismo_2)

# QUITAR LAS ENCUESTAS SIN DATOS DE SALUD

DB_1 <- DB_1[c(-161,-162),]

 DB_2 <- DB_1
 DB_1$Ahora_salud2 <- as.factor(DB_1$Ahora_salud2)

DB_FPBCN <- DB_1

# recategorizar las variables de diferencia en gentri y turis para que sean sólo dos categorías (disminución + aumento/no cambio)
# gentrificación
DB_1$Dif_NCGS_1 <-cut(DB_1$Dif_NCGS, breaks = c(-10, 0, 10), labels = c("Reduced", "No change / Increased")) 
summarytools::freq(DB_1$Dif_NCGS_1)
# turistificación
DB_1$Dif_Turismo_1 <-cut(DB_1$Dif_Turismo, breaks = c(-10, 0, 10), labels = c("Reduced", "No change / Increased")) 
summarytools::freq(DB_1$Dif_Turismo_1)

DB_1<-DB_FPBCN
#INVERTIR LA VARIABLE DE CALIDAD DEL SUEÑO
DB_1 <- DB_1 %>% dplyr::mutate(Antes_dormirInv  = (10 - Antes_dormir))
head(DB_1[,c("Antes_dormirInv", "Antes_dormir")])
DB_1 <- DB_1 %>% dplyr::mutate(Ahora_dormirInv  = (10 - Ahora_dormir))
head(DB_1[,c("Ahora_dormirInv", "Ahora_dormir")])

DB_FPBCN <- DB_1

# DIVIDIMOS LA BASE POR BARRIOS
DB_Barceloneta <- subset(DB_FPBCN, Barrio == "Barceloneta")
DB_SA <- subset(DB_FPBCN, Barrio == "Sant Antoni")
# Guardamos la base antes de eliminar las variables
#aquí tendras que cambiar "dbSpanishclone_English" por el nombre de la db final que crees
save(DB_FPBCN, file= "DB_FEMPUBLIC.RData") 
save(DB_Barceloneta, file= "DB_Barceloneta.RData")
save(DB_SA, file= "DB_SA.RData")
