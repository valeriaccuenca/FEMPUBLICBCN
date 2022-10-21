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
####*CAMBIO DE USO DEL ESPACIO PÚBLICO Y LAS _AHORA_

##*** Entrevistador
##*Cambios visitas A, B, C, D
##*Cambios cambios tiempo A, B, C, D
##*Cambios actividades (?)
##*Servicios, echen, desalojen, delincuencia, ricos, redes, comunidad, fuera, renovación, no bienvenido
##*Turistas, hoteles
##*Cuidados
##*Salud
##*Concentrarse, sueño, papel, decisiones, tensión, superar, disfrutar, problemas, deprimida, confianza, valer, feliz
##*Dormir
##*Género
##*Nacimiento
##*Estudios
##*Monetaria
##*Laboral


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


#si al correr esto te salgo algo FALSE es que te has olvidado de homogeneizar algun nombre de variable
## HECHO

### PRIMERO UNIMOS LAS BASES SPANISH Y ENGLISH
dbSpanish_English <- rbind(dbSpanish, dbEnglish)
head(dbSpanish_English)

### SEGUNDO UNIMOS SPANGLISH Y CAT
names(dbSpanish_English) == names(dbCatalan)
dbSpanish_English_Catalan <- rbind(dbSpanish_English, dbCatalan)
head(dbSpanish_English_Catalan)

### POR ÚLTIMO, UNIMOS CATSPANGLISH CON SPANISH CLONE
names(dbSpanish_English_Catalan) == names(dbSpanishClone)
DB_1 <- rbind(dbSpanish_English_Catalan, dbSpanishClone)
head(DB_1)

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

#Guardar base 1 por si la cagamos en los siguientes pasos
DB1_1<- DB_1

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

DB1_2<- DB_1
DB_1 <- DB1_2

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

DB1_3 <- DB_1

## CUIDADOS AHORA

table(DB_1$Ahora_cuidado)
# Sí
DB_1$Ahora_cuidado[DB_1$Ahora_cuidado=="si"]<- "Si"
# No
DB_1$Ahora_cuidado[DB_1$Ahora_cuidado=="no"]<- "No"
# No aplica
DB_1$Ahora_cuidado[DB_1$Ahora_cuidado=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_cuidado[DB_1$Ahora_cuidado=="no_contesta___no_aplica"]<- "NA"

DB1_4 <- DB_1

## SALUD GENERAL
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
DB_1$Ahora_salud[DB_1$Ahora_salud=="No contesta / No aplica"]<- "NA"
DB_1$Ahora_salud[DB_1$Ahora_salud=="no_contesta___no_aplica"]<- "NA"

DB1_5 <- DB_1
DB_1 <- DB1_5
## SALUD MENTAL

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

## CHECK
table(DB_1$Ahora_concentrarse)
table(DB_1$Ahora_sueno)
table(DB_1$Ahora_papel)
table(DB_1$Ahora_decisiones)
table(DB_1$Ahora_tension)
table(DB_1$Ahora_superar)
table(DB_1$Ahora_disfrutar)
table(DB_1$Ahora_problemas)
table(DB_1$Ahora_deprimida)
table(DB_1$Ahora_confianza)
table(DB_1$Ahora_valer)
table(DB_1$Ahora_feliz)

DB1_6<-DB_1


## CALIDAD DEL SUEÑO
table(DB_1$Ahora_dormir)
DB_1$Ahora_dormir[DB_1$Ahora_dormir=="don_t_know___no_answer"]<- NA
DB_1$Ahora_dormir[DB_1$Ahora_dormir=="No sabe / No contesta"]<- NA
DB_1$Ahora_dormir[DB_1$Ahora_dormir=="No sap / No contesta"]<- NA

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
DB_1$Nacimiento[DB_1$Nacimiento=="italia"]<- "Italia"
DB_1$Nacimiento[DB_1$Nacimiento=="Italia España"]<- "España"
DB_1$Nacimiento[DB_1$Nacimiento=="Los Estados Unidos"]<- "EE.UU."
DB_1$Nacimiento[DB_1$Nacimiento=="los_estados_unidos"]<- "EE.UU."
DB_1$Nacimiento[DB_1$Nacimiento=="marruecos"]<- "Marruecos"
DB_1$Nacimiento[DB_1$Nacimiento=="mexico"]<- "México"
DB_1$Nacimiento[DB_1$Nacimiento=="Mexico"]<- "México"
DB_1$Nacimiento[DB_1$Nacimiento=="No contesta / No aplica"]<- "NA"
DB_1$Nacimiento[DB_1$Nacimiento=="pakist_n"]<- "Pakistán"
DB_1$Nacimiento[DB_1$Nacimiento=="Reino Unido España"]<- "España"
DB_1$Nacimiento[DB_1$Nacimiento=="reino_unido"]<- "Reino Unido"
DB_1$Nacimiento[DB_1$Nacimiento=="rep_dominicana"]<- "República Dominicana"
DB_1$Nacimiento[DB_1$Nacimiento=="Republica Dominicana"]<- "República Dominicana"
DB_1$Nacimiento[DB_1$Nacimiento=="Republica Checa"]<- "República Checa"
DB_1$Nacimiento[DB_1$Nacimiento=="Índia"]<- "India"
DB_1$Nacimiento[DB_1$Nacimiento=="Russia"]<- "Rusia"
DB_1$Nacimiento[DB_1$Nacimiento=="Ucraina"]<- "Ucrania"

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

DB1_9<-DB_1

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
DB_1$Estudios[DB_1$Estudios=="Primaris complets: primària LOGSE completa o cinc cursos aprovats d'EGB."]<- "Primarios"
DB_1$Estudios[DB_1$Estudios=="Primarios completos: primaria LOGSE completa o cinco cursos aprobados d'EGB."]<- "Primarios"
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

DB1_10 <- DB_1

##### SITUACIÓN LABORAL
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
DB_1$Ahora_laboral[DB_1$Ahora_laboral=="NA"]<- NA

DB_Barceloneta$Ahora_laboral[DB_Barceloneta$Ahora_laboral=="NA"]<- NA
DB_SA$Ahora_laboral[DB_SA$Ahora_laboral=="NA"]<- NA
## CHECK
table(DB_1$Ahora_laboral)

DB1_11 <- DB_1

## SITUACIÓN ECONÓMICA
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

## CHECK
table(DB_1$Ahora_monetaria)

#INTERESADA EN HACER LA PARTE CUALITATIVA
table(DB_1$Interesada)
# Sí
DB_1$Interesada[DB_1$Interesada=="si"]<- "Si"
# No
DB_1$Interesada[DB_1$Interesada=="no"]<- "No"


DB_1_1 <- DB_1

####******************************########
####2. Chequeamos que las variables sean del tipo que queremos  ####
####******************************#######
summary(DB_1) 
#vemos que edad es character pero debería ser numérica

#cambiamos el tipo de variable de Edad:
DB_1$Edad_2 <- as.numeric(DB_1$Edad)
summary(DB_1)
DB_1$Edad <- NULL

# Guardamos la base antes de eliminar las variables
#aquí tendras que cambiar "dbSpanishclone_English" por el nombre de la db final que crees
save(DB_1, file= "DB_PreFinal.RData") 

# eliminamos el resto de variables que no vamos a utilizar
DB_1$start<- NULL
DB_1$Cambios_picnic<- NULL
DB_1$Cambios_pasear<- NULL
DB_1$Cambios_tranquilidad<- NULL
DB_1$Cambios_reunirse<- NULL
DB_1$Cambios_dependientes<- NULL
DB_1$Cambios_jugar<- NULL
DB_1$Cambios_estudiar<- NULL
DB_1$Actividad_especificar<- NULL
DB_1$Cambios_especificar<- NULL
DB_1$Antes_cuidado<- NULL
DB_1$Antes_salud<- NULL
DB_1$Antes_concentrarse<- NULL
DB_1$Antes_sueno<- NULL
DB_1$Antes_papel<- NULL
DB_1$Antes_decisiones<- NULL
DB_1$Antes_tension<- NULL
DB_1$Antes_superar<- NULL
DB_1$Antes_disfrutar<- NULL
DB_1$Antes_problemas<- NULL
DB_1$Antes_deprimida<- NULL
DB_1$Antes_confianza<- NULL
DB_1$Antes_valer<- NULL
DB_1$Antes_feliz<- NULL
DB_1$Antes_dormir<- NULL
DB_1$Genero_otro<- NULL
DB_1$Nacimiento_otro<- NULL
DB_1$Estudios_otro<- NULL
DB_1$Antes_monetaria<- NULL
DB_1$Antes_laboral<- NULL
DB_1$Estudios_otro<- NULL

## CHECK DE LAS VARIABLES QUE QUEDAN


#cambiamos el tipo de variable de Edad:
DB_1$Tiempo_barrio <- as.numeric(DB_1$Barrio_Tiempo)

DB_1$Barrio_Tiempo <- NULL
summary(DB_1)

#eliminar las encuestas sin datos de salud

DB1_1 <- DB_1[c(-161,-162),]
DB_Pre <- DB1_1

####******************************########
####3. Guardamos el dataframe limpio/base de datos R limpia  ####
####******************************#######
setwd(outputs)
save(DB_Pre, file= "DB_PreFinal.RData") #cambiar por el nombre de la db final que creas

DB_1 <- DB_Pre
#########################################################
########################################################
## 4. CONVERTIMOS LAS VARIABLES DE SALUD MENTAL Y GENTRIFICACIÓN EN NUMÉRICAS ##

#### CONVERTIMOS LAS VARIABLES DE SALUD MENTAL ####

# Primero transformamos respuestas según cada pregunta
# si la pregunta es positiva, 0 y 1 (siempre, casi siempre) son un 0; 2 y 3 (muy poco, nunca) son un 1
# si la preugnta es negativa, 0 y 1 (siempre, casi siempre) son un 1; 2 y 3 (muy poco, nunca) son un 0

 #* concentrarse = positivo 
table(DB_1$Ahora_concentrarse)
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="1"]<- "0"
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="2"]<- "1"
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="3"]<- "1"
DB_1$Ahora_concentrarse[DB_1$Ahora_concentrarse=="NA"]<- NA
 #* sueño = negativo
table(DB_1$Ahora_sueno)
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="0"]<- "1"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="2"]<- "0"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="3"]<- "0"
DB_1$Ahora_sueno[DB_1$Ahora_sueno=="NA"]<- NA
 #* papel = positivo
table(DB_1$Ahora_papel)
DB_1$Ahora_papel[DB_1$Ahora_papel=="1"]<- "0"
DB_1$Ahora_papel[DB_1$Ahora_papel=="2"]<- "1"
DB_1$Ahora_papel[DB_1$Ahora_papel=="3"]<- "1"
DB_1$Ahora_papel[DB_1$Ahora_papel=="NA"]<- NA
 #* decisiones = positivo
table(DB_1$Ahora_decisiones)
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="1"]<- "0"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="2"]<- "1"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="3"]<- "1"
DB_1$Ahora_decisiones[DB_1$Ahora_decisiones=="NA"]<- NA
 #* tension = negativo
table(DB_1$Ahora_tension)
DB_1$Ahora_tension[DB_1$Ahora_tension=="0"]<- "1"
DB_1$Ahora_tension[DB_1$Ahora_tension=="2"]<- "0"
DB_1$Ahora_tension[DB_1$Ahora_tension=="3"]<- "0"
DB_1$Ahora_tension[DB_1$Ahora_tension=="NA"]<- NA
 #* superar = negativo
table(DB_1$Ahora_superar)
DB_1$Ahora_superar[DB_1$Ahora_superar=="0"]<- "1"
DB_1$Ahora_superar[DB_1$Ahora_superar=="2"]<- "0"
DB_1$Ahora_superar[DB_1$Ahora_superar=="3"]<- "0"
DB_1$Ahora_superar[DB_1$Ahora_superar=="NA"]<- NA
 #* disfrutar = positivo
table(DB_1$Ahora_disfrutar)
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="1"]<- "0"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="2"]<- "1"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="3"]<- "1"
DB_1$Ahora_disfrutar[DB_1$Ahora_disfrutar=="NA"]<- NA
 #* problemas = positivo
table(DB_1$Ahora_problemas)
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="1"]<- "0"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="2"]<- "1"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="3"]<- "1"
DB_1$Ahora_problemas[DB_1$Ahora_problemas=="NA"]<- NA
 #* deprimida = negativo
table(DB_1$Ahora_deprimida)
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="0"]<- "1"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="2"]<- "0"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="3"]<- "0"
DB_1$Ahora_deprimida[DB_1$Ahora_deprimida=="NA"]<- NA
 #* confianza = negativo
table(DB_1$Ahora_confianza)
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="0"]<- "1"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="2"]<- "0"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="3"]<- "0"
DB_1$Ahora_confianza[DB_1$Ahora_confianza=="NA"]<- NA
 #* valer = negativo
table(DB_1$Ahora_valer)
DB_1$Ahora_valer[DB_1$Ahora_valer=="0"]<- "1"
DB_1$Ahora_valer[DB_1$Ahora_valer=="2"]<- "0"
DB_1$Ahora_valer[DB_1$Ahora_valer=="3"]<- "0"
DB_1$Ahora_valer[DB_1$Ahora_valer=="NA"]<- NA
 #* feliz = positivo
table(DB_1$Ahora_feliz)
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="1"]<- "0"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="2"]<- "1"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="3"]<- "1"
DB_1$Ahora_feliz[DB_1$Ahora_feliz=="NA"]<- NA

# Después, las convertimos en numéricas
 #concentrarse
DB_1$Concentrarse <- as.numeric(DB_1$Ahora_concentrarse)
summary(DB_1)
DB_1$Ahora_concentrarse <- NULL
 #papel
DB_1$Papel <- as.numeric(DB_1$Ahora_papel)
summary(DB_1)
DB_1$Ahora_papel <- NULL
 #*deciosnes
DB_1$Decisiones <- as.numeric(DB_1$Ahora_decisiones)
summary(DB_1)
DB_1$Ahora_decisiones <- NULL
 #*disfrutar
DB_1$Disfrutar <- as.numeric(DB_1$Ahora_disfrutar)
summary(DB_1)
DB_1$Ahora_disfrutar <- NULL
 #*problemas
DB_1$Problemas <- as.numeric(DB_1$Ahora_problemas)
summary(DB_1)
DB_1$Ahora_problemas <- NULL
 #*feliz
DB_1$Feliz <- as.numeric(DB_1$Ahora_feliz)
summary(DB_1)
DB_1$Ahora_feliz <- NULL
 #*sueño
DB_1$Sueno <- as.numeric(DB_1$Ahora_sueno)
summary(DB_1)
DB_1$Ahora_sueno <- NULL
 #*tensión
DB_1$Tension <- as.numeric(DB_1$Ahora_tension)
summary(DB_1)
DB_1$Ahora_tension <- NULL
 #*superar
DB_1$Superar <- as.numeric(DB_1$Ahora_superar)
summary(DB_1)
DB_1$Ahora_superar <- NULL
 #*deprimida
DB_1$Deprimida <- as.numeric(DB_1$Ahora_deprimida)
summary(DB_1)
DB_1$Ahora_deprimida <- NULL
 #*confianza
DB_1$Confianza <- as.numeric(DB_1$Ahora_confianza)
summary(DB_1)
DB_1$Ahora_confianza <- NULL
 #*valer
DB_1$Valer <- as.numeric(DB_1$Ahora_valer)
summary(DB_1)
DB_1$Ahora_valer <- NULL

 ## comprobamos
summary(DB_1)

DB_1_1 <- DB_1
# En tercer lugar, sumamos todas en una única variable
library(dbplyr)
DB_1 <- DB_1 %>% dplyr::mutate(GHQ = (Concentrarse + Papel + Decisiones + Disfrutar + Problemas + Feliz + Sueno + Tension + Superar + Deprimida + Confianza + Valer))
head(DB_1[,c("Concentrarse","Papel", "Decisiones", "Disfrutar", "Problemas", "Feliz", "Sueno", "Tension", "Superar", "Deprimida", "Confianza", "Valer")]) #we do this to check that it has worked well
describe(DB_1$GHQ)
boxplot(DB_1$GHQ)

# elimino otra encuesta sin datos de salud mental ni uso del espacio público

DB_1 <- DB_1[-647,]

DB_1_1 <- DB_1


# Por último, categorizamos en buena salud mental (0, 1 y 2) y mala salud mental (3 a 12)
table(DB_1$GHQ)
DB_1$GHQ_2 <- cut(DB_1$GHQ, breaks = c(0, 2, 12), include.lowest = TRUE, labels = c("0", "1")) 
describe(DB_1$GHQ_2)
table(DB_1$GHQ_2)
 ## comprobamos
 summary(DB_1)
 
 
 DB_2 <- DB_1

#### CONVERTIMOS LAS VARIABLES DE GENTRIFICACIÓN ####

#* Según el paper, las preguntas dan lugar a dos factores (Disruption y Gentrification). 
#* El primero estaría compuesto por las variables de echen, redes, desalojen, fuera, nobienvenido y comunidad
#* El segundo sería de servicios, ricos, delincuencia y renovación

#* Primero transformamos todas las preguntas en numéricas
#* echen
DB_1$Echen <- as.numeric(DB_1$Ahora_echen)
DB_1$Ahora_echen <- NULL
#* redes
DB_1$Redes <- as.numeric(DB_1$Ahora_redes)
DB_1$Ahora_redes <- NULL
#* desalojen
DB_1$Desalojen <- as.numeric(DB_1$Ahora_desalojen)
DB_1$Ahora_desalojen <- NULL
#* fuera
DB_1$Fuera <- as.numeric(DB_1$Ahora_fuera)
DB_1$Ahora_fuera <- NULL
#* no bienvenida
DB_1$NoBienvenida <- as.numeric(DB_1$Ahora_nobienvenido)
DB_1$Ahora_nobienvenido <- NULL
#* comunidad
DB_1$Comunidad <- as.numeric(DB_1$Ahora_comunidad)
DB_1$Ahora_comunidad <- NULL
#* servicios
DB_1$Servicios <- as.numeric(DB_1$Ahora_servicios)
DB_1$Ahora_servicios <- NULL
#* delincuencia
DB_1$Delincuencia <- as.numeric(DB_1$Ahora_delincuencia)
DB_1$Ahora_delincuencia <- NULL
#* ricos
DB_1$Ricos <- as.numeric(DB_1$Ahora_ricos)
DB_1$Ahora_ricos <- NULL
#* renovación
DB_1$Renovacion <- as.numeric(DB_1$Ahora_renovacion)
DB_1$Ahora_renovacion <- NULL
 
# comprobamos
summary(DB_1)
 
#* En segundo lugar, obtenemos la variable de Disruption (ND) como media de las 6 antes mencionadas

DB_1 <- DB_1 %>% dplyr::mutate(ND = ((Echen + Redes + Desalojen + Fuera + NoBienvenida + Comunidad)/6))
head(DB_1[,c("ND", "Echen","Redes", "Desalojen", "Fuera", "NoBienvenida", "Comunidad")]) #we do this to check that it has worked well

#* En tercer lugar, obtenemos la variable de Gentrification (PG) como media de las otras 4 variables

DB_1 <- DB_1 %>% dplyr::mutate(PG = ((Servicios + Ricos + Delincuencia + Renovacion)/4))
head(DB_1[,c("PG", "Servicios", "Ricos", "Delincuencia", "Renovacion")])


#* Por último, sumamos ambas medias para obtener la variable de NCGS
DB_1 <- DB_1 %>% dplyr::mutate(NCGS = (ND + PG))
head(DB_1[,c("PG", "ND", "NCGS")])
 
library(summarytools)
summarytools::descr(DB_1$NCGS)
summarytools::descr(DB_1$ND)
summarytools::descr(DB_1$PG)
boxplot(DB_1$NCGS)
boxplot(DB_1$ND)
boxplot(DB_1$PG)

ggplot(DB_1) + geom_qq(aes(sample = NCGS)) 
ggplot(DB_1) + geom_qq(aes(sample = ND)) 
ggplot(DB_1) + geom_qq(aes(sample = PG)) 


DB_2 <- DB_1

save(DB_1, file= "DB_Final.RData") #cambiar por el nombre de la db final que creas

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
summarytools::freq(DB_1$Nacimiento_2)


### TRANSFORMAR LA VARIABLE DE ESTUDIOS EN UNIVERSITARIOS Y NO UNIVERSITARIOS
describe(DB_1$Estudios)
summarytools::freq(DB_1$Estudios)
table(DB_1$Estudios)

DB_1$Estudios2[DB_1$Estudios=="Universitarios PG"] <- 0
DB_1$Estudios2[DB_1$Estudios=="Universitarios SC"] <- 0
DB_1$Estudios2[DB_1$Estudios=="Universitarios PC"] <- 0

DB_1$Estudios2[DB_1$Estudios=="Bachillerato"] <- 1
DB_1$Estudios2[DB_1$Estudios=="FP superior"] <- 1
DB_1$Estudios2[DB_1$Estudios=="FP medio"] <- 1
DB_1$Estudios2[DB_1$Estudios=="Secundarios"] <- 1
DB_1$Estudios2[DB_1$Estudios=="Primarios"] <- 1
DB_1$Estudios2[DB_1$Estudios=="Primarios incompletos"] <- 1
DB_1$Estudios2[DB_1$Estudios=="Ninguno"] <- 1

summarytools::freq(DB_1$Estudios2)
table(DB_1$Estudios2)
# para volver a tenerla como character para los modelos
DB_1$Estudios_2 <- as.character(DB_1$Estudios2)

### TRANSFORMAR LA VARIABLE DE SITUACIÓN ECONÓMICA EN BINARIA
describe(DB_1$Ahora_monetaria)
summarytools::freq(DB_1$Ahora_monetaria)
table(DB_1$Ahora_monetaria)

DB_2$Ahora_monetaria[DB_2$Ahora_monetaria=="NA"] <- NA
DB_Barceloneta$Ahora_monetaria[DB_Barceloneta$Ahora_monetaria=="NA"] <- NA
DB_SA$Ahora_monetaria[DB_SA$Ahora_monetaria=="NA"] <- NA

DB_1$Monetaria[DB_1$Ahora_monetaria=="Cómodamente"] <- "0"
DB_1$Monetaria[DB_1$Ahora_monetaria=="No llega"] <- "1"
DB_1$Monetaria[DB_1$Ahora_monetaria=="Lo justo"] <- "1"

summarytools::freq(DB_1$Monetaria)

table(DB_1$Monetaria)

summary.data.frame(DB_1)
describe(DB_1) #sólo para ver la distribución de variables numéricas continuas

### TRANSORMAR VARIABLE DE SITUACIÓN LABORAL EN BINARIA

summarytools::freq(DB_1$Ahora_laboral)
table(DB_1$Ahora_laboral)


# Dividir la población según si el trabajo es estable (completo o parcial) y no tenerlo

DB_1$Laboral[DB_1$Ahora_laboral=="Trabajo completo"] <- 0
DB_1$Laboral[DB_1$Ahora_laboral=="Trabajo parcial"] <- 0

DB_1$Laboral[DB_1$Ahora_laboral=="Trabajo casual"] <- 1
DB_1$Laboral[DB_1$Ahora_laboral=="Desempleada"] <- 1
DB_1$Laboral[DB_1$Ahora_laboral=="Cuidando"] <- 1
DB_1$Laboral[DB_1$Ahora_laboral=="Estudiante"] <- 1
DB_1$Laboral[DB_1$Ahora_laboral=="Incapaz por salud"] <- 1
DB_1$Laboral[DB_1$Ahora_laboral=="Jubilada"] <- 1

table(DB_1$Laboral)
describe(DB_1$Laboral)
summarytools::freq(DB_1$Laboral)


### CAMBIAR LAS VARIABLES DE TURISMO Y SUMARLAS (igual a las de gentrificación)

DB_1$Turistas <- as.numeric(DB_1$Ahora_turistas)
DB_1$Ahora_turistas <- NULL
DB_1$Hoteles <- as.numeric(DB_1$Ahora_hoteles)
DB_1$Ahora_hoteles <- NULL

DB_1 <- DB_1 %>% dplyr::mutate(Turismo = (Turistas + Hoteles))

describe(DB_1$Turismo, IQR = TRUE)
summary(DB_1$Turismo)
boxplot(DB_1$Turismo)

ggplot(DB_2) + geom_qq(aes(sample = Turismo)) 

DB_1$Turismo_2 <-cut(DB_1$Turismo, breaks = c(0, 5, 10), labels = c("0", "1")) 
summarytools::freq(DB_1$Turismo_2)

############################################################################
###############################################################################
######## CAMBIAR LAS VARIABLES DE USO DEL ESPACIO PÚBLICO EN BINARIAS ######
############################################################################
##########################################################################

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

#### CREAR VARIABLES AGREGADAS DEL USO DEL ESPACIO PÚBLICO

# DB_1 <- DB_1 %>% dplyr::mutate(Cambios_A = (Tiempo_A + Visitas_A))

# table(DB_1$Cambios_A)
# describe(DB_1$Cambios_A)
# boxplot(DB_1$Cambios_A)
# histogram(DB_1$Cambios_A)
DB_1$Cambios_A <- NULL

# DB_1 <- DB_1 %>% dplyr::mutate(Cambios_B = (Tiempo_B + Visitas_B))

# table(DB_1$Cambios_B)
# describe(DB_1$Cambios_B)
# boxplot(DB_1$Cambios_B)
# histogram(DB_1$Cambios_B)
DB_1$Cambios_B <- NULL

# DB_1 <- DB_1 %>% dplyr::mutate(Cambios_C = (Tiempo_C + Visitas_C))

# table(DB_1$Cambios_C)
# describe(DB_1$Cambios_C)
# boxplot(DB_1$Cambios_C)
# histogram(DB_1$Cambios_C)
DB_1$Cambios_C <- NULL

# DB_1 <- DB_1 %>% dplyr::mutate(Cambios_D = (Tiempo_D + Visitas_D))

# table(DB_1$Cambios_D)
# describe(DB_1$Cambios_D)
# boxplot(DB_1$Cambios_D)
# histogram(DB_1$Cambios_D)
DB_1$Cambios_D <- NULL

# DB_1 <- DB_1 %>% dplyr::mutate(Cambios_AB = (Cambios_A + Cambios_B))

# table(DB_1$Cambios_AB)
# describe(DB_1$Cambios_AB)
# boxplot(DB_1$Cambios_AB)
# histogram(DB_1$Cambios_AB)
DB_1$Cambios_AB <- NULL

# DB_1 <- DB_1 %>% dplyr::mutate(Cambios_CD = (Cambios_C + Cambios_D))

# table(DB_1$Cambios_CD)
# describe(DB_1$Cambios_CD)
# boxplot(DB_1$Cambios_CD)
# histogram(DB_1$Cambios_CD)
DB_1$Cambios_CD <- NULL

# DB_1 <- DB_1 %>% dplyr::mutate(Cambios_ABCD = (Cambios_AB + Cambios_CD))
# table(DB_1$Cambios_ABCD)
# summary(DB_1$Cambios_ABCD)
# describe(DB_1$Cambios_ABCD)
# boxplot(DB_1$Cambios_ABCD)
# histogram(DB_1$Cambios_ABCD)
DB_1$Cambios_ABCD <- NULL


### CONVERTIR LA VARIABLE DE SALUD GENERAL EN BINARIA ###
# Los resultados de buena, muy buena y excelente se consideran buena salud (=0)
# Los resultados de regular o mala, se consideran mala salud (=1)
table(DB_1$Ahora_salud)

DB_1$SaludG[DB_1$Ahora_salud == "Buena"] <- "0"
DB_1$SaludG[DB_1$Ahora_salud == "Muy buena"] <- "0"
DB_1$SaludG[DB_1$Ahora_salud == "Excelente"] <- "0"

DB_1$SaludG[DB_1$Ahora_salud == "Regular"] <- "1"
DB_1$SaludG[DB_1$Ahora_salud == "Mala"] <- "1"

table(DB_1$SaludG)
summarytools::freq(DB_1$SaludG)

### CALIDAD DEL SUEÑO EN DOS CATEGORÍAS ###
table(DB_1$Ahora_dormir)
DB_1$Dormir <- as.numeric(DB_1$Ahora_dormir)
table(DB_1$Dormir)
summary(DB_1$Dormir)
summarytools::descr(DB_1$Dormir)
describe(DB_1$Dormir)
boxplot(DB_1$Dormir)
histogram(DB_1$Dormir)
ggplot(DB_1) + geom_qq(aes(sample = Dormir)) 

DB_1$Ahora_dormir <-NULL

DB_1$Dormir_2 <-cut(DB_1$Dormir, breaks = c(0, 5, 10), include.lowest = TRUE, labels = c("1", "0")) 
table(DB_1$Dormir_2)


# CONVERTIR TAMBIÉN LA VARIABLE DE CUIDADOS
table(DB_1$Ahora_cuidado)
summarytools::freq(DB_1$Ahora_cuidado)
DB_1$Cuidado[DB_1$Ahora_cuidado == "Si"] <- "1"
DB_1$Cuidado[DB_1$Ahora_cuidado == "No"] <- "0"

table(DB_1$Cuidado)
summarytools::freq(DB_1$Cuidado)
DB_1$Ahora_cuidado <- NULL


save(DB_1, file= "DB_Final.RData")

