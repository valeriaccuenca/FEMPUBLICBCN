##### TABLAS DE CONTINGENCIA #####
# Cargar package gmodels para obtener las tablas de contingencia
library(gmodels)
######### VARIABLES CONTINUAS ################
# Cargar package psych para realizar la descriptiva de las variables continuas 
#  para cada categoría de la variable categórica y package car para test de levene
library(psych)
library(car)

# NACIMIENTO
CrossTable(DB_1$Nacimiento_2,DB_1$GHQ_2, expected = TRUE)
# no diferencia signifcativa

CrossTable(DB_1$Nacimiento_2,DB_1$SaludG, expected = TRUE)
# no diferencia signifcativa

by(DB_1$Dormir,DB_1$Nacimiento_2,summary)
psych::describeBy(DB_1$Dormir, DB_1$Nacimiento_2)
boxplot(DB_1$Dormir ~ DB_1$Nacimiento_2)
wilcox.test(Dormir ~ Nacimiento_2, data = DB_1)
# no diferencia signifcativa

# SITUACIÓN LABORAL
CrossTable(DB_1$Ahora_laboral,DB_1$GHQ_2, expected = TRUE)
TabAhLaboral_GHQ <- table(DB_1$Ahora_laboral,DB_1$GHQ_2)
fisher.test(TabAhLaboral_GHQ,simulate.p.value=TRUE)

CrossTable(DB_1$Laboral,DB_1$GHQ_2, expected = TRUE)
# diferencia signifcativa

CrossTable(DB_1$Laboral,DB_1$SaludG, expected = TRUE)
CrossTable(DB_1$Ahora_laboral,DB_1$SaludG, expected = TRUE)
TabAhLaboral_GenH <- table(DB_1$Ahora_laboral,DB_1$SaludG)
fisher.test(TabAhLaboral_GenH,simulate.p.value=TRUE)
# diferencia signifcativa

by(DB_1$Dormir,DB_1$Laboral,summary)
psych::describeBy(DB_1$Dormir, DB_1$Laboral)
boxplot(DB_1$Dormir ~ DB_1$Laboral)
wilcox.test(Dormir ~ Laboral, data = DB_1)
# no diferencia signifcativa

by(DB_1$Dormir,DB_1$Ahora_laboral,summary)
psych::describeBy(DB_1$Dormir, DB_1$Ahora_laboral)
boxplot(DB_1$Dormir ~ DB_1$Ahora_laboral)
library(nnet)
SleepQ_Laboral <-multinom(Dormir ~ Ahora_laboral, data = DB_1)
Anova(SleepQ_Laboral, data = DB_1)
# no diferencia signifcativa

#SITUACIÓN MONETARIA
CrossTable(DB_1$Monetaria,DB_1$GHQ_2, expected = TRUE)
# diferencia signifcativa

CrossTable(DB_1$Monetaria,DB_1$SaludG, expected = TRUE)
# diferencia signifcativa

by(DB_1$Dormir,DB_1$Monetaria,summary)
psych::describeBy(DB_1$Dormir, DB_1$Monetaria)
boxplot(DB_1$Dormir ~ DB_1$Monetaria)
wilcox.test(Dormir ~ Monetaria, data = DB_1)
# diferencia significativa

#CUIDADOS
CrossTable(DB_1$Cuidado,DB_1$GHQ_2, expected = TRUE)
# no diferencia signifcativa

CrossTable(DB_1$Cuidado,DB_1$SaludG, expected = TRUE)
# no diferencia signifcativa

by(DB_1$Dormir,DB_1$Cuidado,summary)
psych::describeBy(DB_1$Dormir, DB_1$Cuidado)
boxplot(DB_1$Dormir ~ DB_1$Cuidado)
wilcox.test(Dormir ~ Cuidado, data = DB_1)
# no significativa

#BARRIO
CrossTable(DB_1$Barrio,DB_1$GHQ_2, expected = TRUE)
# no diferencia signifcativa

CrossTable(DB_1$Barrio,DB_1$SaludG, expected = TRUE)
# diferencia signifcativa

by(DB_1$Dormir,DB_1$Barrio,summary)
psych::describeBy(DB_1$Dormir, DB_1$Barrio)
boxplot(DB_1$Dormir ~ DB_1$Barrio)
wilcox.test(Dormir ~ Barrio, data = DB_1)
# diferencia significativa

#ESTUDIOS
CrossTable(DB_1$Estudios_2,DB_1$GHQ_2, expected = TRUE)
# no diferencia signifcativa

CrossTable(DB_1$Estudios_2,DB_1$SaludG, expected = TRUE)
# diferencia signifcativa

by(DB_1$Dormir,DB_1$Estudios_2,summary)
psych::describeBy(DB_1$Dormir, DB_1$Estudios_2)
boxplot(DB_1$Dormir ~ DB_1$Estudios_2)
wilcox.test(Dormir ~ Estudios_2, data = DB_1)
# no diferencia significativa

# EDAD
by(DB_1$Edad_2,DB_1$GHQ_2, summary)
psych::describeBy(DB_1$Edad_2, DB_1$GHQ_2)
boxplot(DB_1$Edad_2 ~ DB_1$GHQ_2)
leveneTest(Edad_2 ~ GHQ_2, data = DB_1)
t.test(Edad_2 ~ GHQ_2, data = DB_1, var.equal = TRUE)
# diferencia signifcativa

by(DB_1$Edad_2,DB_1$SaludG, summary)
psych::describeBy(DB_1$Edad_2, DB_1$SaludG)
boxplot(DB_1$Edad_2 ~ DB_1$SaludG)
leveneTest(Edad_2 ~ SaludG, data = DB_1)
t.test(Edad_2 ~ SaludG, data = DB_1, var.equal = FALSE)
# no diferencia signifcativa

plot(DB_1$Edad_2, DB_1$Dormir, abline(lm(DB_1$Edad_2~DB_1$Dormir), col="red"))
ggplot(DB_1) + geom_qq(aes(sample = Edad_2)) 
ggplot(DB_1) + geom_qq(aes(sample = Dormir)) 
cor.test( ~ Edad_2 + Dormir,data=DB_1, method = "spearman", continuity = FALSE, conf.level = 0.95)
# no relación significativa

# GENTRIFICACIÓN 
by(DB_1$NCGS,DB_1$GHQ_2, summary)
psych::describeBy(DB_1$NCGS,DB_1$GHQ_2)
boxplot(DB_1$NCGS ~ DB_1$GHQ_2)
leveneTest(NCGS ~ GHQ_2, data = DB_1)
t.test(NCGS ~ GHQ_2, data = DB_1, var.equal = TRUE)
# no relación significativa

by(DB_1$NCGS,DB_1$SaludG, summary)
psych::describeBy(DB_1$NCGS, DB_1$SaludG)
boxplot(DB_1$NCGS ~ DB_1$SaludG)
leveneTest(NCGS ~ SaludG, data = DB_1)
t.test(NCGS ~ SaludG, data = DB_1, var.equal = TRUE)
# no relación significativa

plot(DB_1$NCGS, DB_1$Dormir, abline(lm(DB_1$Dormir~DB_1$NCGS), col="red"))

ggplot(DB_1) + geom_qq(aes(sample = NCGS)) 
ggplot(DB_1) + geom_qq(aes(sample = Dormir)) 
cor.test( ~ NCGS + Dormir,data=DB_1, method = "spearman", continuity = FALSE, conf.level = 0.95)
# relación significativa rho = -0.12

# TURISMO

by(DB_1$Turismo,DB_1$GHQ_2, summary)
psych::describeBy(DB_1$Turismo,DB_1$GHQ_2)
boxplot(DB_1$Turismo ~ DB_1$GHQ_2)
leveneTest(Turismo ~ GHQ_2, data = DB_1)
t.test(Turismo ~ GHQ_2, data = DB_1, var.equal = TRUE)
# NO SIGNIFICATIVO

by(DB_1$Turismo,DB_1$SaludG, summary)
psych::describeBy(DB_1$Turismo, DB_1$SaludG)
boxplot(DB_1$Turismo ~ DB_1$SaludG)
leveneTest(Turismo ~ SaludG, data = DB_1)
t.test(Turismo ~ SaludG, data = DB_1, var.equal = FALSE)
#SIGNIFICATIVO

plot(DB_1$NCGS, DB_1$Dormir, abline(lm(DB_1$Dormir~DB_1$NCGS), col="red"))

ggplot(DB_1) + geom_qq(aes(sample = Turismo)) 
ggplot(DB_1) + geom_qq(aes(sample = Dormir)) 
cor.test( ~ Turismo + Dormir,data=DB_1, method = "spearman", continuity = FALSE, conf.level = 0.95)
# relación significativa rho = -0.103


### ANÁLISIS DESCRIPTIVO POR BARRIO
 
# Primero creamos las bases de datos para cada barrio

DB_Barceloneta <- subset(DB_2, Barrio=="Barceloneta")
DB_SA <- subset(DB_2, Barrio=="Sant Antoni")

save(DB_Barceloneta, file= "DB_Barceloneta.RData")
save(DB_SA, file= "DB_SA.RData")

# EMPEZAR LA DESCRIPTIVA DE LA BARCELONETA
library(psych)
library(epiDisplay)
library(tidyverse)
library(gmodels)
library(Hmisc)
library(ggplot2)

#* Tenemos que tener info de: edad, trabajo, origen, educación, situación económica, cuidados, NCGS y turismo
#* También miraremos las variables de Cambio A, B, C, D y total

# edad
describe(DB_Barceloneta$Edad_2)
boxplot(DB_Barceloneta$Edad_2)
ggplot(DB_Barceloneta) + geom_qq(aes(sample = Edad_2)) 
#paramétrica

# nacimiento
summarytools::freq(DB_Barceloneta$Nacimiento)
summarytools::freq(DB_Barceloneta$Nacimiento_2)

# educación
summarytools::freq(DB_Barceloneta$Estudios_2)
summarytools::freq(DB_Barceloneta$Estudios)

# laboral
summarytools::freq(DB_Barceloneta$Ahora_laboral)
summarytools::freq(DB_Barceloneta$Laboral)

# monetaria
summarytools::freq(DB_Barceloneta$Ahora_monetaria)
summarytools::freq(DB_Barceloneta$Monetaria)

# cuidados
summarytools::freq(DB_Barceloneta$Ahora_cuidado)

# NCGS
describe(DB_Barceloneta$NCGS)
boxplot(DB_Barceloneta$NCGS)
ggplot(DB_Barceloneta) + geom_qq(aes(sample = NCGS))
#parametrica

# turismo
describe(DB_Barceloneta$Turismo)
boxplot(DB_Barceloneta$Turismo)
ggplot(DB_Barceloneta) + geom_qq(aes(sample = Turismo))
#no paramétrica

# cambios A
summarytools::freq(DB_Barceloneta$Cambios_A)
summarytools::freq(DB_Barceloneta$Tiempo_A)

# cambios B
summarytools::freq(DB_Barceloneta$Cambios_B)

# cambios C
summarytools::freq(DB_Barceloneta$Cambios_C)

# cambios D
summarytools::freq(DB_Barceloneta$Cambios_D)

# cambios AB
summarytools::freq(DB_Barceloneta$Cambios_AB)

# cambios todos
summarytools::freq(DB_Barceloneta$Cambios_ABCD)

###### DESCRIPTIVA DE SANT ANTONI ########

#* Tenemos que tener info de: edad, trabajo, origen, educación, situación económica, cuidados, NCGS y turismo
#* También miraremos las variables de Cambio A, B, C, D y total

# edad
describe(DB_SA$Edad_2)
boxplot(DB_SA$Edad_2)
ggplot(DB_SA) + geom_qq(aes(sample = Edad_2)) 
#paramétrica

# nacimiento
summarytools::freq(DB_SA$Nacimiento)
summarytools::freq(DB_SA$Nacimiento_2)

# educación
summarytools::freq(DB_SA$Estudios_2)
summarytools::freq(DB_SA$Estudios)

# laboral
summarytools::freq(DB_SA$Ahora_laboral)
summarytools::freq(DB_SA$Laboral)

# monetaria
summarytools::freq(DB_SA$Ahora_monetaria)
summarytools::freq(DB_SA$Monetaria)

# cuidados
summarytools::freq(DB_SA$Ahora_cuidado)

# NCGS
describe(DB_SA$NCGS)
boxplot(DB_SA$NCGS)
ggplot(DB_SA) + geom_qq(aes(sample = NCGS))
#parametrica

# turismo
describe(DB_SA$Turismo)
boxplot(DB_SA$Turismo)
ggplot(DB_SA) + geom_qq(aes(sample = Turismo))
#no paramétrica

# cambios A
summarytools::freq(DB_SA$Cambios_A)
summarytools::freq(DB_SA$Tiempo_A)

# cambios B
summarytools::freq(DB_SA$Cambios_B)

# cambios C
summarytools::freq(DB_SA$Cambios_C)

# cambios D
summarytools::freq(DB_SA$Cambios_D)

# cambios AB
summarytools::freq(DB_SA$Cambios_AB)

# cambios todos
summarytools::freq(DB_SA$Cambios_ABCD)
