# Cargar package gmodels para obtener las tablas de contingencia
library(gmodels)
######### VARIABLES CONTINUAS ################
# Cargar package psych para realizar la descriptiva de las variables continuas 
#  para cada categoría de la variable categórica y package car para test de levene
library(psych)
library(car)
library(ggplot2)

DB_1 <- DB_FPBCN
# ANALISIS BIVARIADO ENTRE LAS VARIABLES DE SALUD Y LAS DE CAMBIO EN TURISMO Y GENTRIFICACIÓN

# Primero las continuas

# Diferencia en gentrificación
  # salud general
by(DB_1$Dif_NCGS,DB_1$Ahora_salud2, summary)
psych::describeBy(DB_1$Dif_NCGS,DB_1$Ahora_salud2)
boxplot(DB_1$Dif_NCGS ~ DB_1$Ahora_salud2)
leveneTest(Dif_NCGS ~ Ahora_salud2, data = DB_1)
t.test(Dif_NCGS ~ Ahora_salud2, data = DB_1, var.equal = TRUE)
# NO SIGNIFICATIVO (t = -1.4848, df = 898, p-value = 0.138) 

  # salud mental
by(DB_1$Dif_NCGS,DB_1$Ahora_GHQ2, summary)
psych::describeBy(DB_1$Dif_NCGS,DB_1$Ahora_GHQ2)
boxplot(DB_1$Dif_NCGS ~ DB_1$Ahora_GHQ2)
leveneTest(Dif_NCGS ~ Ahora_GHQ2, data = DB_1)
t.test(Dif_NCGS ~ Ahora_GHQ2, data = DB_1, var.equal = TRUE)
# NO SIGNIFICATIVO (t = 0.92651, df = 857, p-value = 0.3544)

  # calidad del sueño
plot(DB_1$Dif_NCGS, DB_1$Ahora_dormir, abline(lm(DB_1$Dif_NCGS~DB_1$Ahora_dormir), col="red"))
ggplot(DB_1) + geom_qq(aes(sample = Dif_NCGS)) 
ggplot(DB_1) + geom_qq(aes(sample = Ahora_dormir)) 
cor.test( ~ Dif_NCGS + Ahora_dormir,data=DB_1, method = "spearman", continuity = FALSE, conf.level = 0.95)
# NO SIGNIFICATIVO (S = 125767660, p-value = 0.208)

# Diferencia en turismo
# salud general
by(DB_1$Dif_Turismo,DB_1$Ahora_salud2, summary)
psych::describeBy(DB_1$Dif_Turismo,DB_1$Ahora_salud2)
boxplot(DB_1$Dif_Turismo ~ DB_1$Ahora_salud2)
wilcox.test(Dif_Turismo ~ Ahora_salud2, data = DB_1)
# SIGNIFICATIVO (W = 80979, p-value = 0.03718) 

# salud mental
by(DB_1$Dif_Turismo,DB_1$Ahora_GHQ2, summary)
psych::describeBy(DB_1$Dif_Turismo,DB_1$Ahora_GHQ2)
boxplot(DB_1$Dif_Turismo ~ DB_1$Ahora_GHQ2)
wilcox.test(Dif_Turismo ~ Ahora_GHQ2, data = DB_1)
# NO SIGNIFICATIVO (W = 82438, p-value = 0.439)

# calidad del sueño
plot(DB_1$Dif_Turismo, DB_1$Ahora_dormir, abline(lm(DB_1$Dif_NCGS~DB_1$Ahora_dormir), col="red"))
ggplot(DB_1) + geom_qq(aes(sample = Dif_Turismo)) 
ggplot(DB_1) + geom_qq(aes(sample = Ahora_dormir)) 
cor.test( ~ Dif_Turismo + Ahora_dormir,data=DB_1, method = "spearman", continuity = FALSE, conf.level = 0.95)
# NO SIGNIFICATIVO (S = 125863765, p-value = 0.4418)

# Ahora lo hacemos con las cariables categóricas del cambio

# Primero gentrificación
  # salud general
CrossTable(DB_1$Dif_NCGS_2, DB_1$Ahora_salud2, expected = TRUE)
# NO SIGNIFICATIVO (chi^2 =  1.776273     d.f. =  2     p =  0.4114218)

CrossTable(DB_1$Dif_NCGS_1, DB_1$Ahora_salud2, expected = TRUE)
# NO SIGNIFICATIVO (Chi^2 =  1.776273     d.f. =  1     p =  0.1826077)

  # salud mental
CrossTable(DB_1$Dif_NCGS_2, DB_1$Ahora_GHQ2, expected = TRUE)
# NO SIGNIFICATIVO (Chi^2 =  0.2006578     d.f. =  2     p =  0.9045399)
CrossTable(DB_1$Dif_NCGS_1, DB_1$Ahora_GHQ2, expected = TRUE)
# NO SIGNIFICATIVO (Chi^2 =  0.0441148     d.f. =  1     p =  0.8336402 )


 # calidad del sueño
by(DB_1$Ahora_dormir,DB_1$Dif_NCGS_2, summary)
psych::describeBy(DB_1$Ahora_dormir,DB_1$Dif_NCGS_2)
boxplot(DB_1$Ahora_dormir ~ DB_1$Dif_NCGS_2)
DifGen_Dormir <- lm(Ahora_dormir ~ Dif_NCGS_2, data = DB_1)
anova(DifGen_Dormir)
# NO SIGNIFICATIVO (F=1.6229, p-value=0.1979)
wilcox.test(Ahora_dormir ~ Dif_NCGS_1, data = DB_1)
# NO SIGNIFICATIVO W = 89844, p-value = 0.1826

# Ahora turistificación
# salud general
CrossTable(DB_1$Dif_Turismo_2, DB_1$Ahora_salud2, expected = TRUE)
# NO SIGNIFICATIVO (Chi^2 =  4.117157     d.f. =  2     p =  0.1276353)
CrossTable(DB_1$Dif_Turismo_1, DB_1$Ahora_salud2, expected = TRUE)
# NO SIGNIFICATIVO (Chi^2 =  1.376747     d.f. =  1     p =  0.240656)

# salud mental
CrossTable(DB_1$Dif_Turismo_2, DB_1$Ahora_GHQ2, expected = TRUE)
# NO SIGNIFICATIVO (Chi^2 =  0.7974131     d.f. =  2     p =  0.6711876 )
CrossTable(DB_1$Dif_Turismo_1, DB_1$Ahora_GHQ2, expected = TRUE)
# NO SIGNIFICATIVO (Chi^2 =  1.376747     d.f. =  1     p =  0.240656 )

# calidad del sueño
by(DB_1$Ahora_dormir,DB_1$Dif_Turismo_2, summary)
psych::describeBy(DB_1$Ahora_dormir,DB_1$Dif_Turismo_2)
boxplot(DB_1$Ahora_dormir ~ DB_1$Dif_Turismo_2)
DifTur_Dormir <- lm(Ahora_dormir ~ Dif_Turismo_2, data = DB_1)
anova(DifTur_Dormir)
# NO SIGNIFICATIVO (F=0.2085, p-value= 0.8118)
wilcox.test(Ahora_dormir ~ Dif_Turismo_1, data = DB_1)
# NO SIGNIFICATIVO (W = 24815, p-value = 0.709)