###########??? DECRIPTIVA BÁSICA UNIVARIADA #########################
DB_2 <- DB_FPBCN
# Cargar package gmodels para obtener las tablas de contingencia
library(gmodels)
######### VARIABLES CONTINUAS ################
# Cargar package psych para realizar la descriptiva de las variables continuas 
#  para cada categoría de la variable categórica y package car para test de levene
library(psych)
library(car)


# Edad
describe(DB_2$Edad)
summarytools::descr(DB_2$Edad)
by(DB_2$Edad,DB_2$Barrio, summary)
psych::describeBy(DB_2$Edad, DB_2$Barrio)
boxplot(DB_2$Edad ~ DB_2$Barrio)
leveneTest(Edad ~ Barrio, data = DB_2)
t.test(Edad ~ Barrio, data = DB_2, var.equal = FALSE)
#no diferencia significativa entre barrios (t = 0.30006, p-valor = 0.7642)



# Estudios
summarytools::freq(DB_2$Estudios_2)
summarytools::freq(DB_Barceloneta$Estudios_2)
summarytools::freq(DB_SA$Estudios_2)
CrossTable(DB_2$Barrio,DB_2$Estudios2, expected = TRUE)
#diferencia significativa entre barrios (Chi^2 =  37.00727, p =  1.176898e-09 )

# Nacimiento
summarytools::freq(DB_2$Nacimiento_2)
summarytools::freq(DB_Barceloneta$Nacimiento_2)
summarytools::freq(DB_SA$Nacimiento_2)
CrossTable(DB_2$Barrio,DB_2$Nacimiento_2, expected = TRUE)
#no diferencia significativa ente barrios (chi-sq = 1.916, p-value = 0.166)

# LABORAL (antes y ahora)
summarytools::freq(DB_2$Ahora_laboral2)
summarytools::freq(DB_Barceloneta$Ahora_laboral2)
summarytools::freq(DB_SA$Ahora_laboral2)
CrossTable(DB_2$Barrio,DB_2$Ahora_laboral2, expected = TRUE)
#no diferencia significativa entre barrios (Chi^2 =  3.902478, p =  0.04821495)

summarytools::freq(DB_2$Antes_laboral2)
summarytools::freq(DB_Barceloneta$Antes_laboral2)
summarytools::freq(DB_SA$Antes_laboral2)
CrossTable(DB_2$Barrio,DB_2$Antes_laboral2, expected = TRUE)
#no diferencia significativa entre barrios (Chi^2 =  5.604307, p =  0.01791638 )

chisq.test(DB_2$Antes_laboral2, DB_2$Ahora_laboral2)
# resultado signifficativo X-squared = 364.34, df = 1, p-value < 2.2e-16


# Monetaria
summarytools::freq(DB_2$Antes_monetaria2)
summarytools::freq(DB_Barceloneta$Antes_monetaria2)
summarytools::freq(DB_SA$Antes_monetaria2)
CrossTable(DB_2$Barrio,DB_2$Antes_monetaria2, expected = TRUE)
#diferencia significativa entre barrios (Chi^2 =  11.0588, p =  0.0008826733)

summarytools::freq(DB_2$Ahora_monetaria2)
summarytools::freq(DB_Barceloneta$Ahora_monetaria2)
summarytools::freq(DB_SA$Ahora_monetaria2)
CrossTable(DB_2$Barrio,DB_2$Ahora_monetaria2, expected = TRUE)
#diferencia significativa entre barrios (Chi^2 =  6.809861, p =  0.009065583)

chisq.test(DB_2$Antes_monetaria2, DB_2$Ahora_monetaria2)
#diferencia significativa: X-squared = 293.43, df = 1, p-value < 2.2e-16

# Cuidados
summarytools::freq(DB_2$Antes_cuidado)
summarytools::freq(DB_Barceloneta$Antes_cuidado)
summarytools::freq(DB_SA$Antes_cuidado)
CrossTable(DB_2$Barrio,DB_2$Antes_cuidado, expected = TRUE)
# diferencia significativa entre barrios (Chi^2 =  11.36678, p =  0.000747695 )
summarytools::freq(DB_2$Ahora_cuidado)
summarytools::freq(DB_Barceloneta$Ahora_cuidado)
summarytools::freq(DB_SA$Ahora_cuidado)
CrossTable(DB_2$Barrio,DB_2$Ahora_cuidado, expected = TRUE)
# no diferencia significativa entre barrios (Chi^2 =  1.657433, p =  0.1979503 )

Cuidados_prepost <- table("Pre" = DB_2$Antes_cuidado, "Post" = DB_2$Ahora_cuidado)
chisq.test(Cuidados_prepost)
fisher.test(DB_2$Antes_cuidado, DB_2$Ahora_cuidado)
Cuidados_prepost
# diferencia significativa X-squared = 508, df = 1, p-value < 2.2e-16
mcnemar.test(Cuidados_prepost)
# McNemar's chi-squared = 0, df = 1, p-value = 1

# NCGS
summarytools::descr(DB_2$Antes_NCGS)
summarytools::descr(DB_Barceloneta$Antes_NCGS)
summarytools::descr(DB_SA$Antes_NCGS)
psych::describeBy(DB_2$Antes_NCGS, DB_2$Barrio)
boxplot(DB_2$Antes_NCGS ~ DB_2$Barrio)
leveneTest(Antes_NCGS ~ Barrio, data = DB_2)
t.test(Antes_NCGS ~ Barrio, data = DB_2, var.equal = TRUE)
# no diferencia significativa entre barrios (t = 0.79859, p-value = 0.79859)
summarytools::descr(DB_2$Ahora_NCGS)
summarytools::descr(DB_Barceloneta$Ahora_NCGS)
summarytools::descr(DB_SA$Ahora_NCGS)
psych::describeBy(DB_2$Ahora_NCGS, DB_2$Barrio)
boxplot(DB_2$Ahora_NCGS ~ DB_2$Barrio)
leveneTest(Ahora_NCGS ~ Barrio, data = DB_2)
t.test(Ahora_NCGS ~ Barrio, data = DB_2, var.equal = TRUE)
#diferencia significativa entre barrios (t = 2.6445, p-value = 0.008324)

leveneTest(DB_2$Antes_NCGS, DB_2$Ahora_NCGS)
#no diferencia significativa en la varianza F = 1.1191, p = 0.2536
t.test(DB_2$Antes_NCGS, DB_2$Ahora_NCGS, var.equal = TRUE)
# diferencia significativa: t = 9.7226, df = 1813, p-value < 2.2e-16


#Cambio entre antes y después
# Primero en continuo y después por categorías
summarytools::descr(DB_2$Dif_NCGS)
boxplot(DB_2$Dif_NCGS)
ggplot(DB_2) + geom_qq(aes(sample = Dif_NCGS)) 
histogram(DB_2$Dif_NCGS)
#es normal
summarytools::descr(DB_Barceloneta$Dif_NCGS)
summarytools::descr(DB_SA$Dif_NCGS)
psych::describeBy(DB_2$Dif_NCGS, DB_2$Barrio)
boxplot(DB_2$Dif_NCGS ~ DB_2$Barrio)
leveneTest(Dif_NCGS ~ Barrio, data = DB_2)
t.test(Dif_NCGS ~ Barrio, data = DB_2, var.equal = FALSE)

summarytools::freq(DB_2$Dif_NCGS_2)
summarytools::freq(DB_Barceloneta$Dif_NCGS_2)
summarytools::freq(DB_SA$Dif_NCGS_2)
CrossTable(DB_2$Dif_NCGS_2, DB_2$Barrio, expected = TRUE)
#diferencia significativa entre barrios, p=0.003

# TURISMO
summarytools::descr(DB_2$Antes_turismo)
summarytools::descr(DB_Barceloneta$Antes_turismo)
summarytools::descr(DB_SA$Antes_turismo)
psych::describeBy(DB_2$Antes_turismo, DB_2$Barrio)
boxplot(DB_2$Antes_turismo ~ DB_2$Barrio)
leveneTest(Antes_turismo ~ Barrio, data = DB_2)
t.test(Antes_turismo ~ Barrio, data = DB_2, var.equal = TRUE)
#no diferencia significativa entre barrios (t = -0.20687, p-value = 0.8367)
summarytools::descr(DB_2$Ahora_turismo)
summarytools::descr(DB_Barceloneta$Ahora_turismo)
summarytools::descr(DB_SA$Ahora_turismo)
psych::describeBy(DB_2$Ahora_turismo, DB_2$Barrio)
boxplot(DB_2$Ahora_turismo ~ DB_2$Barrio)
leveneTest(Ahora_turismo ~ Barrio, data = DB_2)
t.test(Ahora_turismo ~ Barrio, data = DB_2, var.equal = FALSE)
#diferencia significativa entre barrios (t = 4.3324, df = 870.91, p-value = 1.646e-05)

leveneTest(DB_2$Antes_turismo, DB_2$Ahora_turismo)
#diferencia significativa en la varianza F = 0.912, p = 0.5209
t.test(DB_2$Antes_turismo, DB_2$Ahora_turismo, var.equal = TRUE)
# DIFERENCIA SIGNIFICATIVA: t = 43.942, df = 1818, p-value < 2.2e-16

# Diferencia entre los cambios de barrio a barrio
# Primero en continuo y después por categorías
summarytools::descr(DB_2$Dif_Turismo)
boxplot(DB_2$Dif_Turismo)
ggplot(DB_2) + geom_qq(aes(sample = Dif_Turismo)) 
histogram(DB_2$Dif_Turismo)
# no es normal
summarytools::descr(DB_Barceloneta$Dif_Turismo)
summarytools::descr(DB_SA$Dif_Turismo)
psych::describeBy(DB_2$Dif_Turismo, DB_2$Barrio)
boxplot(DB_2$Dif_Turismo ~ DB_2$Barrio)
wilcox.test(Dif_Turismo ~ Barrio, data = DB_2)

summarytools::freq(DB_2$Dif_Turismo_2)
summarytools::freq(DB_Barceloneta$Dif_Turismo_2)
summarytools::freq(DB_SA$Dif_Turismo_2)
CrossTable(DB_2$Dif_Turismo_2, DB_2$Barrio, expected = TRUE)
#no diferencia significativa, p=0.07


# Salud General
summarytools::freq(DB_2$Antes_salud2)
summarytools::freq(DB_Barceloneta$Antes_salud2)
summarytools::freq(DB_SA$Antes_salud2)
CrossTable(DB_2$Barrio, DB_2$Antes_salud2, expected = TRUE)
# diferencia significativa (Chi^2 =  2.725805, p =  0.09873834)
summarytools::freq(DB_Barceloneta$Ahora_salud2)
summarytools::freq(DB_SA$Ahora_salud2)
CrossTable(DB_2$Barrio, DB_2$Ahora_salud2, expected = TRUE)
# diferencia significativa (Chi^2 =  6.135524, p =  0.01324923)

chisq.test(DB_2$Antes_salud2, DB_2$Ahora_salud2)
# diferencia significativa X-squared = 157.52, df = 1, p-value < 2.2e-16

# GHQ
summarytools::freq(DB_2$Antes_GHQ2)
summarytools::freq(DB_Barceloneta$Antes_GHQ2)
summarytools::freq(DB_SA$Antes_GHQ2)
CrossTable(DB_2$Barrio, DB_2$Antes_GHQ2, expected = TRUE)
# diferencia significativa (Chi^2 =  10.92919, p =  0.0009466062 )
summarytools::freq(DB_2$Ahora_GHQ2)
summarytools::freq(DB_Barceloneta$Ahora_GHQ2)
summarytools::freq(DB_SA$Ahora_GHQ2)
CrossTable(DB_2$Barrio, DB_2$Ahora_GHQ2, expected = TRUE)
# no diferencia significativa (Chi^2 =  2.971676, p =  0.08473401 )

chisq.test(DB_2$Antes_GHQ2, DB_2$Ahora_GHQ2)
# diferencia significativa X-squared = 53.973, df = 1, p-value = 2.032e-13

# Calidad del sueño
summarytools::descr(DB_2$Antes_dormir)
summarytools::descr(DB_Barceloneta$Antes_dormir)
summarytools::descr(DB_SA$Antes_dormir)
wilcox.test(Antes_dormir ~ Barrio, data = DB_2)
# diferencia significativa (W = 90351, p-value = 0.003292)
summarytools::descr(DB_2$Ahora_dormir)
summarytools::descr(DB_Barceloneta$Ahora_dormir)
summarytools::descr(DB_SA$Ahora_dormir)
wilcox.test(Ahora_dormir ~ Barrio, data = DB_2) 
# diferencia significativa (W = 84513, p-value = 9.416e-06)

wilcox.test(DB_2$Antes_dormir, DB_2$Ahora_dormir, paired = TRUE)
# diferencia significativa (W = 483420, p-value = 8.619e-12) when paired = default
# diferencia significativa (V = 84509, p-value = 8.619e-12) when paired = TRUE

wilcoxsign_test(Antes_dormir ~ Ahora_dormir, data = DB_2, distribution = "exact")
# Z = 8.9472, p-value < 2.2e-16
wilcox_test(DB_2$Antes_dormir, DB_2$Ahora_dormir, distribution = "exact")

dormir <- factor(c( rep( "Pre", length(DB_2$Antes_dormir) ),
                    rep( "Post", length(DB_2$Ahora_dormir) ) ))
muestraDormir <- c(DB_2$Antes_dormir, DB_2$Ahora_dormir)
wilcox_test(muestraDormir ~ dormir, distribution = "exact")

# CAMBIO EN EL USO DEL ESPACIO PÚBLICO

#ESPACIOS CERCANOS
summarytools::freq(DB_2$Cambios_AB)
summarytools::freq(DB_Barceloneta$Cambios_AB)
summarytools::freq(DB_SA$Cambios_AB)
CrossTable(DB_2$Barrio, DB_2$Cambios_AB, expected = TRUE)
# diferencia significativa (Chi^2 =  69.67173     d.f. =  2     p =  7.429781e-16)

#ESPACIOS DENTRO DE LA CIUDAD
summarytools::freq(DB_2$Cambios_ABC)
summarytools::freq(DB_Barceloneta$Cambios_ABC)
summarytools::freq(DB_SA$Cambios_ABC)
CrossTable(DB_2$Barrio, DB_2$Cambios_ABC, expected = TRUE)
# diferencia significativa (Chi^2 =  65.1541     d.f. =  2     p =  7.111608e-15)


#TODOS LOS ESPACIOS
summarytools::freq(DB_2$Cambios_ABCD)
summarytools::freq(DB_Barceloneta$Cambios_ABCD)
summarytools::freq(DB_SA$Cambios_ABCD)
CrossTable(DB_2$Barrio, DB_2$Cambios_ABCD, expected = TRUE)
# diferencia significativa (Chi^2 =  63.22994     d.f. =  2     p =  1.861197e-14)


