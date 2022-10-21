##############################################################################
###################### DESCRIPTIVA UNIVARIADA ############################

# Edad
describe(DB_2$Edad_2)
summarytools::descr(DB_2$Edad_2)
by(DB_2$Edad_2,DB_2$Barrio, summary)
psych::describeBy(DB_2$Edad_2, DB_2$Barrio)
boxplot(DB_2$Edad_2 ~ DB_2$Barrio)
leveneTest(Edad_2 ~ Barrio, data = DB_2)
t.test(Edad_2 ~ Barrio, data = DB_2, var.equal = FALSE)
#no diferencia significativa entre barrios

# Nacimiento
summarytools::freq(DB_2$Nacimiento_2)
summarytools::freq(DB_Barceloneta$Nacimiento_2)
summarytools::freq(DB_SA$Nacimiento_2)
CrossTable(DB_2$Barrio,DB_2$Nacimiento_2, expected = TRUE)
#no diferencia significativa ente barrios

# Laboral
summarytools::freq(DB_2$Ahora_laboral)
summarytools::freq(DB_Barceloneta$Ahora_laboral)
summarytools::freq(DB_SA$Ahora_laboral)
CrossTable(DB_2$Barrio,DB_2$Ahora_laboral, expected = TRUE)

summarytools::freq(DB_2$Laboral)
summarytools::freq(DB_Barceloneta$Laboral)
summarytools::freq(DB_SA$Laboral)
CrossTable(DB_2$Barrio,DB_2$Laboral, expected = TRUE)
#no diferencia significativa entre barrios

# Estudios
summarytools::freq(DB_2$Estudios_2)
summarytools::freq(DB_Barceloneta$Estudios_2)
summarytools::freq(DB_SA$Estudios_2)
CrossTable(DB_2$Barrio,DB_2$Estudios_2, expected = TRUE)
#diferencia significativa entre barrios

# Monetaria
summarytools::freq(DB_2$Ahora_monetaria)
summarytools::freq(DB_Barceloneta$Ahora_monetaria)
summarytools::freq(DB_SA$Ahora_monetaria)
CrossTable(DB_2$Barrio,DB_2$Ahora_monetaria, expected = TRUE)
#diferencia significativa entre barrios

summarytools::freq(DB_2$Monetaria)
summarytools::freq(DB_Barceloneta$Monetaria)
summarytools::freq(DB_SA$Monetaria)
CrossTable(DB_2$Barrio,DB_2$Monetaria, expected = TRUE)
#diferencia significativa entre barrios

# Cuidados
summarytools::freq(DB_2$Cuidado)
summarytools::freq(DB_Barceloneta$Cuidado)
summarytools::freq(DB_SA$Cuidado)
CrossTable(DB_2$Barrio,DB_2$Cuidado, expected = TRUE)
# no diferencia significativa entre barrios

# NCGS
summarytools::descr(DB_2$NCGS)
summarytools::descr(DB_Barceloneta$NCGS)
summarytools::descr(DB_SA$NCGS)
psych::describeBy(DB_2$NCGS, DB_2$Barrio)
boxplot(DB_2$NCGS ~ DB_2$Barrio)
leveneTest(NCGS ~ Barrio, data = DB_2)
t.test(NCGS ~ Barrio, data = DB_2, var.equal = TRUE)
#diferencia significativa entre barrios

# Turismo
summarytools::descr(DB_2$Turismo)
summarytools::descr(DB_Barceloneta$Turismo)
summarytools::descr(DB_SA$Turismo)
psych::describeBy(DB_2$Turismo, DB_2$Barrio)
boxplot(DB_2$Turismo ~ DB_2$Barrio)
leveneTest(Turismo ~ Barrio, data = DB_2)
t.test(Turismo ~ Barrio, data = DB_2, var.equal = FALSE)
#diferencia significativa entre barrios

# GHQ
summarytools::freq(DB_Barceloneta$GHQ_2)
summarytools::freq(DB_SA$GHQ_2)
CrossTable(DB_2$Barrio, DB_2$GHQ_2, expected = TRUE)
# no diferencia significativa

# Salud General
summarytools::freq(DB_Barceloneta$SaludG)
summarytools::freq(DB_SA$SaludG)
CrossTable(DB_2$Barrio, DB_2$SaludG, expected = TRUE)
# diferencia significativa

# Calidad del sueño
summarytools::descr(DB_2$Dormir)
summarytools::descr(DB_Barceloneta$Dormir)
summarytools::descr(DB_SA$Dormir)
wilcox.test(Dormir ~ Barrio, data = DB_2)
# diferencia significativa

# CAMBIO EN EL USO DEL ESPACIO PÚBLICO

#ESPACIOS CERCANOS
summarytools::freq(DB_2$Cambios_AB)
summarytools::freq(DB_Barceloneta$Cambios_AB)
summarytools::freq(DB_SA$Cambios_AB)
CrossTable(DB_2$Barrio, DB_2$Cambios_AB, expected = TRUE)
#ESPACIOS DENTRO DE LA CIUDAD
summarytools::freq(DB_2$Cambios_ABC)
summarytools::freq(DB_Barceloneta$Cambios_ABC)
summarytools::freq(DB_SA$Cambios_ABC)
CrossTable(DB_2$Barrio, DB_2$Cambios_ABC, expected = TRUE)
#TODOS LOS ESPACIOS
summarytools::freq(DB_2$Cambios_ABCD)
summarytools::freq(DB_Barceloneta$Cambios_ABCD)
summarytools::freq(DB_SA$Cambios_ABCD)
CrossTable(DB_2$Barrio, DB_2$Cambios_ABCD, expected = TRUE)
