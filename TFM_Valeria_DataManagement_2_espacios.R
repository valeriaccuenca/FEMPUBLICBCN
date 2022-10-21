######## MANAGEMENT DE VARIABLES DEL ESPACIO PÚBLICO ###############

# Primero voy a guardar una nueva base de datos sobre la que trabajaré, por si
#* meto la pata y me cargo la base que ya tengo 
DB_2 <- DB_1

#* Explorar primero cada variable sola vs. el outcome de interés (análisis bivariados) 

# Cargar package gmodels para obtener las tablas de contingencia
library(gmodels)
######### VARIABLES CONTINUAS ################
# Cargar package psych para realizar la descriptiva de las variables continuas 
#  para cada categoría de la variable categórica y package car para test de levene
library(psych)
library(car)

# Visitas Superilla/playa 
CrossTable(DB_2$Visitas_A,DB_2$GHQ_2, expected = TRUE)
# diferencia significativa

CrossTable(DB_2$Visitas_A,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Visitas_A,summary)
psych::describeBy(DB_2$Dormir, DB_2$Visitas_A)
boxplot(DB_2$Dormir ~ DB_2$Visitas_A)
wilcox.test(Dormir ~ Visitas_A, data = DB_2)
# no diferencia significativa


# Visitas <15 min 
CrossTable(DB_2$Visitas_B,DB_2$GHQ_2, expected = TRUE)
# diferencia significativa

CrossTable(DB_2$Visitas_B,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Visitas_B,summary)
psych::describeBy(DB_2$Dormir, DB_2$Visitas_B)
boxplot(DB_2$Dormir ~ DB_2$Visitas_B)
wilcox.test(Dormir ~ Visitas_B, data = DB_2)
# no diferencia significativa

# Visitas >15 min 
CrossTable(DB_2$Visitas_C,DB_2$GHQ_2, expected = TRUE)
# no diferencia significativa

CrossTable(DB_2$Visitas_C,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Visitas_C,summary)
psych::describeBy(DB_2$Dormir, DB_2$Visitas_C)
boxplot(DB_2$Dormir ~ DB_2$Visitas_C)
wilcox.test(Dormir ~ Visitas_C, data = DB_2)
# no diferencia significativa

# Visitas Alrededores 
CrossTable(DB_2$Visitas_D,DB_2$GHQ_2, expected = TRUE)
# no diferencia significativa

CrossTable(DB_2$Visitas_D,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Visitas_D,summary)
psych::describeBy(DB_2$Dormir, DB_2$Visitas_D)
boxplot(DB_2$Dormir ~ DB_2$Visitas_D)
wilcox.test(Dormir ~ Visitas_D, data = DB_2)
# no diferencia significativa

#######################################################

### Tiempo Superilla/playa ###
CrossTable(DB_2$Tiempo_A,DB_2$GHQ_2, expected = TRUE)
#  diferencia significativa

CrossTable(DB_2$Tiempo_A,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Tiempo_A,summary)
psych::describeBy(DB_2$Dormir, DB_2$Tiempo_A)
boxplot(DB_2$Dormir ~ DB_2$Tiempo_A)
wilcox.test(Dormir ~ Tiempo_A, data = DB_2)
# diferencia significativa

### Tiempo espacios <15 min 
CrossTable(DB_2$Tiempo_B,DB_2$GHQ_2, expected = TRUE)
#  diferencia significativa

CrossTable(DB_2$Tiempo_B,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Tiempo_B,summary)
psych::describeBy(DB_2$Dormir, DB_2$Tiempo_B)
boxplot(DB_2$Dormir ~ DB_2$Tiempo_B)
wilcox.test(Dormir ~ Tiempo_B, data = DB_2)
# no diferencia significativa

### Tiempo espacios >15 min 
CrossTable(DB_2$Tiempo_C,DB_2$GHQ_2, expected = TRUE)
# no diferencia significativa

CrossTable(DB_2$Tiempo_C,DB_2$SaludG, expected = TRUE)
# no diferencia significativa

by(DB_2$Dormir,DB_2$Tiempo_C,summary)
psych::describeBy(DB_2$Dormir, DB_2$Tiempo_C)
boxplot(DB_2$Dormir ~ DB_2$Tiempo_C)
wilcox.test(Dormir ~ Tiempo_C, data = DB_2)
# no diferencia significativa

### Tiempo Alrededores
CrossTable(DB_2$Tiempo_D,DB_2$GHQ_2, expected = TRUE)
# no diferencia significativa

CrossTable(DB_2$Tiempo_D,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Tiempo_D,summary)
psych::describeBy(DB_2$Dormir, DB_2$Tiempo_D)
boxplot(DB_2$Dormir ~ DB_2$Tiempo_D)
wilcox.test(Dormir ~ Tiempo_D, data = DB_2)
# no diferencia significativa

### RESUMEN DE RESULTADOS ####
# El uso de los espacios A y B sí está relacionado con las variables de salud GHQ y Salud General
# El uso de los espacios C y D sólo tienen relación con la variable de salud general
# Sólo los espacios A tienen relación con la calidad del sueño
# ¿Es posible que sea por la falta de respuesta o porque realmente estos lugares son menos importantes?

############ VAMOS A PELEARNOS ###############
# Una forma interesante es usar la función dplyr::case_when(), 
#* Primero hay que cargar el package dplyr
library(dplyr)
library(dbplyr)
#* df %>% 
#* mutate(
#  cond = case_when( 
 #   a == "SI" & b == "AYER" ~ 5,
  #  a == "NO" & b == "HOY" ~ 10,
#    TRUE ~ 20
 # )
# )

#  <is.na(a) & b == "1"  ~ "C"> se añade para incluir las respuestas en las que una de las variables
# tenga NA pero en la otra sí haya respuesta (al menos ha aumentado/mantenido en una de las variables)

# mutate para crear una nueva columna (CambiosA, CambiosB, CambiosC y CambiosD)
# case_when() para establecer las condicionas de forma (condición ~ valor deseado)
# el valor default será TRUE ~ valor
# las condiciones se evalúan según el orden

# UNIMOS LAS VARIABLES DE VISITAS Y TIEMPO PARA TODOS LOS ESPACIOS
# A = SÓLO REPORTAN DISMINUCIÓN DE FRECUENCIA Y/O TIEMPO
# B = SÓLO REPORTAN MANTENIMIENTO/AUMENTO DE FRECUENCIA Y/O TIEMPO
# C = SÓLO UNA DE ELLAS SE MANTIENE/AUMENTA

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

### ANÁLISIS BIVARIADO ###
library(gmodels)

# SUPERILLA Y PLAYA
CrossTable(DB_2$Cambios_A,DB_2$GHQ_2, expected = TRUE)
#  diferencia significativa

CrossTable(DB_2$Cambios_A,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Cambios_A,summary)
psych::describeBy(DB_2$Dormir, DB_2$Cambios_A)
boxplot(DB_2$Dormir ~ DB_2$Cambios_A)
kruskal.test(DB_2$Dormir, DB_2$Cambios_A)
# no diferencia significativa

# ESPACIOS < 15 MIN
CrossTable(DB_2$Cambios_B,DB_2$GHQ_2, expected = TRUE)
#  diferencia significativa

CrossTable(DB_2$Cambios_B,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Cambios_B,summary)
psych::describeBy(DB_2$Dormir, DB_2$Cambios_B)
boxplot(DB_2$Dormir ~ DB_2$Cambios_B)
kruskal.test(DB_2$Dormir, DB_2$Cambios_B)
# no diferencia significativa

# ESPACIOS > 15 MIN
CrossTable(DB_2$Cambios_C,DB_2$GHQ_2, expected = TRUE)
# diferencia significativa

CrossTable(DB_2$Cambios_C,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Cambios_C,summary)
psych::describeBy(DB_2$Dormir, DB_2$Cambios_C)
boxplot(DB_2$Dormir ~ DB_2$Cambios_C)
kruskal.test(DB_2$Dormir, DB_2$Cambios_C)
# no diferencia significativa

# ALREDEDORES DE BARCELONA
CrossTable(DB_2$Cambios_D,DB_2$GHQ_2, expected = TRUE)
# no diferencia significativa

CrossTable(DB_2$Cambios_D,DB_2$SaludG, expected = TRUE)
# no diferencia significativa

by(DB_2$Dormir,DB_2$Cambios_D,summary)
psych::describeBy(DB_2$Dormir, DB_2$Cambios_D)
boxplot(DB_2$Dormir ~ DB_2$Cambios_D)
kruskal.test(DB_2$Dormir, DB_2$Cambios_D)
# no diferencia significativa

# guardar base de datos
save(DB_2, file= "DB_Espacios2.RData")


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

#### ANÁLISIS BIVARIADO #####

CrossTable(DB_2$Cambios_AB,DB_2$GHQ_2, expected = TRUE)
# diferencia significativa

CrossTable(DB_2$Cambios_AB,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Cambios_AB,summary)
psych::describeBy(DB_2$Dormir, DB_2$Cambios_AB)
boxplot(DB_2$Dormir ~ DB_2$Cambios_AB)
kruskal.test(DB_2$Dormir, DB_2$Cambios_AB)
# no diferencia significativa

#################################################################
###### CONSTRUIR MACROVARIABLE SIN MORIR EN EL INTENTO ##########
#################################################################

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

#### ANÁLISIS BIVARIADO #####

CrossTable(DB_2$Cambios_CD,DB_2$GHQ_2, expected = TRUE)
# NO diferencia significativa

CrossTable(DB_2$Cambios_CD,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Cambios_CD,summary)
psych::describeBy(DB_2$Dormir, DB_2$Cambios_CD)
boxplot(DB_2$Dormir ~ DB_2$Cambios_CD)
kruskal.test(DB_2$Dormir, DB_2$Cambios_CD)
# no diferencia significativa

DB_22 <- DB_2

######################################################################
### VARIABLE QUE REÚNA TODOS LOS ESPACIOS DENTRO DE LA CIUDAD ########
######## SUPERILLA/ PLAYA + <15 MIN + >15 MIN ########################
DB_2 <- DB_2 %>%  dplyr::mutate(Cambios_ABC = case_when( 
  Cambios_C == "A" & Cambios_AB == "A" ~ "A",
  Cambios_C == "B" & Cambios_AB == "B" ~ "B",
  Cambios_C == "C" & Cambios_AB == "C" ~ "C",
  Cambios_C == "A" & Cambios_AB == "B" ~ "C",
  Cambios_C == "B" & Cambios_AB == "A" ~ "C",
  TRUE ~ "C"
))
summarytools::freq(DB_2$Cambios_ABC)

#Bivariado

CrossTable(DB_2$Cambios_ABC,DB_2$GHQ_2, expected = TRUE)
# diferencia significativa

CrossTable(DB_2$Cambios_ABC,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Cambios_ABC,summary)
psych::describeBy(DB_2$Dormir, DB_2$Cambios_ABC)
boxplot(DB_2$Dormir ~ DB_2$Cambios_ABC)
kruskal.test(DB_2$Dormir, DB_2$Cambios_ABC)
# no diferencia significativa

######################################################################
#### VAMOS A HACER LA BIG ASS VARIABLE #########################

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

#### ANÁLISIS BIVARIADO #####

CrossTable(DB_2$Cambios_ABCD,DB_2$GHQ_2, expected = TRUE)
# diferencia significativa

CrossTable(DB_2$Cambios_ABCD,DB_2$SaludG, expected = TRUE)
# diferencia significativa

by(DB_2$Dormir,DB_2$Cambios_ABCD,summary)
psych::describeBy(DB_2$Dormir, DB_2$Cambios_ABCD)
boxplot(DB_2$Dormir ~ DB_2$Cambios_ABCD)
kruskal.test(DB_2$Dormir, DB_2$Cambios_ABCD)
# no diferencia significativa

save(DB_2, file= "DB_Final_2.RData")

## RELACIÓN ENTRE ESPACIOS PÚBLICOS Y TURISMO

# ESPACIOS A
by(DB_2$Turismo,DB_2$Cambios_A, summary)
psych::describeBy(DB_2$Turismo,DB_2$Cambios_A)
boxplot(DB_2$Turismo ~ DB_2$Cambios_A)
Cambios_A_Turismo <- multinom(Turismo ~ Cambios_A, data = DB_2)
Anova(Cambios_A_Turismo)
# diferencia significativa

# ESPACIOS B
by(DB_2$Turismo,DB_2$Cambios_B, summary)
psych::describeBy(DB_2$Turismo,DB_2$Cambios_B)
boxplot(DB_2$Turismo ~ DB_2$Cambios_B)
Cambios_B_Turismo <- multinom(Turismo ~ Cambios_B, data = DB_2)
Anova(Cambios_B_Turismo)
# NO diferencia significativa

# ESPACIOS C
by(DB_2$Turismo,DB_2$Cambios_C, summary)
psych::describeBy(DB_2$Turismo,DB_2$Cambios_C)
boxplot(DB_2$Turismo ~ DB_2$Cambios_C)
Cambios_C_Turismo <- multinom(Turismo ~ Cambios_C, data = DB_2)
Anova(Cambios_C_Turismo)
# NO diferencia significativa

# ESPACIOS D
by(DB_2$Turismo,DB_2$Cambios_D, summary)
psych::describeBy(DB_2$Turismo,DB_2$Cambios_D)
boxplot(DB_2$Turismo ~ DB_2$Cambios_D)
Cambios_D_Turismo <- multinom(Turismo ~ Cambios_D, data = DB_2)
Anova(Cambios_D_Turismo)
# NO diferencia significativa

# ESPACIOS AB
by(DB_2$Turismo,DB_2$Cambios_AB, summary)
psych::describeBy(DB_2$Turismo,DB_2$Cambios_AB)
boxplot(DB_2$Turismo ~ DB_2$Cambios_AB)
Cambios_AB_Turismo <- lm(Turismo ~ Cambios_AB, data = DB_2)
anova(Cambios_AB_Turismo)
# diferencia significativa MUY CERCANA A 0.05 (=0,0499)

# ESPACIOS CD
by(DB_2$Turismo,DB_2$Cambios_CD, summary)
psych::describeBy(DB_2$Turismo,DB_2$Cambios_CD)
boxplot(DB_2$Turismo ~ DB_2$Cambios_CD)
Cambios_CD_Turismo <- multinom(Turismo ~ Cambios_CD, data = DB_2)
Anova(Cambios_CD_Turismo)
# NO diferencia significativa 

# ESPACIOS ABC
by(DB_2$Turismo,DB_2$Cambios_ABC, summary)
psych::describeBy(DB_2$Turismo,DB_2$Cambios_ABC)
boxplot(DB_2$Turismo ~ DB_2$Cambios_ABC)
Cambios_ABC_Turismo <- lm(Turismo ~ Cambios_ABC, data = DB_2)
anova(Cambios_ABC_Turismo)
# diferencia significativa MUY CERCANA A 0.05 (=0,0499)


# TODOS LOS ESPACIOS
by(DB_2$Turismo,DB_2$Cambios_ABCD, summary)
psych::describeBy(DB_2$Turismo,DB_2$Cambios_ABCD)
boxplot(DB_2$Turismo ~ DB_2$Cambios_ABCD)
Cambios_TODOS_Turismo <- lm(Turismo ~ Cambios_ABCD, data = DB_2)
anova(Cambios_TODOS_Turismo)
# NO diferencia significativa 

#### RELACIÓN ENTRE ESPACIOS PÚBLICOS Y GENTRIFICACIÓN #############

# ESPACIOS A
by(DB_2$NCGS,DB_2$Cambios_A, summary)
psych::describeBy(DB_2$NCGS,DB_2$Cambios_A)
boxplot(DB_2$NCGS ~ DB_2$Cambios_A)
Cambios_A_NCGS <- multinom(NCGS ~ Cambios_A, data = DB_2)
Anova(Cambios_A_NCGS)
# NO diferencia significativa

# ESPACIOS B
by(DB_2$NCGS,DB_2$Cambios_B, summary)
psych::describeBy(DB_2$NCGS,DB_2$Cambios_B)
boxplot(DB_2$NCGS ~ DB_2$Cambios_B)
Cambios_B_NCGS<- multinom(NCGS ~ Cambios_B, data = DB_2)
Anova(Cambios_B_NCGS)
# NO diferencia significativa

# ESPACIOS C
by(DB_2$NCGS,DB_2$Cambios_C, summary)
psych::describeBy(DB_2$NCGS,DB_2$Cambios_C)
boxplot(DB_2$NCGS ~ DB_2$Cambios_C)
Cambios_C_NCGS <- multinom(NCGS ~ Cambios_C, data = DB_2)
Anova(Cambios_C_NCGS)
# NO diferencia significativa

# ESPACIOS D
by(DB_2$NCGS,DB_2$Cambios_D, summary)
psych::describeBy(DB_2$NCGS,DB_2$Cambios_D)
boxplot(DB_2$NCGS ~ DB_2$Cambios_D)
Cambios_D_NCGS <- multinom(NCGS ~ Cambios_D, data = DB_2)
Anova(Cambios_D_NCGS)
# NO diferencia significativa

# ESPACIOS AB
by(DB_2$NCGS,DB_2$Cambios_AB, summary)
psych::describeBy(DB_2$NCGS,DB_2$Cambios_AB)
boxplot(DB_2$NCGS ~ DB_2$Cambios_AB)
Cambios_AB_NCGS <- lm(NCGS ~ Cambios_AB, data = DB_2)
anova(Cambios_AB_NCGS)
# diferencia significativa

# ESPACIOS ABC
by(DB_2$NCGS,DB_2$Cambios_ABC, summary)
psych::describeBy(DB_2$NCGS,DB_2$Cambios_ABC)
boxplot(DB_2$NCGS ~ DB_2$Cambios_ABC)
Cambios_ABC_NCGS <- lm(NCGS ~ Cambios_ABC, data = DB_2)
anova(Cambios_ABC_NCGS)
# diferencia significativa

# ESPACIOS CD
by(DB_2$NCGS,DB_2$Cambios_CD, summary)
psych::describeBy(DB_2$NCGS,DB_2$Cambios_CD)
boxplot(DB_2$NCGS ~ DB_2$Cambios_CD)
Cambios_CD_NCGS <- multinom(NCGS ~ Cambios_CD, data = DB_2)
Anova(Cambios_CD_NCGS)
# NO diferencia significativa 

# TODOS LOS ESPACIOS
by(DB_2$NCGS,DB_2$Cambios_ABCD, summary)
psych::describeBy(DB_2$NCGS,DB_2$Cambios_ABCD)
boxplot(DB_2$NCGS ~ DB_2$Cambios_ABCD)
Cambios_TODOS_NCGS <- lm(NCGS ~ Cambios_ABCD, data = DB_2)
anova(Cambios_TODOS_NCGS)
# NO diferencia significativa 


