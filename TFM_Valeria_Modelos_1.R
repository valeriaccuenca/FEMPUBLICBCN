#############################################
##### MODELOS DE REGRESIÓN LOGÍSTICA ######
############################################
library(epiDisplay)

# Ejemplo de los modelos (plantilla)

modelo_1 <- glm(GHQ_2 ~ Cambios_ABCD, data = DB_2, family = "binomial") 
summary(modelo_1)
exp(cbind(OR = coef(modelo_1), confint(modelo_1)))    

modelo_2 <- glm(SaludG ~ Cambios_AB, data = DB_2, family = "binomial")
summary(modelo_2)
exp(cbind(OR = coef(modelo_2), confint(modelo_2)))

modelo_3 <- glm(Dormir_2 ~ Cambios_A, data = DB_2, family = "binomial") 
summary(modelo_3)
exp(cbind(OR = coef(modelo_3), confint(modelo_3)))

summarytools::freq(DB_2$Dormir_2)

##############################################################
###### MODELOS PARA SALUD MENTAL ############################
##############################################################

# SÓLO ESPACIOS CERCANOS (SUPERILLA/PLAYA Y <15 MIN = ESPACIOS A Y B)

GHQ_AB <- glm(GHQ_2 ~ Cambios_AB, data = DB_2, family = "binomial") 
summary(GHQ_AB)
exp(cbind(OR = coef(GHQ_AB), confint(GHQ_AB)))
                  OR     2.5 %    97.5 %
(Intercept) 0.7450980 0.4866845 1.1309072
Cambios_ABB 0.5665168 0.3594180 0.8988909
Cambios_ABC 1.1849218 0.7198373 1.9615652


# Para ajustar el modelo, se insertan las variables cuya relación ha sido significativa con la GHQ
# en el análisis bivariado, que en este caso han sido: Edad, nivel educativo situación laboral y situación monetaria
# Incluyo la NCGS porque su relación con los espacios seleccionados aquí sí es estadísticamente significativa
# según el análisis bivariado 

GHQ_AB_adj <- glm(GHQ_2 ~ Cambios_AB + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(GHQ_AB_adj)
exp(cbind(OR = coef(GHQ_AB_adj), confint(GHQ_AB_adj)))
                              OR     2.5 %    97.5 %
(Intercept)              0.9810429 0.4918693 1.9545770
Cambios_ABB              0.5347069 0.3290911 0.8724981
Cambios_ABC              1.2223355 0.7231332 2.0766979
Edad_2                   0.9796438 0.9696686 0.9895390
Estudios21               0.8507499 0.6131581 1.1783324
Nacimiento_2Global South 0.7148706 0.4791977 1.0571002
Laboral1                 1.9156517 1.3820493 2.6640078
Monetaria1               2.4904924 1.8182565 3.4300616

#diagnóstico de modelo para ver colinearidad en gráfico (package performance)
check_model(GHQ_AB_adj)
# el gráfico de colinealidad indica que ninguna variable predictora se corresponde con otra
# es decir, la situación laboral y monetaria no son dependientes entre sí

# Modelos de Turismo y gentrificación
GHQ_T_NCGS <- glm(GHQ_2 ~ Turismo + NCGS, data = DB_2, family = "binomial") 
summary(GHQ_T_NCGS)
exp(cbind(OR = coef(GHQ_T_NCGS), confint(GHQ_T_NCGS)))
                OR     2.5 %    97.5 %
(Intercept) 0.2599096 0.1102400 0.6067378
Turismo     1.0770989 0.9870507 1.1756900
NCGS        1.0770164 0.9227650 1.2573340

GHQ_T_NCGSadj <- glm(GHQ_2 ~ Turismo + NCGS + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(GHQ_T_NCGSadj)
exp(cbind(OR = coef(GHQ_T_NCGSadj), confint(GHQ_T_NCGSadj)))
                              OR     2.5 %    97.5 %
(Intercept)              0.4891545 0.1788025 1.3324723
Turismo                  1.0727324 0.9781806 1.1769646
NCGS                     0.9763036 0.8288209 1.1494215
Edad_2                   0.9819271 0.9721222 0.9916695
Estudios21               0.8930723 0.6466170 1.2318604
Nacimiento_2Global South 0.8484323 0.5729958 1.2470677
Laboral1                 1.8465820 1.3375941 2.5566447
Monetaria1               2.5148941 1.8399460 3.4565993

check_model(GHQ_T_NCGSadj)
# Modelos de cambios AB y Turismo y gentrificación
GHQ_AB_TG <- glm(GHQ_2 ~ Cambios_AB + Turismo + NCGS, data = DB_2, family = "binomial") 
summary(GHQ_AB_TG)
exp(cbind(OR = coef(GHQ_AB_TG), confint(GHQ_AB_TG)))
                OR     2.5 %    97.5 %
(Intercept) 0.3854782 0.1407467 1.0466809
Cambios_ABB 0.5928260 0.3742416 0.9455942
Cambios_ABC 1.2275318 0.7425613 2.0414542
Turismo     1.0685382 0.9782746 1.1674571
NCGS        1.0612571 0.9066812 1.2422933

GHQ_AB_TGadj <- glm(GHQ_2 ~ Cambios_AB + Turismo + NCGS + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(GHQ_AB_TGadj)
exp(cbind(OR = coef(GHQ_AB_TGadj), confint(GHQ_AB_TGadj)))
                              OR     2.5 %    97.5 %
(Intercept)              0.9298304 0.2831190 3.0521528
Cambios_ABB              0.5398498 0.3306377 0.8855578
Cambios_ABC              1.2233707 (0.7210630 2.0869021)
Turismo                  1.0584760 (0.9638775 1.1628944)
NCGS                     0.9600138 (0.8119152 1.1343222)
Edad_2                   0.9797287 0.9697241 0.9896531
Estudios21               0.8417270 0.6059762 1.1670222
Nacimiento_2Global South 0.7417179 0.4950126 1.1021266
Laboral1                 1.9053324 1.3731843 2.6522512
Monetaria1               2.5026703 1.8226445 3.4554441

check_model(GHQ_AB_TGadj)

# ESPACIOS DENTRO DE BARCELONA (SUPERILLA, <15 MIN Y >15 MIN)
GHQ_ABC <- glm(GHQ_2 ~ Cambios_ABC, data = DB_2, family = "binomial") 
summary(GHQ_ABC)
exp(cbind(OR = coef(GHQ_ABC), confint(GHQ_ABC)))
                  OR     2.5 %   97.5 %
(Intercept)  0.6250000 0.3745490 1.022934
Cambios_ABCB 0.6571429 (0.3861207 1.136481)
Cambios_ABCC 1.2260870 (0.7145892 2.137511)

GHQ_ABC_adj <- glm(GHQ_2 ~ Cambios_ABC + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(GHQ_ABC_adj)
exp(cbind(OR = coef(GHQ_ABC_adj), confint(GHQ_ABC_adj)))
                              OR     2.5 %   97.5 %
(Intercept)              0.7827195 0.3684273 1.650969
Cambios_ABCB             0.6181040 (0.3507465 1.102932)
Cambios_ABCC             1.1918260 (0.6748786 2.133281)
Edad_2                   0.9810241 0.9711336 0.990844
Estudios21               0.8499794 0.6129088 1.176660
Nacimiento_2Global South 0.7264703 0.4877144 1.072979
Laboral1                 1.8834624 1.3609834 2.614836
Monetaria1               2.5147593 1.8385277 3.458987

#después de ajustar el modelo
GHQ_ABC_adj2 <- glm(GHQ_2 ~ Cambios_ABC + Edad_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(GHQ_ABC_adj2)
exp(cbind(OR = coef(GHQ_ABC_adj2), confint(GHQ_ABC_adj2)))
                  OR     2.5 %    97.5 %
(Intercept)  0.6296952 0.3057999 1.2843105
Cambios_ABCB 0.6899923 (0.3962160 1.2191040)
Cambios_ABCC 1.2666151 (0.7214697 2.2563668)
Edad_2       0.9820009 0.9725833 0.9913363
Laboral1     1.7527803 1.2846633 2.3960429
Monetaria1   2.3718548 1.7473167 3.2357729

# no significativo, nos quedamos con el más sencillo

# ESPACIOS ABC CON TURISMO Y GENTRIFICACIÓN
GHQ_ABC_TG <- glm(GHQ_2 ~ Cambios_ABC + Turismo + NCGS, data = DB_2, family = "binomial") 
summary(GHQ_ABC_TG)
exp(cbind(OR = coef(GHQ_ABC_TG), confint(GHQ_ABC_TG)))
                  OR     2.5 %    97.5 %
(Intercept)  0.3005771 0.1059966 0.8407568
Cambios_ABCB 0.6916221 (0.4044625 1.2020953)
Cambios_ABCC 1.2901741 (0.7486025 2.2601392)
Turismo      1.0794060 (0.9882611 1.1793709)
NCGS         1.0647083 (0.9101927 1.2456031)

GHQ_ABC_TGadj <- glm(GHQ_2 ~ Cambios_ABC + Turismo + NCGS + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(GHQ_ABC_TGadj)
exp(cbind(OR = coef(GHQ_ABC_TGadj), confint(GHQ_ABC_TGadj)))
                              OR     2.5 %    97.5 %
(Intercept)              0.6885756 0.2027934 2.3266732
Cambios_ABCB             0.6298583 (0.3559683 1.1289318)
Cambios_ABCC             1.2134672 (0.6848875 2.1801627)
Turismo                  1.0696566 (0.9743372 1.1748806)
NCGS                     0.9628338 (0.8148273 1.1369289)
Edad_2                   0.9811627 0.9712361 0.9910178
Estudios21               0.8396741 0.6047030 1.1637354
Nacimiento_2Global South 0.7573509 0.5062541 1.1239928
Laboral1                 1.8698849 1.3497857 2.5985120
Monetaria1               2.5202237 1.8374989 3.4760614

# TODOS LOS ESPACIOS
GHQ_ABCD <- glm(GHQ_2 ~ Cambios_ABCD, data = DB_2, family = "binomial") 
summary(GHQ_ABCD)
exp(cbind(OR = coef(GHQ_ABCD), confint(GHQ_ABCD)))
                  OR     2.5 %   97.5 %
(Intercept)   0.6285714 0.3636017 1.063205
Cambios_ABCDB 0.6734848 (0.3824787 1.207733)
Cambios_ABCDC 1.0959596 (0.6232143 1.963762)

GHQ_TODOS_adj <- glm(GHQ_2 ~ Cambios_ABCD + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(GHQ_TODOS_adj)
exp(cbind(OR = coef(GHQ_TODOS_adj), confint(GHQ_TODOS_adj)))
                              OR     2.5 %   97.5 %
(Intercept)              0.7150454 0.3268747 1.549679
Cambios_ABCDB            0.6725237 (0.3692492 1.243788)
Cambios_ABCDC            1.1026186 (0.6089785 2.029251)
Edad_2                   0.9816178 0.9717706 0.991401
Estudios21               0.8810657 0.6370523 1.216854
Nacimiento_2Global South 0.7590545 0.5107914 1.118836
Laboral1                 1.8755001 1.3571617 2.599918
Monetaria1               2.4957142 1.8273023 3.427388

# TODOS LOS ESPACIOS CON TURISMO Y GENTRIFICACIÓN
GHQ_TODOS_TG <- glm(GHQ_2 ~ Cambios_ABCD + Turismo + NCGS, data = DB_2, family = "binomial") 
summary(GHQ_TODOS_TG)
exp(cbind(OR = coef(GHQ_TODOS_TG), confint(GHQ_TODOS_TG)))
                  OR     2.5 %    97.5 %
(Intercept)   0.2927142 0.1024650 0.8239035
Cambios_ABCDB 0.7015908 (0.3969194 1.2631809)
Cambios_ABCDC 1.1538315 (0.6534234 2.0767172)
Turismo       1.0842563 (0.9929490 1.1844205)
NCGS          1.0678752 (0.9135764 1.2483747)

GHQ_TODOS_TGadj <- glm(GHQ_2 ~ Cambios_ABCD + Turismo + NCGS + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(GHQ_TODOS_TGadj)
exp(cbind(OR = coef(GHQ_TODOS_TGadj), confint(GHQ_TODOS_TGadj)))
                              OR     2.5 %    97.5 %
(Intercept)              0.5932619 0.1740777 2.0080324
Cambios_ABCDB            0.6835681 (0.3742054 1.2685240)
Cambios_ABCDC            1.1282500 (0.6211931 2.0840276)
Turismo                  1.0761050 (0.9807140 1.1813968)
NCGS                     0.9683703 (0.8205873 1.1420557)
Edad_2                   0.9817805 0.9718960 0.9916002
Estudios21               0.8697244 0.6280168 1.2026245
Nacimiento_2Global South 0.7925890 0.5311449 1.1737304
Laboral1                 1.8577373 1.3428777 2.5778499
Monetaria1               2.4975602 1.8237957 3.4390530

###############################################################################
############# MODELOS PARA SALUD GENERAL #####################################
##############################################################################

# Para esta variable de salud, el análisis bivariado mostraba relación con todos
# los espacios excepto los de alrededor de la ciudad (espacios D). 
# el análisis también indicaba que al agrupar (AB y CD), se mantenía la relación, 
# la relación entre el uso de TODOS los espacios también es significativa

# SÓLO ESPACIOS CERCANOS (SUPERILLA/PLAYA Y <15 MIN = ESPACIOS A Y B)
SG_AB <- glm(SaludG ~ Cambios_AB, data = DB_2, family = "binomial")
summary(SG_AB)
exp(cbind(OR = coef(SG_AB), confint(SG_AB)))
                OR     2.5 %    97.5 %
(Intercept) 0.9583333 0.6383107 1.4371247
Cambios_ABB 0.4017146 0.2577696 0.6261610
Cambios_ABC 0.5469265 0.3343090 0.8931938

SG_AB_adj <- glm(SaludG ~ Cambios_AB + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial")
summary(SG_AB_adj)
exp(cbind(OR = coef(SG_AB_adj), confint(SG_AB_adj)))
                            OR     2.5 %    97.5 %
(Intercept)              0.2200868 0.1108236 0.4317668
Cambios_ABB              0.4609488 0.2883499 0.7375393
Cambios_ABC              0.5738091 0.3421355 0.9609109
Edad_2                   1.0167997 1.0069173 1.0269407
Estudios21               0.8784814 0.6314252 1.2203198
Nacimiento_2Global South 0.8441635 0.5643195 1.2501559
Laboral1                 1.8998267 1.3663753 2.6468912
Monetaria1               1.8324375 1.3395066 2.5172689

# Estudios, barrio y NCGS no salen significativos, reajustamos el modelo
#SG_AB_adj2 <- glm(SaludG ~ Cambios_AB + Laboral + Monetaria + Barrio + Turismo + NCGS, data = DB_2, family = "binomial")3
#summary(SG_AB_adj2)
#exp(cbind(OR = coef(SG_AB_adj2), confint(SG_AB_adj2)))
#                      OR      2.5 %    97.5 %
#(Intercept)       0.2375437 0.08260294 0.6747173
#Cambios_ABB       0.4822200 0.29841208 0.7798914
#Cambios_ABC       0.5824127 0.34566760 0.9797964
#Laboral1          2.1419009 1.59077105 2.8926282
#Monetaria1        1.6818692 1.23809744 2.2923525
#BarrioSant Antoni 0.8847379 0.65056111 1.2047206
#Turismo           1.1173303 1.02043147 1.2243805
#NCGS              1.0220440 0.86747674 1.2040497

# Barrio sigue sin ser significativo, hay que reajustar
#SG_AB_adj3 <- glm(SaludG ~ Cambios_AB + Laboral + Monetaria + Turismo + NCGS, data = DB_2, family = "binomial")
#summary(SG_AB_adj3)
#exp(cbind(OR = coef(SG_AB_adj3), confint(SG_AB_adj3)))

# ESPACIOS DENTRO DE BARCELONA (SUPERILLA, <15 MIN Y >15 MIN)
SG_ABC <- glm(SaludG ~ Cambios_ABC, data = DB_2, family = "binomial") 
summary(SG_ABC)
exp(cbind(OR = coef(SG_ABC), confint(SG_ABC)))
                  OR     2.5 %    97.5 %
(Intercept)  1.0606061 0.6584512 1.7122000
Cambios_ABCB 0.3766087 0.2245214 0.6300215
Cambios_ABCC 0.4451257 0.2620006 0.7540221

#ajustar el modelo
SG_ABC_adj <- glm(SaludG ~ Cambios_ABC + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria + Barrio, data = DB_2, family = "binomial") 
summary(SG_ABC_adj)
exp(cbind(OR = coef(SG_ABC_adj), confint(SG_ABC_adj)))
                              OR     2.5 %    97.5 %
(Intercept)              0.2482998 0.1181040 0.5168505
Cambios_ABCB             0.4919215 0.2807007 0.8611653
Cambios_ABCC             0.5288305 0.3016843 0.9254759
Edad_2                   1.0174753 1.0075638 1.0276534
Estudios21               0.8434228 0.6029878 1.1775197
Nacimiento_2Global South 0.8487596 0.5663860 1.2591999
Laboral1                 1.8763235 1.3499209 2.6128491
Monetaria1               1.8390460 1.3442415 2.5265532
BarrioSant Antoni        0.7743732 0.5668753 1.0576713

#reajustamos por barrio, estudios y NCGS
# barrio fuera
SG_ABC_adj2 <- glm(SaludG ~ Cambios_ABC + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(SG_ABC_adj2)
exp(cbind(OR = coef(SG_ABC_adj2), confint(SG_ABC_adj2)))
                              OR     2.5 %    97.5 %
(Intercept)              0.2347844 0.1120833 0.4866663
Cambios_ABCB             0.4404349 0.2554564 0.7581850
Cambios_ABCC             0.4907790 0.2820960 0.8520748
Edad_2                   1.0169853 1.0071221 1.0271084
Estudios21               0.8824364 0.6342548 1.2259121
Nacimiento_2Global South 0.8481367 0.5663281 1.2574642
Laboral1                 1.8735243 1.3488755 2.6070626
Monetaria1               1.8613367 1.3614322 2.5557982
lrtest(SG_ABC_adj2, SG_ABC_adj) #no significativo, nos quedamos con el más sencillo

###### Ahora hacemos los modelos para TODOS LOS ESPACIOS
SG_TODOS <- glm(SaludG ~ Cambios_ABCD, data = DB_2, family = "binomial")
summary(SG_TODOS)
exp(cbind(OR = coef(SG_TODOS), confint(SG_TODOS)))
                   OR     2.5 %    97.5 %
(Intercept)   0.9354839 0.5614731 1.5541635
Cambios_ABCDB 0.4282606 0.2475916 0.7419751
Cambios_ABCDC 0.5127879 0.2961106 0.8895544


SG_TODOS_adj <- glm(SaludG ~ Cambios_ABCD + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial")
summary(SG_TODOS_adj)
exp(cbind(OR = coef(SG_TODOS_adj), confint(SG_TODOS_adj)))
                            OR      2.5 %    97.5 %
(Intercept)              0.2062724 0.09578725 0.4388390
Cambios_ABCDB            0.4924091 0.27715490 0.8761587
Cambios_ABCDC            0.5646618 0.31848118 1.0027178
Edad_2                   1.0172135 1.00736671 1.0273190
Estudios21               0.8906520 0.64116296 1.2355142
Nacimiento_2Global South 0.8518456 0.56939453 1.2615459
Laboral1                 1.8701218 1.34736917 2.6004717
Monetaria1               1.8552380 1.35765763 2.5460661
# En el test de Wald sí es significativa Cambios_ABCDC

# Modelo con turismo y gentrificación
SG_T_NCGS <- glm(SaludG ~ Turismo + NCGS, data = DB_2, family = "binomial") 
summary(SG_T_NCGS)
exp(cbind(OR = coef(SG_T_NCGS), confint(SG_T_NCGS)))
                OR      2.5 %   97.5 %
(Intercept) 0.1520728 0.06377999 0.357733
Turismo     1.1393020 1.04397689 1.244256
NCGS        1.1000869 0.94120397 1.286111

SG_T_NCGSadj <- glm(SaludG ~ Turismo + NCGS + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(SG_T_NCGSadj)
exp(cbind(OR = coef(SG_T_NCGSadj), confint(SG_T_NCGSadj)))
                                OR      2.5 %    97.5 %
(Intercept)              0.03852453 0.01336708 0.1080523
Turismo                  1.13221929 1.03312857 1.2416060
NCGS                     1.08108212 0.91730938 1.2744123
Edad_2                   1.01921853 1.00931166 1.0294083
Estudios21               0.89039103 0.64013545 1.2367419
Nacimiento_2Global South 0.96813490 0.64795067 1.4339096
Laboral1                 1.80800622 1.30092834 2.5165287
Monetaria1               1.83946893 1.34352215 2.5289497

# Espacios cercanos (superilla/playa y <15 min = A y B) con turismo y NCGS
SG_AB_TG <- glm(SaludG ~ Cambios_AB + Turismo + NCGS, data = DB_2, family = "binomial") 
summary(SG_AB_TG)
exp(cbind(OR = coef(SG_AB_TG), confint(SG_AB_TG)))
                OR     2.5 %    97.5 %
(Intercept) 0.3666154 0.1352704 0.9866222
Cambios_ABB 0.4274651 0.2726101 0.6707589
Cambios_ABC 0.5709767 0.3470878 0.9380213
Turismo     1.1308801 1.0355552 1.2359454
NCGS        1.0664559 0.9105725 1.2490522

SG_AB_TGadj <- glm(SaludG ~ Cambios_AB + Turismo + NCGS + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(SG_AB_TGadj)
exp(cbind(OR = coef(SG_AB_TGadj), confint(SG_AB_TGadj)))
                              OR      2.5 %    97.5 %
(Intercept)              0.09013378 0.02709104 0.2938122
Cambios_ABB              0.49347387 0.30700241 0.7943295
Cambios_ABC              0.59939365 0.35593949 1.0083458
Turismo                  1.12307803 1.02435775 1.2321411
NCGS                     1.05247319 0.89136356 1.2427435
Edad_2                   1.01781093 1.00783242 1.0280653
Estudios21               0.86339955 0.61895234 1.2022941
Nacimiento_2Global South 0.90104735 0.59888611 1.3430762
Laboral1                 1.86260743 1.33688700 2.5997628
Monetaria1               1.80631655 1.31700377 2.4873054

# Espacios dentro de Barcelona (superilla/playa, <15 min, >15 min = A, B Y C) con turismo y NCGS
SG_ABC_TG <- glm(SaludG ~ Cambios_ABC + Turismo + NCGS, data = DB_2, family = "binomial") 
summary(SG_ABC_TG)
exp(cbind(OR = coef(SG_ABC_TG), confint(SG_ABC_TG)))
                OR     2.5 %    97.5 %
(Intercept)  0.3863403 0.1389129 1.0683166
Cambios_ABCB 0.3990490 0.2364165 0.6721253
Cambios_ABCC 0.4687630 0.2744010 0.7988734
Turismo      1.1349681 1.0393677 1.2403273
NCGS         1.0728193 0.9164625 1.2558722

SG_ABC_TGadj <- glm(SaludG ~ Cambios_ABC + Turismo + NCGS + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(SG_ABC_TGadj)
exp(cbind(OR = coef(SG_ABC_TGadj), confint(SG_ABC_TGadj)))
                            OR      2.5 %    97.5 %
(Intercept)              0.09178414 0.02687342 0.3070244
Cambios_ABCB             0.47178252 0.27218178 0.8167597
Cambios_ABCC             0.52001644 0.29772748 0.9067224
Turismo                  1.12573600 1.02676616 1.2350836
NCGS                     1.05920002 0.89783354 1.2496643
Edad_2                   1.01799551 1.00803336 1.0282349
Estudios21               0.86535547 0.62021645 1.2053141
Nacimiento_2Global South 0.90465547 0.60064168 1.3498328
Laboral1                 1.83768648 1.32031559 2.5619948
Monetaria1               1.82955361 1.33444601 2.5186340

# Todos los espacios con turismo y NCGS
SG_TODOS_TG <- glm(SaludG ~ Cambios_ABCD + Turismo + NCGS, data = DB_2, family = "binomial") 
summary(SG_TODOS_TG)
exp(cbind(OR = coef(SG_TODOS_TG), confint(SG_TODOS_TG)))
                  OR     2.5 %    97.5 %
(Intercept)   0.3217801 0.1146947 0.8950417
Cambios_ABCDB 0.4473792 0.2571135 0.7799844
Cambios_ABCDC 0.5423609 0.3114158 0.9467178
Turismo       1.1390282 1.0432540 1.2445946
NCGS          1.0819637 0.9248287 1.2659433

SG_TODOS_TGadj <- glm(SaludG ~ Cambios_ABCD + Turismo + NCGS + Edad_2 + Estudios2 + Nacimiento_2 + Laboral + Monetaria, data = DB_2, family = "binomial") 
summary(SG_TODOS_TGadj)
exp(cbind(OR = coef(SG_TODOS_TGadj), confint(SG_TODOS_TGadj)))
                                OR      2.5 %    97.5 %
(Intercept)              0.07708324 0.02241555 0.2592850
Cambios_ABCDB            0.51933898 0.29100113 0.9283039
Cambios_ABCDC            0.59884003 0.33641256 1.0678879
Turismo                  1.12940028 1.03027836 1.2389127
NCGS                     1.06700226 0.90488703 1.2583790
Edad_2                   1.01824124 1.00829280 1.0284661
Estudios21               0.87205569 0.62587653 1.2130758
Nacimiento_2Global South 0.90663628 0.60255472 1.3513230
Laboral1                 1.83255295 1.31737803 2.5533364
Monetaria1               1.82074604 1.32865546 2.5052023

########### MODELOS PARA LA CALIDAD DEL SUEÑO ###############
#* Como la calidad del sueño es una escala, hay que utilizar un modelo de regresión lineal
#* La única variable de espacio público que tenía una relación significativa en el análisis bivariado
#* era la de Tiempo en el espacio A (superilla/playa), por lo que haremos los modelos con ella

# Modelo Tiemmpo A y calidad del sueño
Dormir_TiemA <- lm(Dormir ~ as.factor(Tiempo_A), data = DB_2) 
summary(Dormir_TiemA)
confint(Dormir_TiemA)
               2.5 %    97.5 %
(Intercept) 5.777598364 6.4162792
Tiempo_A1   0.001285492 0.7265504

Dormir_TiemAadj <- lm(Dormir ~ as.factor(Tiempo_A) + Edad_2 + as.factor(Monetaria) + as.factor(Barrio) + as.factor(Estudios2) + as.factor(Nacimiento_2) + as.factor(Laboral), data = DB_2) 
summary(Dormir_TiemAadj)
confint(Dormir_TiemAadj)
                                        2.5 %       97.5 %
(Intercept)                          5.63875029  6.862734761
as.factor(Tiempo_A)1                -0.26019810  0.494278946
Edad_2                              -0.01113285  0.009442169
as.factor(Monetaria)1               -1.06061088 -0.434972117
as.factor(Barrio)Sant Antoni         0.39447730  1.031469979
as.factor(Estudios2)1               -0.34801907  0.310888290
as.factor(Nacimiento_2)Global South  0.15505061  0.947099206
as.factor(Laboral)1                 -0.32569564  0.344007571


# Modelos para los espacios cercanos (Superilla/playa y <15 min = A y B)
Dormir_AB <- lm(Dormir ~ as.factor(Cambios_AB), data = DB_2) 
summary(Dormir_AB)
confint(Dormir_AB)
                                  2.5 %   97.5 %
(Intercept)                     5.3550972 6.279311
as.factor(Cambios_AB)B 0.6259   0.1286669 1.123172
as.factor(Cambios_AB)C 0.6534   0.1025619 1.204206

Dormir_ABadj <- lm(Dormir ~ as.factor(Cambios_AB) + Edad_2 + as.factor(Monetaria) + as.factor(Barrio) + as.factor(Estudios2) + as.factor(Nacimiento_2) + as.factor(Laboral), data = DB_2) 
summary(Dormir_ABadj)
confint(Dormir_ABadj)
                                          2.5 %      97.5 %
(Intercept)                          5.224818006  6.61096364
as.factor(Cambios_AB)B 0.310190     -0.203432694  0.82381288
as.factor(Cambios_AB)C 0.443018     -0.108512691  0.99454879
Edad_2                              -0.008284493  0.01153592
as.factor(Monetaria)1               -1.047013705 -0.43315571
as.factor(Barrio)Sant Antoni         0.370572202  0.99209144
as.factor(Estudios2)1               -0.320367405  0.33231684
as.factor(Nacimiento_2)Global South  0.142792286  0.92610512
as.factor(Laboral)1                 -0.278117783  0.37888687

# Modelos para los espacios centro de la ciudad (Superilla/playa, <15 min y >15 min = A, B y C)
Dormir_ABC <- lm(Dormir ~ as.factor(Cambios_ABC), data = DB_2) 
summary(Dormir_ABC)
confint(Dormir_ABC)
                            2.5 %   97.5 %
(Intercept)             5.25326575 6.334970
as.factor(Cambios_ABC)B 0.6582 0.08126747 1.235163
as.factor(Cambios_ABC)C 0.6123 0.02013034 1.204500

Dormir_ABCadj <- lm(Dormir ~ as.factor(Cambios_ABC) + Edad_2 + as.factor(Monetaria) + as.factor(Barrio) + as.factor(Estudios2) + as.factor(Nacimiento_2) + as.factor(Laboral), data = DB_2) 
summary(Dormir_ABCadj)
confint(Dormir_ABCadj)
                                          2.5 %      97.5 %
(Intercept)                          5.095652921  6.60767609
as.factor(Cambios_ABC)B   0.362888  -0.230737525  0.95651376
as.factor(Cambios_ABC)C   0.430567  -0.162958153  1.02409246
Edad_2                              -0.008048496  0.01175556
as.factor(Monetaria)1               -1.052065597 -0.43875403
as.factor(Barrio)Sant Antoni         0.376614538  0.99618268
as.factor(Estudios2)1               -0.318694049  0.33456007
as.factor(Nacimiento_2)Global South  0.147788320  0.93304291
as.factor(Laboral)1                 -0.271761675  0.38494835

# Modelo con el uso de todos los espacios (ya sabemos que no es significativo)
Dormir_ABCD <- lm(Dormir ~ as.factor(Cambios_ABCD), data = DB_2) 
summary(Dormir_ABCD)
confint(Dormir_ABCD)
                                      2.5 %   97.5 %
(Intercept)                      5.257157290 6.409509
as.factor(Cambios_ABCD)B 0.6046 -0.009366193 1.218546
as.factor(Cambios_ABCD)C 0.5767 -0.041213303 1.194547

Dormir_ABCDadj <- lm(Dormir ~ as.factor(Cambios_ABCD) + Edad_2 + as.factor(Monetaria) + as.factor(Barrio) + as.factor(Estudios2) + as.factor(Nacimiento_2) + as.factor(Laboral), data = DB_2) 
summary(Dormir_ABCDadj)
confint(Dormir_ABCDadj)
                                          2.5 %     97.5 %
(Intercept)                          5.150020956  6.7179678
as.factor(Cambios_ABCD)B  0.262879  -0.365556981  0.8913155
as.factor(Cambios_ABCD)C  0.336913  -0.282512916  0.9563383
Edad_2                              -0.008167706  0.0116440
as.factor(Monetaria)1               -1.053172752 -0.4393739
as.factor(Barrio)Sant Antoni         0.388348334  1.0071595
as.factor(Estudios2)1               -0.315598495  0.3372089
as.factor(Nacimiento_2)Global South  0.146046737  0.9313232
as.factor(Laboral)1                 -0.273302244  0.3838566

# MODELO SÓLO CON TURISMO Y GENTRIFICACIÓN
Dormir_T_NCGS <- lm(Dormir ~ Turismo + NCGS, data = DB_2) 
summary(Dormir_T_NCGS)
confint(Dormir_T_NCGS)
                      2.5 %      97.5 %
(Intercept)         7.8229834  9.59336609
Turismo   -0.14051  -0.2320155 -0.04899772
NCGS      -0.31658  -0.4786895 -0.15446753

Dormir_T_NCGSadj <- lm(Dormir ~ Turismo + NCGS + Edad_2 + as.factor(Estudios2) + as.factor(Nacimiento_2) + as.factor(Monetaria) + as.factor(Laboral) + as.factor(Barrio), data = DB_2) 
summary(Dormir_T_NCGSadj)
confint(Dormir_T_NCGSadj)
                                        2.5 %       97.5 %
(Intercept)                          7.16470965  9.234209687
Turismo    -0.09625               -0.18821025 -0.004282591
NCGS        -0.2767               -0.43875224 -0.114565318
Edad_2                              -0.00977121  0.009769800
as.factor(Estudios2)1               -0.32670936  0.320333357
as.factor(Nacimiento_2)Global South  0.11365408  0.891659965
as.factor(Monetaria)1               -0.99593693 -0.385887781
as.factor(Laboral)1                 -0.22588299  0.425496902
as.factor(Barrio)Sant Antoni         0.33062837  0.931345580

Dormir_T_NCGSadj2 <- lm(Dormir ~ Turismo + NCGS + Edad_2 + as.factor(Estudios2) + as.factor(Nacimiento_2) + as.factor(Monetaria) + as.factor(Barrio), data = DB_2) 
summary(Dormir_T_NCGSadj2)
confint(Dormir_T_NCGSadj2)
                                        2.5 %       97.5 %
(Intercept)                          7.146619718  9.196475866
Turismo            -0.0972          -0.188669145 -0.005841288
NCGS               -0.2731          -0.434396051 -0.111995628
Edad_2                              -0.008408997  0.010157628
as.factor(Estudios2)1               -0.301292779  0.327437825
as.factor(Nacimiento_2)Global South  0.142444237  0.909270170
as.factor(Monetaria)1               -0.976454183 -0.374289345
as.factor(Barrio)Sant Antoni         0.326577007  0.923883643

anova(Dormir_T_NCGSadj2, Dormir_T_NCGSadj)

# MODELOS CON USO DEL ESPACIO PÚBLICO + GENTRIFICACIÓN
Dormir_AB_TG <- lm(Dormir ~ as.factor(Cambios_AB) + Turismo + NCGS, data = DB_2) 
summary(Dormir_AB_TG)
confint(Dormir_AB_TG)
                                     2.5 %      97.5 %
(Intercept)                     7.13633884  9.21878574
as.factor(Cambios_AB)B 0.45630 -0.03850911  0.95110762
as.factor(Cambios_AB)C 0.51879 -0.02724105  1.06481909
Turismo               -0.13757 -0.22911036 -0.04602637
NCGS                  -0.29916 -0.46215646 -0.13616035

Dormir_AB_TGadj <- lm(Dormir ~ as.factor(Cambios_AB) + Turismo + NCGS + Edad_2 + as.factor(Estudios2) + as.factor(Nacimiento_2) + as.factor(Monetaria) + as.factor(Barrio), data = DB_2) 
summary(Dormir_AB_TGadj)
confint(Dormir_AB_TGadj)
                                        2.5 %       97.5 %
(Intercept)                          6.762711796  9.086620396
as.factor(Cambios_AB)B   0.1995243  -0.309814642  0.708863274
as.factor(Cambios_AB)C   0.3558478  -0.190390728  0.902086368
Turismo                 -0.0973430  -0.188838797 -0.005847228
NCGS                    -0.2660667  -0.427954844 -0.104178517
Edad_2                              -0.008380091  0.010307583
as.factor(Estudios2)1               -0.307491463  0.321654303
as.factor(Nacimiento_2)Global South  0.137379520  0.910486628
as.factor(Monetaria)1               -0.972674000 -0.369981464
as.factor(Barrio)Sant Antoni         0.297714383  0.914468778


Dormir_ABC_TG <- lm(Dormir ~ as.factor(Cambios_ABC) + Turismo + NCGS, data = DB_2) 
summary(Dormir_ABC_TG)
confint(Dormir_ABC_TG)
                                    2.5 %      97.5 %
(Intercept)                      7.1024775  9.24635984
as.factor(Cambios_ABC)B 0.49898 -0.0731055  1.07105678
as.factor(Cambios_ABC)C 0.47790 -0.1081263  1.06391705
Turismo                -0.13761 -0.2291254 -0.04608865
NCGS                   -0.30395 -0.4666769 -0.14122834

Dormir_ABC_TGadj <- lm(Dormir ~ as.factor(Cambios_ABC) + Turismo + NCGS + Edad_2 + as.factor(Estudios2) + as.factor(Nacimiento_2) + as.factor(Monetaria) + as.factor(Barrio), data = DB_2) 
summary(Dormir_ABC_TGadj)
confint(Dormir_ABC_TGadj)
                                        2.5 %      97.5 %
(Intercept)                          6.660792023  9.05409922
as.factor(Cambios_ABC)B   0.263592 -0.324435701  0.85161957
as.factor(Cambios_ABC)C   0.338321  -0.249408346  0.92605042
Turismo                  -0.095759   -0.187245207 -0.00427269
NCGS                     -0.268274  -0.429937998 -0.10660945
Edad_2                              -0.008059202  0.01062435
as.factor(Estudios2)1               -0.305576570  0.32417226
as.factor(Nacimiento_2)Global South  0.146520500  0.92133639
as.factor(Monetaria)1               -0.974396518 -0.37192713
as.factor(Barrio)Sant Antoni         0.297691104  0.91334405

Dormir_TODOS_TG <- lm(Dormir ~ as.factor(Cambios_ABCD) + Turismo + NCGS, data = DB_2) 
summary(Dormir_TODOS_TG)
confint(Dormir_TODOS_TG)
                            2.5 %      97.5 %
(Intercept)               7.1365262  9.29978001
as.factor(Cambios_ABCD)B 0.47783 -0.1292729  1.08493466
as.factor(Cambios_ABCD)C 0.44827 -0.1624997  1.05904533
Turismo                  -0.13881 -0.2303967 -0.04723256
NCGS                     -0.30730 -0.4698303 -0.14477368

Dormir_TODOS_TGadj <- lm(Dormir ~ as.factor(Cambios_ABCD) + Turismo + NCGS + Edad_2 + as.factor(Estudios2) + as.factor(Nacimiento_2) + as.factor(Monetaria) + as.factor(Barrio), data = DB_2) 
summary(Dormir_TODOS_TGadj)
confint(Dormir_TODOS_TGadj)
                                          2.5 %       97.5 %
(Intercept)                          6.72196541  9.129795373
as.factor(Cambios_ABCD)B  0.201168  -0.41995498  0.822290376
as.factor(Cambios_ABCD)C  0.262724  -0.34988951  0.875338143
Turismo                  -0.095932  -0.18750003 -0.004364368
NCGS                     -0.270471  -0.43199079 -0.108952014
Edad_2                              -0.00812683  0.010552458
as.factor(Estudios2)1               -0.30235774  0.326885284
as.factor(Nacimiento_2)Global South  0.14838205  0.922833428
as.factor(Monetaria)1               -0.97472181 -0.371906164
as.factor(Barrio)Sant Antoni         0.30259706  0.918100524
as.factor(Laboral)1                 -0.230200737  0.421904517
as.factor(Barrio)Sant Antoni         0.307490461  0.926575329
> 
