# NUEVOS MODELOS INCLUYENDO EL CAMBIO EN LA GENTRIFICACIÓN Y LA TURISTIFICACIÓN
DB_2 <- DB_1

# MODELOS PARA SALUD GENERAL

SG_Dif_adj <- glm(Ahora_salud2 ~ Dif_NCGS + Dif_Turismo + Edad + Estudios2 + Nacimiento_2 + Ahora_laboral2 + Ahora_monetaria2, data = DB_2, family = "binomial") 
summary(SG_Dif_adj)
exp(cbind(OR = coef(SG_Dif_adj), confint(SG_Dif_adj)))
                              OR      2.5 %    97.5 %
(Intercept)              0.1350562 0.07263661 0.2455759
Dif_NCGS                 1.0620346 0.90862565 1.2421159
Dif_Turismo              1.0721633 1.01121941 1.1369143**
Edad                     1.0169241 1.00727675 1.0269141
Estudios2Universitarios  1.1182023 0.80555305 1.5544641
Nacimiento_2Global South 0.9214810 0.61923668 1.3585961
Ahora_laboral21          1.8722138 1.34856085 2.6037072
Ahora_monetaria21        1.8703859 1.36859705 2.5671767

SG_Dif2_adj <- glm(Ahora_salud2 ~ relevel(Dif_NCGS_2, ref = "No change") + relevel(Dif_Turismo_2, ref = "No change") + Edad + Estudios2 + Nacimiento_2 + Ahora_laboral2 + Ahora_monetaria2, data = DB_2, family = "binomial") 
summary(SG_Dif2_adj)
exp(cbind(OR = coef(SG_Dif2_adj), confint(SG_Dif2_adj)))
# antes de relevel
                            OR      2.5 %    97.5 %
(Intercept)              0.09257162 0.05023352 0.1665931
Dif_NCGS_2No change      0.87637289 0.37956781 1.8969735
Dif_NCGS_2Increased      1.13172830 0.81760552 1.5611605
Dif_Turismo_2No change   1.60411387 0.88097242 2.8868285
Dif_Turismo_2Increased   1.58872370 0.87270568 2.8488080
Edad                     1.01724911 1.00759453 1.0272581
Estudios2Universitarios  1.10105230 0.79390855 1.5289821
Nacimiento_2Global South 0.91882917 0.61745837 1.3546528
Ahora_laboral21          1.84522965 1.32755343 2.5689611
Ahora_monetaria21        1.86307461 1.36265727 2.5582929

# después de relevel
                                                        OR      2.5 %    97.5 %
(Intercept)                                        0.1301374 (0.04070429 0.3927061)
relevel(Dif_NCGS_2, ref = "No change")Reduced      1.1410668 (0.52715550 2.6345753)
relevel(Dif_NCGS_2, ref = "No change")Increased    1.2913776 (0.58261981 3.0415611)
relevel(Dif_Turismo_2, ref = "No change")Reduced   0.6233971 (0.34640090 1.1351093)
relevel(Dif_Turismo_2, ref = "No change")Increased 0.9904058 (0.44403751 2.2072608)
Edad                                               1.0172491 1.00759453 1.0272581
Estudios2Universitarios                            1.1010523 0.79390855 1.5289821
Nacimiento_2Global South                           0.9188292 0.61745837 1.3546528
Ahora_laboral21                                    1.8452296 1.32755343 2.5689611
Ahora_monetaria21                                  1.8630746 1.36265727 2.5582929


# MODELOS PARA SALUD MENTAL

GHQ_Dif_adj <- glm(Ahora_GHQ2 ~ Dif_NCGS + Dif_Turismo + Edad + Estudios2 + Nacimiento_2 + Ahora_laboral2 + Ahora_monetaria2, data = DB_2, family = "binomial") 
summary(GHQ_Dif_adj)
exp(cbind(OR = coef(GHQ_Dif_adj), confint(GHQ_Dif_adj)))
                              OR     2.5 %    97.5 %
(Intercept)              0.4686506 0.2610530 0.8390685
Dif_NCGS                 0.9038999 (0.7710853 1.0578379)
Dif_Turismo              1.0209314 (0.9618716 1.0833815)
Edad                     0.9847075 0.9751556 0.9940963
Estudios2Universitarios  1.1209182 0.8113961 1.5504509
Nacimiento_2Global South 0.8405324 0.5699427 1.2299928
Ahora_laboral21          1.8725817 1.3555354 2.5950457
Ahora_monetaria21        2.5381990 1.8582738 3.4867411

GHQ_Dif2_adj <- glm(Ahora_GHQ2 ~ relevel(Dif_NCGS_2, ref="No change") + relevel(Dif_Turismo_2, ref="No change") + Edad + Estudios2 + Nacimiento_2 + Ahora_laboral2 + Ahora_monetaria2, data = DB_2, family = "binomial") 
summary(GHQ_Dif2_adj)
exp(cbind(OR = coef(GHQ_Dif2_adj), confint(GHQ_Dif2_adj)))
#antes de relevel
                            OR     2.5 %    97.5 %
(Intercept)              0.4568164 0.2593408 0.8023299
Dif_NCGS_2No change      0.8410442 0.3628592 1.8352032
Dif_NCGS_2Increased      0.9254931 0.6664063 1.2801918
Dif_Turismo_2No change   1.2214415 0.6634114 2.2100126
Dif_Turismo_2Increased   0.9775028 0.5143680 1.8081163
Edad                     0.9848261 0.9752568 0.9942363
Estudios2Universitarios  1.1325642 0.8202058 1.5660131
Nacimiento_2Global South 0.8506867 0.5765718 1.2455869
Ahora_laboral21          1.8677329 1.3502400 2.5916666
Ahora_monetaria21        2.5334582 1.8543803 3.4811450

# después de relevel
                                                        OR     2.5 %    97.5 %
(Intercept)                                        0.4692812 0.1491086 1.4250723
relevel(Dif_NCGS_2, ref = "No change")Reduced      1.1889982 (0.5448988 2.7558902)
relevel(Dif_NCGS_2, ref = "No change")Increased    1.1004095 (0.4912561 2.6077162)
relevel(Dif_Turismo_2, ref = "No change")Reduced   0.8187048 (0.4524861 1.5073603)
relevel(Dif_Turismo_2, ref = "No change")Increased 0.8002862 (0.3459379 1.8396145)
Edad                                               0.9848261 0.9752568 0.9942363
Estudios2Universitarios                            1.1325642 0.8202058 1.5660131
Nacimiento_2Global South                           0.8506867 0.5765718 1.2455869
Ahora_laboral21                                    1.8677329 1.3502400 2.5916666
Ahora_monetaria21                                  2.5334582 1.8543803 3.4811450

# MODELOS PARA CALIDAD DEL SUEÑO
SQ_Dif_adj <- lm(Ahora_dormir ~ Dif_NCGS + Dif_Turismo + Edad + as.factor(Estudios2) + as.factor(Nacimiento_2) + as.factor(Ahora_laboral2) + as.factor(Ahora_monetaria2), data = DB_2, family = "binomial") 
summary(SQ_Dif_adj)
confint(SQ_Dif_adj)
                                                     2.5 %      97.5 %
(Intercept)                                       5.837585003  6.99551082
Dif_NCGS                              -0.106030  -0.262683013  0.05062331
Dif_Turismo                           -0.016608  -0.076304158  0.04308770
Edad                                             -0.006461013  0.01268221
as.factor(Estudios2)Universitarios               -0.219355301  0.42809285
as.factor(Nacimiento_2)Global South               0.093895352  0.87634130
as.factor(Ahora_laboral2)1                       -0.269504315  0.39056422
as.factor(Ahora_monetaria2)1                     -1.110014976 -0.49177816

SQ_Dif2_adj <- lm(Ahora_dormir ~ as.factor(Dif_NCGS_2) + as.factor(Dif_Turismo_2) + Edad + Estudios2 + Nacimiento_2 + Ahora_laboral2 + Ahora_monetaria2, data = DB_2, family = "binomial") 
summary(SQ_Dif2_adj)
confint(SQ_Dif2_adj)
                                                 2.5 %        97.5 %
(Intercept)                                   6.072692112  7.1961469139
as.factor(Dif_NCGS_2)No change  -0.247635    -1.036966356  0.5416965658
as.factor(Dif_NCGS_2)Increased  -0.326433    -0.653160489  0.0002946987
as.factor(Dif_Turismo_2)No change -0.102892  -0.721965713  0.5161816987
as.factor(Dif_Turismo_2)Increased 0.008529   -0.612859990  0.6299173656
Edad                                         -0.006458891  0.0127336982
Estudios2Universitarios                      -0.211444972  0.4351650872
Nacimiento_2Global South                      0.096283386  0.8794018003
Ahora_laboral21                              -0.256580446  0.4075326664
Ahora_monetaria21                            -1.122623192 -0.5033945395
