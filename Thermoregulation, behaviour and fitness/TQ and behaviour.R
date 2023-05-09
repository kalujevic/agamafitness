


main <- read.csv(file="Main spreadsheet_seasons_NEW.csv", header=TRUE, sep=",")


View(main)


m1 <- lme(Sum_Thermoregulation ~ Mb*Season*Tb_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal residuals

summary(m1)
#Fixed effects: Sum_Thermoregulation ~ Mb * Season * Tb_average 
#Value Std.Error DF   t-value p-value
#(Intercept)                  7.112883  4.217122 14  1.686667  0.1138
#Mb                          -0.240695  0.187969 14 -1.280503  0.2212
#Seasonsummer               -13.720227 10.661384  6 -1.286909  0.2455
#Tb_average                  -0.162400  0.112348  6 -1.445512  0.1984
#Mb:Seasonsummer              0.496484  0.468938  6  1.058742  0.3305
#Mb:Tb_average                0.006305  0.005018  6  1.256455  0.2557
#Seasonsummer:Tb_average      0.366895  0.288837  6  1.270252  0.2510
#Mb:Seasonsummer:Tb_average  -0.013375  0.012703  6 -1.052888  0.3329



m2 <- lme(Sum_Thermoregulation ~ Mb+Season*Tb_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)
#Fixed effects: Sum_activity ~ Mb + Season * Tb_average 
#Value Std.Error DF   t-value p-value
#(Intercept)              1.7972905 0.5075948 14  3.540798  0.0033
#Mb                      -0.0043663 0.0043682 14 -0.999574  0.3345
#Seasonsummer            -2.7277662 0.9895598  9 -2.756545  0.0222
#Tb_average              -0.0207476 0.0130470  9 -1.590217  0.1462
#Seasonsummer:Tb_average  0.0708865 0.0267105  9  2.653886  0.0263


anova(m1,m2)
#Model df       AIC       BIC   logLik   Test  L.Ratio p-value
#m1     1 10 -64.92273 -51.60068 42.46136                        
#m2     2  7 -69.46679 -60.14136 41.73340 1 vs 2 1.455934  0.6925

m3 <- lme(Sum_Thermoregulation ~ Season*Tb_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#Value Std.Error DF   t-value p-value
#(Intercept)              1.6214196 0.4761171 15  3.405506  0.0039
#Seasonsummer            -2.6114326 0.9826746  9 -2.657474  0.0262
#Tb_average              -0.0186528 0.0128774  9 -1.448492  0.1814
#Seasonsummer:Tb_average  0.0676836 0.0265171  9  2.552450  0.0311

anova(m2,m3)
#Model df       AIC       BIC   logLik   Test  L.Ratio p-value
#m2     1  7 -69.46679 -60.14136 41.73340                        
#m3     2  6 -70.27612 -62.28289 41.13806 1 vs 2 1.190676  0.2752


library(effects)

Effect_Thermo_Tb_Season <- Effect(c("Tb_average", "Season"), m3)  
plot(Effect_Thermo_Tb_Season)



# BUT ACTUALLY HERE WE ONLY WANT SUMMER:

main_Tb_thermo_S <- subset(main, Season == "summer", select = c("Tb_average","Sum_Thermoregulation"))

theme_set(theme_classic())

p <- ggplot(data=main_Tb_thermo_S, aes(Sum_Thermoregulation, Tb_average))+
  labs(y= "Body temperature (?C)", x = "Time invested in thermoregulation (%)") +
  geom_point(size=2) +
  stat_smooth(method = lm, level=0.95)
p + theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14, face="bold"))

#############################


summer <- main[main$Season == "summer", ]


m1 <- lm(Thigmothermy ~ Mb*Tb_average, na.action = na.omit, data = summer)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal residuals

summary(m1)


m2 <- lm(Thigmothermy ~ Mb+Tb_average, na.action = na.omit, data = summer)

summary(m2)

m3 <- lm(Thigmothermy ~ Tb_average, na.action = na.omit, data = summer)

summary(m3)

###############################################################



# Does thermal quality predict time invested in signalling, activity and thermoregulation?


#Activity:

m1 <- lme(Sum_activity ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)
#                                    Value Std.Error DF    t-value p-value
#((Intercept)                    0.5998529 0.4597908 13  1.3046214  0.2146
#Mb                            -0.0199154 0.0203993 13 -0.9762800  0.3467
#Seasonsummer                  -0.1865946 0.6919598  2 -0.2696611  0.8127
#Hours_average                 -0.1462444 0.2247452  2 -0.6507121  0.5820
#Mb:Seasonsummer                0.0154077 0.0316946  2  0.4861308  0.6749
#Mb:Hours_average               0.0071295 0.0093922  2  0.7590882  0.5271
#Seasonsummer:Hours_average    -0.0362890 0.2555333  2 -0.1420129  0.9001
#Mb:Seasonsummer:Hours_average  0.0003632 0.0109540  2  0.0331547  0.9766


m1 <- lme(Sum_activity ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)


m2 <- lme(Sum_activity ~ Mb*Hours_average+Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)
#                                 Value Std.Error DF    t-value p-value
#(Intercept)                 0.6697954 0.4425028 13  1.5136524  0.1540
#Mb                         -0.0223099 0.0197279 13 -1.1308794  0.2785
#Hours_average              -0.2512100 0.1946835  4 -1.2903508  0.2665
#Seasonsummer                0.1195972 0.0728851  4  1.6409001  0.1762
#Mb:Hours_average            0.0113199 0.0082272  4  1.3759061  0.2409
#Hours_average:Seasonsummer -0.0109140 0.0234016  4 -0.4663796  0.6652

anova(m1,m2)
#m1     1 10 -30.44983 -19.09489 25.22491                        
#m2     2  8 -32.81622 -23.73226 24.40811 1 vs 2 1.633614  0.4418


m3 <- lme(Sum_activity ~ Mb*Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#                       Value Std.Error DF   t-value p-value
#(Intercept)       0.6518635 0.4254518 13  1.532168  0.1494
#Mb               -0.0213819 0.0189447 13 -1.128649  0.2794
#Hours_average    -0.2365837 0.1870649  5 -1.264715  0.2617
#Seasonsummer      0.0891442 0.0326366  5  2.731418  0.0412
#Mb:Hours_average  0.0106415 0.0078840  5  1.349756  0.2350

anova(m2,m3)
#p=0.62

m4 <- lme(Sum_activity ~ Mb+Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m4)
#Value  Std.Error DF    t-value p-value
#(Intercept)    0.17611785 0.24077975 13  0.7314479  0.4775
#Mb            -0.00056958 0.01107269 13 -0.0514403  0.9598
#Hours_average  0.01535473 0.01722021  6  0.8916691  0.4069
#Seasonsummer   0.08067650 0.03308885  6  2.4381781  0.0506

anova(m3,m4)
#p=0.13

m5 <- lme(Sum_activity ~ Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m5)
#Value  Std.Error DF  t-value p-value
#(Intercept)   0.16399145 0.04347264 14 3.772291  0.0021
#Hours_average 0.01496560 0.01523886  6 0.982069  0.3640
#Seasonsummer  0.08091355 0.03190606  6 2.535993  0.0443

m6 <- lme(Sum_activity ~ Hours_average*Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m6)
#(Intercept)                 0.16162366 0.04693506 14  3.443559  0.0040
#Hours_average               0.01599049 0.01676013  5  0.954079  0.3839
#Seasonsummer                0.09744265 0.07329407  5  1.329475  0.2411
#Hours_average:Seasonsummer -0.00607279 0.02394705  5 -0.253592  0.8099



###########


#Thermoregultion:

m16 <- lme(Sum_Thermoregulation ~ Mb*Season*Hours_combined, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m16) 
par(op)

par(mfrow=c(1,1))
hist(resid(m16))  # normal

summary(m16)
#                                    Value Std.Error DF    t-value p-value
#(Intercept)                     0.5691339 0.2184721 13  2.6050646  0.0218
#Mb                              0.0158028 0.0097201 13  1.6257947  0.1280
#Seasonsummer                    0.3044033 0.4061684  2  0.7494511  0.5317
#Hours_combined                  0.0582648 0.0266266  2  2.1882163  0.1601
#Mb:Seasonsummer                -0.0136572 0.0184235  2 -0.7412919  0.5357
#Mb:Hours_combined              -0.0024393 0.0011131  2 -2.1914810  0.1598
#Seasonsummer:Hours_combined    -0.0652229 0.0394042  2 -1.6552246  0.2397
#Mb:Seasonsummer:Hours_combined  0.0024756 0.0016812  2  1.4725598  0.2787


m16_1 <- lme(Sum_Thermoregulation ~ Season*Hours_combined+Mb*Hours_combined, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)
summary(m16_1)
#                                 Value  Std.Error DF   t-value p-value
#(Intercept)                  0.6710731 0.19988084 13  3.357366  0.0051
#Seasonsummer                -0.0271379 0.04858962  4 -0.558513  0.6063
#Hours_combined               0.0323569 0.02167871  4  1.492566  0.2098
#Mb                           0.0117119 0.00891663 13  1.313487  0.2117
#Seasonsummer:Hours_combined -0.0050667 0.00397174  4 -1.275682  0.2711
#Hours_combined:Mb           -0.0013841 0.00091405  4 -1.514245  0.2045

m16_2 <- lme(Sum_Thermoregulation ~ Season+Mb*Hours_combined, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)
summary(m16_2)
#                       Value  Std.Error DF   t-value p-value
#(Intercept)        0.6883828 0.20236661 13  3.401662  0.0047
#Seasonsummer      -0.0824685 0.02226914  5 -3.703263  0.0140
#Mb                 0.0114310 0.00904639 13  1.263600  0.2286
#Hours_combined     0.0322282 0.02199129  5  1.465496  0.2027
#Mb:Hours_combined -0.0014289 0.00092656  5 -1.542185  0.1837

m16_3 <- lme(Sum_Thermoregulation ~ Season+Mb+Hours_combined, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)
summary(m16_3)
#                    Value  Std.Error DF   t-value p-value
#(Intercept)     0.9516009 0.11372745 13  8.367381  0.0000
#Seasonsummer   -0.0784465 0.02250748  6 -3.485350  0.0131
#Mb             -0.0001334 0.00529750 13 -0.025177  0.9803
#Hours_combined -0.0014926 0.00222662  6 -0.670345  0.5276 

m16_4 <- lme(Sum_Thermoregulation ~ Season+Hours_combined, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)
summary(m16_4)
#Value   Std.Error DF  t-value p-value
#(Intercept)     0.9489260 0.022415720 14 42.33306   0.000
#Seasonsummer   -0.0782552 0.022014592  6 -3.55470   0.012
#Hours_combined -0.0015411 0.001912201  6 -0.80591   0.451

m16_5 <- lme(Sum_Thermoregulation ~ Hours_combined, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)
summary(m16_5)
#Value   Std.Error DF  t-value p-value
#(Intercept)     0.9309203 0.026115927 14 35.64569  0.0000
#Hours_combined -0.0028273 0.002230653  7 -1.26749  0.2455



#############

# Displays:

m1 <- lme(Sum_displays ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1)) 

summary(m1)
#                                    Value Std.Error DF    t-value p-value
#Mb                            -0.1029381 0.0689627 13 -1.4926627  0.1594
#Seasonsummer                   1.0892621 2.2450170  2  0.4851910  0.6755
#Hours_average                 -1.7506575 0.7603657  2 -2.3023890  0.1479
#Mb:Seasonsummer               -0.0151378 0.1029910  2 -0.1469815  0.8966
#Mb:Hours_average               0.0789886 0.0317766  2  2.4857449  0.1308
#Seasonsummer:Hours_average     0.1645661 0.8210836  2  0.2004255  0.8597
#Mb:Seasonsummer:Hours_average -0.0085280 0.0352337  2 -0.2420411  0.8313


m1 <- lme(Sum_displays ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)


m2 <- lme(Sum_displays ~ Mb*Hours_average+Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)
#Value Std.Error DF   t-value p-value
#(Intercept)                 2.5568493 1.4748212 13  1.733667  0.1066
#Mb                         -0.0938616 0.0657480 13 -1.427597  0.1770
#Hours_average              -1.4659089 0.6473780  4 -2.264378  0.0863
#Seasonsummer                0.8544511 0.2320582  4  3.682055  0.0212
#Mb:Hours_average            0.0675522 0.0273623  4  2.468801  0.0690
#Hours_average:Seasonsummer -0.0784429 0.0743292  4 -1.055344  0.3508

m3 <- lme(Sum_displays ~ Mb*Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#Value Std.Error DF   t-value p-value
#((Intercept)       2.5885284 1.4261443 13  1.815054  0.0926
#Mb               -0.0933999 0.0635063 13 -1.470719  0.1652
#Hours_average    -1.4665350 0.6271473  5 -2.338422  0.0665
#Seasonsummer      0.6415659 0.1099303  5  5.836115  0.0021
#Mb:Hours_average  0.0668190 0.0264316  5  2.527995  0.0527


m4 <- lme(Sum_displays ~ Mb+Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m4)
#Value Std.Error DF   t-value p-value
#                       Value Std.Error DF   t-value p-value
#(Intercept)   -0.3609980 0.9722682 13 -0.371295  0.7164
#Mb             0.0361703 0.0444281 13  0.814130  0.4302
#Hours_average  0.1111871 0.0657777  6  1.690346  0.1419
#Seasonsummer   0.5798212 0.1079430  6  5.371551  0.0017


m5 <- lme(Sum_displays ~ Hours_combined+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m5)
#Value Std.Error DF   t-value p-value
#(Intercept)    -0.3576562 0.9809853 13 -0.364589  0.7213
#Mb              0.0360308 0.0447998 13  0.804262  0.4357
#Hours_combined  0.0277966 0.0164972  6  1.684931  0.1430
#Seasonsummer    0.5791172 0.1068059  6  5.422147  0.0016


m21_5 <- lme(Sum_displays ~ Season+Hours_combined, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)
summary(m21_5)
#Fixed effects: Sum_displays ~ Season + Hours_combined 
#                   Value  Std.Error DF  t-value p-value
#(Intercept)    0.4191026 0.17199821 14 2.436668  0.0288
#Hours_combined 0.0330196 0.01503544  6 2.196117  0.0705
#Seasonsummer   0.5644391 0.10405473  6 5.424444  0.0016



##########################################################################

## try with separate behaviours

library(lme4)
library(nlme)

#Moving:

m1 <- lme(Moving ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)
#                                    Value Std.Error DF    t-value p-value
#(Intercept)                    0.4222426 0.3280932 13  1.2869593  0.2206
#Mb                            -0.0137010 0.0145288 13 -0.9430225  0.3629
#Seasonsummer                   0.2275076 0.4290075  2  0.5303115  0.6489
#Hours_average                 -0.1694621 0.1603285  2 -1.0569680  0.4013
#Mb:Seasonsummer               -0.0037451 0.0197434  2 -0.1896903  0.8671
#Mb:Hours_average               0.0080851 0.0067021  2  1.2063578  0.3510
#Seasonsummer:Hours_average    -0.0591041 0.1532225  2 -0.3857402  0.7369
#Mb:Seasonsummer:Hours_average  0.0011464 0.0065916  2  0.1739128  0.8779


m1 <- lme(Moving ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)


m2 <- lme(Moving ~ Mb*Hours_average+Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)
#                                 Value Std.Error DF    t-value p-value
#(Intercept)                 0.4422358 0.30759402 13  1.437725  0.1741
#Mb                         -0.0146007 0.01370971 13 -1.064993  0.3063
#Hours_average              -0.1787540 0.13273782  4 -1.346670  0.2493
#Seasonsummer                0.1543249 0.04068506  4  3.793158  0.0192
#Mb:Hours_average            0.0084809 0.00561396  4  1.510685  0.2054
#Hours_average:Seasonsummer -0.0362522 0.01291118  4 -2.807814  0.0484

anova(m1,m2)
#p=0.9392


m3 <- lme(Moving ~ Mb+Hours_average*Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#                       Value Std.Error DF   t-value p-value
#(Intercept)                 0.06944383 0.18361905 13  0.378195  0.7114
#Mb                          0.00183223 0.00836381 13  0.219067  0.8300
#Hours_average               0.02087127 0.01275187  5  1.636723  0.1626
#Seasonsummer                0.13598603 0.04295626  5  3.165686  0.0249
#Hours_average:Seasonsummer -0.03219012 0.01390801  5 -2.314502  0.0685

anova(m2,m3)
#p=0.0935

m4 <- lme(Moving ~ Hours_average*Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m4)
#Value  Std.Error DF    t-value p-value
#(Intercept)                 0.10899674 0.03257319 14  3.346210  0.0048
#Hours_average               0.02191075 0.01153732  5  1.899119  0.1160
#Seasonsummer                0.13519009 0.04165439  5  3.245519  0.0228
#Hours_average:Seasonsummer -0.03216641 0.01354192  5 -2.375321  0.0635

anova(m3,m4)
#p=0.8046



#Basking:


m1 <- lme(Basking ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)
#                                    Value Std.Error DF    t-value p-value
#(Intercept)                    3.0410828 0.8362633 13  3.636514  0.0030
#Mb                            -0.1261510 0.0372291 13 -3.388504  0.0048
#Seasonsummer                  -1.9906437 1.7200408  2 -1.157324  0.3667
#Hours_average                 -1.3732062 0.4073434  2 -3.371127  0.0779
#Mb:Seasonsummer                0.0900085 0.0775953  2  1.159973  0.3658
#Mb:Hours_average               0.0600309 0.0170310  2  3.524801  0.0719
#Seasonsummer:Hours_average     1.0276859 0.6854289  2  1.499333  0.2725
#Mb:Seasonsummer:Hours_average -0.0441366 0.0291987  2 -1.511593  0.2698


m1 <- lme(Basking ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)


m2 <- lme(Basking ~ Mb*Hours_average+Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)
#                                 Value Std.Error DF    t-value p-value
#(Intercept)                 2.4761379 0.7358955 13  3.364796  0.0051
#Mb                         -0.1019451 0.0328285 13 -3.105385  0.0084
#Hours_average              -0.9845411 0.3164428  4 -3.111277  0.0358
#Seasonsummer                0.0710351 0.1909733  4  0.371964  0.7288
#Mb:Hours_average            0.0438316 0.0133304  4  3.288090  0.0303
#Hours_average:Seasonsummer -0.0132828 0.0625556  4 -0.212335  0.8422

anova(m1,m2)
#p=0.1856


m3 <- lme(Basking ~ Mb*Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#                       Value Std.Error DF   t-value p-value
#(Intercept)       2.4906882 0.7129981 13  3.493261  0.0040
#Mb               -0.1022631 0.0319126 13 -3.204477  0.0069
#Hours_average    -0.9863140 0.3078274  5 -3.204113  0.0239
#Seasonsummer      0.0349239 0.0845433  5  0.413088  0.6967
#Mb:Hours_average  0.0437704 0.0129690  5  3.375010  0.0198

anova(m2,m3)
#p=0.8051

m4 <- lme(Basking ~ Mb*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m4)
#Value  Std.Error DF    t-value p-value
#(Intercept)       2.4865830 0.6971954 13  3.566551  0.0034
#Mb               -0.1017831 0.0311876 13 -3.263574  0.0062
#Hours_average    -0.9696492 0.2984377  6 -3.249084  0.0175
#Mb:Hours_average  0.0431777 0.0126049  6  3.425469  0.0140

anova(m3,m4)
#p=0.6413


Effect_Basking_TQ_Mb <- Effect(c("Hours_average", "Mb"), m4)  
plot(Effect_Basking_TQ_Mb)


p <- ggplot(data=main, aes(Hours_average, Basking))+
#  labs(y= "(?C)", x = "Time invested in thermoregulation (%)") +
  geom_point(size=2) +
  stat_smooth(method = lm, level=0.95)
p + theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14, face="bold"))

# --> counterintuitive



#Thigmo:


m1 <- lme(Thigmothermy ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)
#                                    Value Std.Error DF    t-value p-value
#(Intercept)                   -2.6225079 0.8951396 13 -2.929719  0.0117
#Mb                             0.1480303 0.0398502 13  3.714671  0.0026
#Seasonsummer                   2.2194929 1.8411387  2  1.205500  0.3513
#Hours_average                  1.6561607 0.4360220  2  3.798342  0.0628
#Mb:Seasonsummer               -0.1014762 0.0830584  2 -1.221746  0.3463
#Mb:Hours_average              -0.0718456 0.0182301  2 -3.941047  0.0588
#Seasonsummer:Hours_average    -1.2522653 0.7336859  2 -1.706814  0.2300
#Mb:Seasonsummer:Hours_average  0.0526850 0.0312544  2  1.685682  0.2339


m1 <- lme(Thigmothermy ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)


m2 <- lme(Thigmothermy ~ Mb*Hours_average+Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)
#                                 Value Std.Error DF    t-value p-value
#(Intercept)                -1.9654796 0.8031149 13 -2.447320  0.0294
#Mb                          0.1200574 0.0358272 13  3.351014  0.0052
#Hours_average               1.1756616 0.3453478  4  3.404283  0.0272
#Seasonsummer               -0.1206758 0.2084176  4 -0.579010  0.5936
#Mb:Hours_average           -0.0518751 0.0145481  4 -3.565770  0.0235
#Hours_average:Seasonsummer -0.0039582 0.0682697  4 -0.057978  0.9565

anova(m1,m2)
#p=0.1189


m3 <- lme(Thigmothermy ~ Mb*Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#                       Value Std.Error DF   t-value p-value
#(Intercept)      -1.9611437 0.7771731 13 -2.523432  0.0254
#Mb                0.1199626 0.0347849 13  3.448693  0.0043
#Hours_average     1.1751333 0.3355341  5  3.502277  0.0172
#Seasonsummer     -0.1314367 0.0921528  5 -1.426291  0.2131
#Mb:Hours_average -0.0518933 0.0141363  5 -3.670932  0.0144

anova(m2,m3)
#p=0.9462

m4 <- lme(Thigmothermy ~ Mb*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m4)
#Value  Std.Error DF    t-value p-value
#(Intercept)      -1.9456937 0.7979687 13 -2.438308  0.0299
#Mb                0.1181558 0.0356955 13  3.310105  0.0056
#Hours_average     1.1124153 0.3415742  6  3.256731  0.0173
#Mb:Hours_average -0.0496627 0.0144268  6 -3.442382  0.0138

anova(m3,m4)
#p=0.1166


theme_set(theme_classic())

p <- ggplot(data=main, aes(Hours_average, Thigmothermy))+
  labs(y= "Thigmothermy (%)", x = "Thermal quality index") +
  geom_point(size=2) +
  stat_smooth(method = lm, level=0.95)
p + theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14, face="bold"))


######## THIS IS COOL!!!


#Head bobbing:


m1 <- lme(Head.bobbing ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)
#                                    Value Std.Error DF    t-value p-value
#(Intercept)                    0.3376278 0.2417637 13  1.396520  0.1859
#Mb                            -0.0142974 0.0107629 13 -1.328390  0.2069
#Seasonsummer                   0.6457939 0.4970036  2  1.299375  0.3234
#Hours_average                 -0.1226678 0.1177630  2 -1.041650  0.4069
#Mb:Seasonsummer               -0.0284520 0.0224217  2 -1.268946  0.3322
#Mb:Hours_average               0.0059376 0.0049237  2  1.205930  0.3512
#Seasonsummer:Hours_average    -0.3587417 0.1980247  2 -1.811601  0.2117
#Mb:Seasonsummer:Hours_average  0.0166189 0.0084357  2  1.970054  0.1876


m1 <- lme(Head.bobbing ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)


m2 <- lme(Head.bobbing ~ Mb*Hours_average+Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)
#                                 Value Std.Error DF    t-value p-value
#(Intercept)                 0.5347870 0.22600545 13  2.3662569  0.0342
#Mb                         -0.0225826 0.01008216 13 -2.2398536  0.0432
#Hours_average              -0.2842339 0.09718471  4 -2.9246772  0.0430
#Seasonsummer               -0.0203295 0.05865101  4 -0.3466173  0.7463
#Mb:Hours_average            0.0126201 0.00409399  4  3.0825993  0.0368
#Hours_average:Seasonsummer  0.0384385 0.01921185  4  2.0007682  0.1160

anova(m1,m2)
#p=0.05


m3 <- lme(Head.bobbing ~ Mb*Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#                       Value Std.Error DF   t-value p-value
#(Intercept)       0.4926803 0.24307091 13  2.026899  0.0637
#Mb               -0.0216623 0.01087944 13 -1.991124  0.0679
#Hours_average    -0.2791034 0.10494262  5 -2.659581  0.0449
#Seasonsummer      0.0841714 0.02882198  5  2.920389  0.0330
#Mb:Hours_average  0.0127972 0.00442130  5  2.894444  0.0340

anova(m2,m3)
#p=0.0274


# --> stick with the m2:

summary(m2)
#                                 Value Std.Error DF    t-value p-value
#(Intercept)                 0.5347870 0.22600545 13  2.3662569  0.0342
#Mb                         -0.0225826 0.01008216 13 -2.2398536  0.0432
#Hours_average              -0.2842339 0.09718471  4 -2.9246772  0.0430
#Seasonsummer               -0.0203295 0.05865101  4 -0.3466173  0.7463
#Mb:Hours_average            0.0126201 0.00409399  4  3.0825993  0.0368
#Hours_average:Seasonsummer  0.0384385 0.01921185  4  2.0007682  0.1160

theme_set(theme_classic())

p <- ggplot(data=main, aes(Hours_average, Head.bobbing))+
   labs(y= bquote("Rate of head bobbing"~(min^-1)), x = "Thermal quality index") +
  geom_point(size=2) +
  stat_smooth(method = lm, level=0.95)
p + theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14, face="bold"))


# WOOHOOOOO!!!



#Push-ups:

plot(main$Push.ups, main$Hours_average)
m1 <- lme(Push.ups ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)
#                                    Value Std.Error DF    t-value p-value
#(Intercept)                    2.5701806 1.5143925 13  1.6971694  0.1135
#Mb                            -0.0933033 0.0671350 13 -1.3897863  0.1879
#Seasonsummer                   0.6149950 2.1533726  2  0.2855962  0.8020
#Hours_average                 -1.6936438 0.7403796  2 -2.2875345  0.1494
#Mb:Seasonsummer                0.0059844 0.0988390  2  0.0605468  0.9572
#Mb:Hours_average               0.0757625 0.0309420  2  2.4485318  0.1341
#Seasonsummer:Hours_average     0.4685712 0.7848000  2  0.5970580  0.6111
#Mb:Seasonsummer:Hours_average -0.0227687 0.0336894  2 -0.6758438  0.5688


m1 <- lme(Push.ups ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)


m2 <- lme(Push.ups ~ Mb*Hours_average+Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)
#                                 Value Std.Error DF    t-value p-value
#(Intercept)                 2.1954158 1.4542758 13  1.509628  0.1551
#Mb                         -0.0791529 0.0648350 13 -1.220836  0.2438
#Hours_average              -1.2709094 0.6397284  4 -1.986639  0.1179
#Seasonsummer                0.8819120 0.2386866  4  3.694853  0.0209
#Mb:Hours_average            0.0587936 0.0270349  4  2.174732  0.0953
#Hours_average:Seasonsummer -0.1168747 0.0766221  4 -1.525339  0.2019

anova(m1,m2)
#p=0.2999


m3 <- lme(Push.ups ~ Mb*Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#                       Value Std.Error DF   t-value p-value
#(Intercept)       2.2501469 1.4041741 13  1.602470  0.1331
#Mb               -0.0789422 0.0625780 13 -1.261501  0.2293
#Hours_average    -1.2722480 0.6180663  5 -2.058433  0.0946
#Seasonsummer      0.5654961 0.1196126  5  4.727732  0.0052
#Mb:Hours_average  0.0577661 0.0260470  5  2.217767  0.0773

anova(m2,m3)
#p=0.1275


m4 <- lme(Push.ups ~ Mb+Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m4)
#                       Value Std.Error DF   t-value p-value
#(Intercept)   -0.3168779 0.9041190 13 -0.350483  0.7316
#Mb             0.0339391 0.0415018 13  0.817774  0.4282
#Hours_average  0.0908770 0.0637029  6  1.426575  0.2036
#Seasonsummer   0.5126360 0.1168906  6  4.385606  0.0046

anova(m3,m4)
#p=0.0248


# --> stick with m3:

summary(m3)
#                       Value Std.Error DF   t-value p-value
#(Intercept)       2.2501469 1.4041741 13  1.602470  0.1331
#Mb               -0.0789422 0.0625780 13 -1.261501  0.2293
#Hours_average    -1.2722480 0.6180663  5 -2.058433  0.0946
#Seasonsummer      0.5654961 0.1196126  5  4.727732  0.0052
#Mb:Hours_average  0.0577661 0.0260470  5  2.217767  0.0773




##########################################################################

## TQ and accuracy of thermoregulation:


m1 <- lme(Dev_Topt ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)
#                                    Value Std.Error DF    t-value p-value
#(Intercept)                   -11.031107 10.012766 13 -1.101704  0.2906
#Mb                              0.615931  0.450123 13  1.368362  0.1944
#Seasonsummer                   13.747337 11.952006  3  1.150212  0.3334
#Hours_average                   7.257181  4.563836  3  1.590150  0.2100
#Mb:Seasonsummer                -0.624393  0.528451  3 -1.181554  0.3225
#Mb:Hours_average               -0.269069  0.192257  3 -1.399527  0.2561
#Seasonsummer:Hours_average     -4.964468  4.696531  3 -1.057050  0.3681
#Mb:Seasonsummer:Hours_average   0.205339  0.197332  3  1.040576  0.3746


m1 <- lme(Dev_Topt ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)


m2 <- lme(Dev_Topt ~ Mb*Hours_average+Season*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)
#                                 Value Std.Error DF    t-value p-value
#(Intercept)                -11.360255  9.890635 13 -1.1485870  0.2714
#Mb                           0.635879  0.446435 13  1.4243505  0.1779
#Hours_average                7.612949  4.118521  5  1.8484668  0.1238
#Seasonsummer                -0.412500  1.205971  5 -0.3420481  0.7462
#Mb:Hours_average            -0.286942  0.175564  5 -1.6344016  0.1631
#Hours_average:Seasonsummer  -0.176303  0.408454  5 -0.4316360  0.6840

anova(m1,m2)
#p=0.3436


m3 <- lme(Dev_Topt ~ Mb*Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#(Intercept)      -12.781413  9.758856 13 -1.309724  0.2130
#Mb                 0.708894  0.438735 13  1.615767  0.1301
#Hours_average      8.100009  4.047206  6  2.001383  0.0922
#Seasonsummer      -0.829579  0.552349  6 -1.501909  0.1838
#Mb:Hours_average  -0.311867  0.171955  6 -1.813656  0.1197

anova(m2,m3)
#p=0.6522


m4 <- lme(Dev_Topt ~ Mb+Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m4)
#                       Value Std.Error DF   t-value p-value
#(Intercept)    2.7443160  5.067769 13  0.5415235  0.5973
#Mb             0.0099568  0.234242 13  0.0425063  0.9667
#Hours_average  0.8740065  0.372600  7  2.3456978  0.0514
#Seasonsummer  -0.7012490  0.609569  7 -1.1504012  0.2878

anova(m3,m4)
#p=0.0643


m5 <- lme(Dev_Topt ~ Hours_average+Season, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m5)
#                       Value Std.Error DF   t-value p-value
#(Intercept)    2.9543658 0.9552476 14  3.092775  0.0079
#Hours_average  0.8819058 0.3231268  7  2.729287  0.0294
#Seasonsummer  -0.7062908 0.5889711  7 -1.199194  0.2695

anova(m4,m5)
#p=0.9636


m6 <- lme(Dev_Topt ~ Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m6)
#                       Value Std.Error DF   t-value p-value
#(Intercept)   3.0250013 0.9691190 14 3.121393  0.0075
#Hours_average 0.7504086 0.3107953  8 2.414479  0.0422

anova(m5,m6)
#p=0.2084


p <- ggplot(data=main, aes(Hours_average, Dev_Topt))+
  labs(y= "db (?C)", x = "Thermal quality index") +
  geom_point(size=2) +
  stat_smooth(method = lm, level=0.95)
p + theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14, face="bold"))

##############

## TQ and Tb:


m1 <- lme(Tb_average ~ Mb*Season*Hours_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)

m1 <- lme(Tb_average ~ Season*Mb*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)


m2 <- lme(Tb_average ~ Season+Mb*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)

anova(m1,m2)

m3 <- lme(Tb_average ~ Mb*Hours_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#Fixed effects:  Tb_average ~ Mb * Hours_average 
#Value Std.Error DF   t-value p-value
#(Intercept)      46.15151  4.451489 13 10.367658  0.0000
#Mb               -0.43127  0.199640 13 -2.160252  0.0500
#Hours_average    -3.86038  1.862673  7 -2.072493  0.0769
#Mb:Hours_average  0.17598  0.078954  7  2.228849  0.0611

anova(m2,m3)

########################################

## does Tb predict behavior?

m1 <- lme(Sum_displays ~ Mb*Season*Tb_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)
#Value Std.Error DF    t-value p-value
#(Intercept)                  24.19892  27.43371 14  0.8820870  0.3926
#Mb                           -1.26118   1.22257 14 -1.0315862  0.3198
#Seasonsummer               -111.05918  68.97137  6 -1.6102213  0.1585
#Tb_average                   -0.69169   0.73085  6 -0.9464123  0.3805
#Mb:Seasonsummer               5.86881   3.03737  6  1.9322024  0.1015
#Mb:Tb_average                 0.03671   0.03264  6  1.1247173  0.3037
#Seasonsummer:Tb_average       3.00050   1.86894  6  1.6054574  0.1595
#Mb:Seasonsummer:Tb_average   -0.15783   0.08230  6 -1.9177071  0.1036

m1 <- lme(Sum_displays ~ Mb*Season*Tb_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)

m2 <- lme(Sum_displays ~ Season+Mb*Tb_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)

anova(m1,m2)
#0.0053

m3 <- lme(Sum_displays ~ Season+Mb+Tb_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#Fixed effects:  Sum_displays ~ Season + Mb + Tb_average 
#Value Std.Error DF   t-value p-value
#(Intercept)  -2.1639965 3.1389357 14 -0.689405  0.5018
#Seasonsummer  0.5392573 0.1282359 10  4.205199  0.0018
#Mb            0.1360674 0.0448797 14  3.031824  0.0090
#Tb_average   -0.0026019 0.0776026 10 -0.033528  0.9739

############

# does accuracy predict displays?
m1 <- lme(Sum_displays ~ Mb*Season*Dev_Topt, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)

m1 <- lme(Sum_displays ~ Mb*Season*Dev_Topt, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)

m2 <- lme(Sum_displays ~ Mb+Season*Dev_Topt, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)

m3 <- lme(Sum_displays ~ Mb+Season+Dev_Topt, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#Value Std.Error DF   t-value p-value
#(Intercept)  -2.2555768 1.0282836 14 -2.193536  0.0456
#Mb            0.1436643 0.0451747 14  3.180193  0.0067
#Seasonsummer  0.5322272 0.1240513 10  4.290379  0.0016
#Dev_Topt     -0.0362621 0.0401840 10 -0.902401  0.3881

#############

## does Tb predict thermoregulation?

m1 <- lme(Sum_Thermoregulation ~ Mb*Season*Tb_average, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)

m1 <- lme(Sum_Thermoregulation ~ Mb*Season*Tb_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)

m2 <- lme(Sum_Thermoregulation ~ Mb+Season*Tb_average, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)
#Fixed effects:  Sum_Thermoregulation ~ Mb + Season * Tb_average 
#Value Std.Error DF   t-value p-value
#(Intercept)              1.7972905 0.5075948 14  3.540798  0.0033
#Mb                      -0.0043663 0.0043682 14 -0.999574  0.3345
#Seasonsummer            -2.7277662 0.9895598  9 -2.756545  0.0222
#Tb_average              -0.0207476 0.0130470  9 -1.590217  0.1462
#Seasonsummer:Tb_average  0.0708865 0.0267105  9  2.653886  0.0263

# does accuracy predict thermoregulation?
m1 <- lme(Sum_Thermoregulation ~ Mb*Season*Dev_Topt, random = ~ 1 | Lizard_ID, method = "REML", na.action = na.omit, data = main)

op <- par(mfrow=c(2,2))
plot(m1) 
par(op)

par(mfrow=c(1,1))
hist(resid(m1))  # normal

summary(m1)

m1 <- lme(Sum_Thermoregulation ~ Mb*Season*Dev_Topt, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)

m2 <- lme(Sum_Thermoregulation ~ Season+Mb*Dev_Topt, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m2)

m3 <- lme(Sum_Thermoregulation ~ Mb+Season+Dev_Topt, random = ~ 1 | Lizard_ID, method = "ML", na.action = na.omit, data = main)
summary(m3)
#                  Value  Std.Error DF   t-value p-value
#(Intercept)   0.9975094 0.11599418 14  8.599651  0.0000
#Mb           -0.0028597 0.00519117 14 -0.550875  0.5904
#Seasonsummer -0.1029091 0.02435032 10 -4.226192  0.0018
#Dev_Topt     -0.0001939 0.00481524 10 -0.040265  0.9687




