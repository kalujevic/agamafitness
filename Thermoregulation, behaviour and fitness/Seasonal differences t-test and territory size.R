main <- read.csv(file="Main spreadsheet_seasons_NEW.csv", header=TRUE, sep=",")


View(main)



shapiro.test(main$Hours_combined[main$Season=="summer"]) #p-value = 0.1208
shapiro.test(main$Hours_combined[main$Season=="autumn"]) #p-value = 0.02483

main$Season <- as.factor(main$Season)

wt <- wilcox_test(Hours_combined~Season, data = main,
                  distribution = "exact", conf.int = TRUE)

wt
#Exact Wilcoxon-Mann-Whitney Test

#data:  Hours_combined by Season (autumn, summer)
#Z = -1.2649, p-value = 0.2199
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -6.959211  2.683994
#sample estimates:
#  difference in location 
#-2.044788 


main %>% cohens_d(Hours_combined~Season)
#.y.            group1 group2 effsize    n1    n2 magnitude
#* <chr>          <chr>  <chr>    <dbl> <int> <int> <ord>    
#  1 Hours_combined autumn summer  -0.284    16    10 small   


shapiro.test(main$Hours_full.sun[main$Season=="summer"]) #p-value = 0.42
shapiro.test(main$Hours_full.sun[main$Season=="autumn"]) #p-value = 0.20

var.test(main$Hours_full.sun[main$Season=="summer"],main$Hours_full.sun[main$Season=="autumn"])
#p-value = 0.06

t.test(main$Hours_full.sun ~ main$Season, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=T, na.action = na.pass)
#Paired t-test

#data:  main$Hours_full.sun by main$Season
#t = 0.51003, df = 9, p-value = 0.6223
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.3769763  0.5964469
#sample estimates:
#  mean of the differences 
#0.1097353





shapiro.test(main$Hours_partly.sun[main$Season=="summer"]) #p-value = 0.26
shapiro.test(main$Hours_partly.sun[main$Season=="autumn"]) #p-value = 0.12

var.test(main$Hours_partly.sun[main$Season=="summer"],main$Hours_partly.sun[main$Season=="autumn"])
#p-value = 0.80

t.test(main$Hours_partly.sun ~ main$Season, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=T, na.action = na.pass)
#Paired t-test

#data:  main$Hours_partly.sun by main$Season
#t = -1.8776, df = 9, p-value = 0.09316
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.2501128  0.1161193
#sample estimates:
#  mean of the differences 
#-0.5669967 





shapiro.test(main$Hours_shade[main$Season=="summer"]) #p-value = 0.19
shapiro.test(main$Hours_shade[main$Season=="autumn"]) #p-value = 0.005

wt <- wilcox_test(Hours_shade~Season, data = main,
                  distribution = "exact", conf.int = TRUE)

wt
#Exact Wilcoxon-Mann-Whitney Test

#data:  Hours_shade by Season (autumn, summer)
#Z = -1.8447, p-value = 0.06842
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -2.2142375  0.1153846
#sample estimates:
#  difference in location 
#-0.9556765 



shapiro.test(main$Hours_crevice[main$Season=="summer"]) #p-value = 0.11
shapiro.test(main$Hours_crevice[main$Season=="autumn"]) #p-value = 0.0008

wt <- wilcox_test(Hours_crevice~Season, data = main,
                  distribution = "exact", conf.int = TRUE)

wt
# Wilcoxon-Mann-Whitney Test

#data:  Hours_crevice by Season (autumn, summer)
#Z = -1.9006, p-value = 0.05819
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -2.36909582  0.05870445
#sample estimates:
#  difference in location 
#-0.7894737 




shapiro.test(main$Tb_average[main$Season=="summer"]) #p-value = 0.91
shapiro.test(main$Tb_average[main$Season=="autumn"]) #p-value = 0.29

var.test(main$Tb_average[main$Season=="summer"],main$Tb_average[main$Season=="autumn"])
#p-value = 0.01

t.test(main$Tb_average ~ main$Season, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=T, na.action = na.pass)
#Paired t-test

#data:  main$Tb_average by main$Season
#t = 0.40556, df = 13, p-value = 0.6917
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.8202157  1.1993400
#sample estimates:
#  mean of the differences 
#0.1895621

main %>% cohens_d(Tb_average~Season, var.equal = F)
#.y.        group1 group2 effsize    n1    n2 magnitude 
#* <chr>      <chr>  <chr>    <dbl> <int> <int> <ord>     
#1 Tb_average autumn summer   0.179    14    16 negligible


shapiro.test(main$Dev_Topt[main$Season=="summer"]) #p-value = 0.08
shapiro.test(main$Dev_Topt[main$Season=="autumn"]) #p-value = 0.54

var.test(main$Dev_Topt[main$Season=="summer"],main$Dev_Topt[main$Season=="autumn"])
#p-value = 0.73

t.test(main$Dev_Topt ~ main$Season, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=T, na.action = na.pass)
#Paired t-test

#data:  main$Dev_Topt by main$Season
#t = 0.39958, df = 13, p-value = 0.696
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.7210986  1.0483800
#sample estimates:
#  mean of the differences 
#0.1636407

main %>% cohens_d(Dev_Topt~Season, var.equal = T)
#.y.      group1 group2 effsize    n1    n2 magnitude 
#* <chr>    <chr>  <chr>    <dbl> <int> <int> <ord>     
#  1 Dev_Topt autumn summer   0.121    14    16 negligible

shapiro.test(main$Sum_Thermoregulation[main$Season=="summer"]) #p-value = 0.77
shapiro.test(main$Sum_Thermoregulation[main$Season=="autumn"]) #p-value = 0.44

var.test(main$Sum_Thermoregulation[main$Season=="summer"],main$Sum_Thermoregulation[main$Season=="autumn"])
#p-value = 0.29

t.test(main$Sum_Thermoregulation ~ main$Season, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=T, na.action = na.pass)
#Paired t-test

#data:  main$Sum_Thermoregulation by main$Season
#t = 4.4949, df = 12, p-value = 0.0007331
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.05843289 0.16837073
#sample estimates:
#  mean of the differences 
#0.1134018 

main %>% cohens_d(Sum_Thermoregulation~Season, var.equal = T)
#.y.                  group1 group2 effsize    n1    n2 magnitude
#* <chr>                <chr>  <chr>    <dbl> <int> <int> <ord>    
#  1 Sum_Thermoregulation autumn summer    1.66    14    15 large   


shapiro.test(main$Sum_activity[main$Season=="summer"]) #p-value = 0.16
shapiro.test(main$Sum_activity[main$Season=="autumn"]) #p-value = 0.48

var.test(main$Sum_activity[main$Season=="summer"],main$Sum_activity[main$Season=="autumn"])
#p-value = 0.49

t.test(main$Sum_activity ~ main$Season, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=T, na.action = na.pass)
#Paired t-test

#data:  main$Sum_activity by main$Season
#t = -1.7672, df = 12, p-value = 0.1026
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.18064821  0.01884402
#sample estimates:
#  mean of the differences 
#-0.08090209 

main %>% cohens_d(Sum_activity~Season, var.equal = T)


shapiro.test(main$Sum_displays[main$Season=="summer"]) #p-value = 0.20
shapiro.test(main$Sum_displays[main$Season=="autumn"]) #p-value = 0.01

wt <- wilcox_test(Sum_displays~Season, data = main,
                  distribution = "exact", conf.int = TRUE)

wt
#Exact Wilcoxon-Mann-Whitney Test

#data:  Sum_displays by Season (autumn, summer)
#Z = -3.2733, p-value = 0.0006293
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -1.0926959 -0.3527026
#sample estimates:
#  difference in location 
#-0.6504555 

main %>% cohens_d(Sum_displays~Season)
#.y.          group1 group2 effsize    n1    n2 magnitude
#* <chr>        <chr>  <chr>    <dbl> <int> <int> <ord>    
#  1 Sum_displays autumn summer   -1.21    14    15 large    


#############################################################


TS <- read.csv(file="Territory size_summary_Mb_SVL_NEW.csv", header=TRUE, sep=",")


View(TS)


# Summer:

MCP50s <- lm(MCP_50_summer ~ Mb, data = TS)
summary(MCP50s)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1428.60     370.73   3.853 0.000809 ***
#  Mb            -49.26      15.71  -3.135 0.004649 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 233.8 on 23 degrees of freedom
#(9 observations deleted due to missingness)
#Multiple R-squared:  0.2993,	Adjusted R-squared:  0.2689 
#F-statistic: 9.826 on 1 and 23 DF,  p-value: 0.004649

op <- par(mfrow=c(2,2))
plot(MCP50s) 
par(op)

par(mfrow=c(1,1))
hist(resid(MCP50s))



p <- ggplot(data=TS, aes(Mb, MCP_50_summer))+
  labs(y= "Core area (m^2)", x = "Body mass (g)") +
  geom_point(size=3) +
  geom_abline(intercept = 1428.6, slope = -49.26, size = 2, color = "blue") + theme_classic()


p + theme(axis.title.x = element_text(size=16, 
                                      color="black", 
                                      face="bold",
                                      angle=0),
          axis.title.y = element_text(size=16, 
                                      color="black", 
                                      face="bold",
                                      angle=90),
          axis.text.x= element_text(family = "Tahoma", 
                                    face="bold", colour="black",
                                    size=14),
          axis.text.y= element_text(family = "Tahoma", 
                                    face="bold", colour="black",
                                    size=14))

##


MCP95s <- lm(MCP_95_summer ~ Mb, data = TS)
summary(MCP95s)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   5328.6     1715.0   3.107  0.00496 **
#  Mb            -172.2       72.7  -2.368  0.02664 * 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1082 on 23 degrees of freedom
#(9 observations deleted due to missingness)
#Multiple R-squared:  0.1961,	Adjusted R-squared:  0.1611 
#F-statistic:  5.61 on 1 and 23 DF,  p-value: 0.02664


op <- par(mfrow=c(2,2))
plot(MCP95s) 
par(op)

par(mfrow=c(1,1))
hist(resid(MCP95s)) #not normal


MCP95s1 <- lm(log(MCP_95_summer) ~ Mb, data = TS)
summary(MCP95s1)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  9.78191    1.32957   7.357 1.75e-07 ***
#  Mb          -0.12679    0.05636  -2.250   0.0343 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.8386 on 23 degrees of freedom
#(9 observations deleted due to missingness)
#Multiple R-squared:  0.1804,	Adjusted R-squared:  0.1447 
#F-statistic: 5.061 on 1 and 23 DF,  p-value: 0.03434


par(mfrow=c(1,1))
hist(resid(MCP95s1))

TS$MCP_95_summer_log<-log(TS$MCP_95_summer)


p <- ggplot(data=TS, aes(Mb, MCP_95_summer_log))+
  labs(y= "log Home range (m^2)", x = "Body mass (g)") +
  geom_point(size=3) +
  geom_abline(intercept = 9.78, slope = -0.127, size = 2, color = "blue") + theme_classic()



p + theme(axis.title.x = element_text(size=16, 
                                      color="black", 
                                      face="bold",
                                      angle=0),
          axis.title.y = element_text(size=16, 
                                      color="black", 
                                      face="bold",
                                      angle=90),
          axis.text.x= element_text(family = "Tahoma", 
                                    face="bold", colour="black",
                                    size=14),
          axis.text.y= element_text(family = "Tahoma", 
                                    face="bold", colour="black",
                                    size=14))



# Autumn:

MCP50a <- lm(MCP_50_autumn ~ Mb, data = TS)
summary(MCP50a)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  509.576    205.203   2.483   0.0204 *
#  Mb           -10.986      9.194  -1.195   0.2438  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 164.7 on 24 degrees of freedom
#(8 observations deleted due to missingness)
#Multiple R-squared:  0.05615,	Adjusted R-squared:  0.01682 
#F-statistic: 1.428 on 1 and 24 DF,  p-value: 0.2438



op <- par(mfrow=c(2,2))
plot(MCP50a) 
par(op)

par(mfrow=c(1,1))
hist(resid(MCP50a))



p <- ggplot(data=TS, aes(Mb, MCP_50_autumn))+
  labs(y= "Core area (m^2)", x = "Body mass (g)") +
  geom_point(size=3) +
  geom_abline(intercept = 509.58, slope = -10.99, size = 2, color = "blue") + theme_classic()


p + theme(axis.title.x = element_text(size=16, 
                                      color="black", 
                                      face="bold",
                                      angle=0),
          axis.title.y = element_text(size=16, 
                                      color="black", 
                                      face="bold",
                                      angle=90),
          axis.text.x= element_text(family = "Tahoma", 
                                    face="bold", colour="black",
                                    size=14),
          axis.text.y= element_text(family = "Tahoma", 
                                    face="bold", colour="black",
                                    size=14))

##


MCP95a <- lm(MCP_95_autumn ~ Mb, data = TS)
summary(MCP95a)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  3777.53    1356.88   2.784   0.0103 *
#  Mb           -107.60      60.79  -1.770   0.0895 .
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1089 on 24 degrees of freedom
#(8 observations deleted due to missingness)
#Multiple R-squared:  0.1155,	Adjusted R-squared:  0.0786 
#F-statistic: 3.132 on 1 and 24 DF,  p-value: 0.08945


op <- par(mfrow=c(2,2))
plot(MCP95a) 
par(op)

par(mfrow=c(1,1))
hist(resid(MCP95a)) #not normal



p <- ggplot(data=TS, aes(Mb, MCP_95_autumn))+
  labs(y= "Home range (m^2)", x = "Body mass (g)") +
  geom_point(size=3) +
  geom_abline(intercept = 3777.53, slope = -107.60, size = 2, color = "blue") + theme_classic()



p + theme(axis.title.x = element_text(size=16, 
                                      color="black", 
                                      face="bold",
                                      angle=0),
          axis.title.y = element_text(size=16, 
                                      color="black", 
                                      face="bold",
                                      angle=90),
          axis.text.x= element_text(family = "Tahoma", 
                                    face="bold", colour="black",
                                    size=14),
          axis.text.y= element_text(family = "Tahoma", 
                                    face="bold", colour="black",
                                    size=14))

