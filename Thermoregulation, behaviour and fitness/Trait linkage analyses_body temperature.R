library(nlme)
library(lme4)
library(ggplot2)
library(coin)

############################ T-TEST ############################


# (categories 'zero' or 'some' offspring; all that had offspring detected were pooled into category 'some' because when I split into 1, 2 or more offspring groups have too few datapoints per group)


TB1 <- read.csv(file="Body temperature_offspring number_t-test_NEW_no outlier.csv", header=TRUE, sep=",")

View(TB1)


## Tb average S:

shapiro.test(TB1$Tb_average_S[TB1$CPI_407_all=="zero"]) #p-value = 0.2956
shapiro.test(TB1$Tb_average_S[TB1$CPI_407_all=="some"]) #p-value = 0.1484

var.test(TB1$Tb_average_S[TB1$CPI_407_all=="zero"],TB1$Tb_average_S[TB1$CPI_407_all=="some"])
#p-value = 0.8162

t.test(TB1$Tb_average_S ~ TB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#Two Sample t-test

#data:  TB1$Tb_average_S by TB1$CPI_407_all
#t = -0.64138, df = 21, p-value = 0.5282
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.9522162  0.5033127
#sample estimates:
#  mean in group some mean in group zero 
#37.00397           37.22842 



# MAF 10:
shapiro.test(TB1$Tb_average_S[TB1$CPI_MAF10_all=="zero"]) #p-value = 0.4167
shapiro.test(TB1$Tb_average_S[TB1$CPI_MAF10_all=="some"]) #p-value = 0.997

var.test(TB1$Tb_average_S[TB1$CPI_MAF10_all=="zero"],TB1$Tb_average_S[TB1$CPI_MAF10_all=="some"])
#p-value = 0.2589

t.test(TB1$Tb_average_S ~ TB1$CPI_MAF10_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#Two Sample t-test

#data:  TB1$Tb_average_S by TB1$CPI_MAF10_all
#t = -0.19188, df = 21, p-value = 0.8497
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.8019589  0.6664702
#sample estimates:
#  mean in group some mean in group zero 
#37.09935           37.16710 


# MAF 20:
shapiro.test(TB1$Tb_average_S[TB1$CPI_MAF20_all=="zero"]) #p-value = 0.9187
shapiro.test(TB1$Tb_average_S[TB1$CPI_MAF20_all=="some"]) #p-value = 0.9804

var.test(TB1$Tb_average_S[TB1$CPI_MAF20_all=="zero"],TB1$Tb_average_S[TB1$CPI_MAF20_all=="some"])
#p-value = 0.5088

t.test(TB1$Tb_average_S ~ TB1$CPI_MAF20_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#Two Sample t-test

#data:  TB1$Tb_average_S by TB1$CPI_MAF20_all
#t = 1.1929, df = 21, p-value = 0.2462
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.2962456  1.0933671
#sample estimates:
#  mean in group some mean in group zero 
#37.33121           36.93265 


TB1$CPI_407_all<-factor(TB1$CPI_407_all, c("zero", "some")) 

ggplot(data = TB1, aes(x=CPI_407_all, y=Tb_average_S)) + 
  labs(y= "Body temperature (?C)", x = "Reproductive success") +
  ggtitle("Average Tb in summer") +
  geom_boxplot(aes(fill=CPI_407_all)) 



TB1$CPI_407_all<-factor(TB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = TB1, aes(x=CPI_407_all, y=Tb_average_S)) + 
  labs(y= "Body temperature", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

TB1$CPI_MAF10_all<-factor(TB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = TB1, aes(x=CPI_MAF10_all, y=Tb_average_S)) + 
  labs(y= "Body temperature", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

TB1$CPI_MAF20_all<-factor(TB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = TB1, aes(x=CPI_MAF20_all, y=Tb_average_S)) + 
  labs(y= "Body temperature", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)





# Boxplots with SE:

library(ggpubr)

Thermo_fitness <- TB1 %>%
  group_by(CPI_407_all)

plot <- ggerrorplot(Thermo_fitness, x = "CPI_407_all", y = "Tb_average_S", 
                    desc_stat = "mean_se",
                    error.plot = "errorbar",            
                    add = "mean",
                    xlab = "Reproductive success",
                    ylab = "Body temperature (?C)", 
                    size = 1)

plot <- plot + scale_x_discrete(limits = c("zero", "some"))

ggpar(plot, ylim = c(36, 38), font.x = c(14, "bold"),
      font.y = c(14, "bold"), font.tickslab = 12)


library(plotrix)

mean(TB1$Tb_average_S[TB1$CPI_407_all=="zero"])
37.22842

std.error(TB1$Tb_average_S[TB1$CPI_407_all=="zero"])
0.2263525



mean(TB1$Tb_average_S[TB1$CPI_407_all=="some"])
37.00397

std.error(TB1$Tb_average_S[TB1$CPI_407_all=="some"])
0.2572311



## Abs dev Topt S:

shapiro.test(TB1$Abs_Dev_Topt_S[TB1$CPI_407_all=="zero"]) #p-value = 0.3458
shapiro.test(TB1$Abs_Dev_Topt_S[TB1$CPI_407_all=="some"]) #p-value = 0.5667

var.test(TB1$Abs_Dev_Topt_S[TB1$CPI_407_all=="zero"],TB1$Abs_Dev_Topt_S[TB1$CPI_407_all=="some"])
#p-value = 0.6422

t.test(TB1$Abs_Dev_Topt_S ~ TB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#	Two Sample t-test

#data:  TB1$Abs_Dev_Topt_S by TB1$CPI_407_all
#t = 1.4887, df = 15, p-value = 0.1573
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.670751  3.777897
#sample estimates:
#  mean in group some mean in group zero 
#5.104937           3.551364 

TB1$CPI_407_all<-factor(TB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = TB1, aes(x=CPI_407_all, y=Abs_Dev_Topt_S)) + 
  labs(y= "Accuracy of thermoreg.", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a




test <- read.csv("Body temperature_offspring number_NEW_no outlier_zero inflated.csv")

m1 <- zeroinfl(CPI_407_all ~ Abs_Dev_Topt_S, data = test)

summary(m1)
#Call:
#  zeroinfl(formula = CPI_407_all ~ Abs_Dev_Topt_S, data = test)

#Pearson residuals:
#  Min         1Q     Median         3Q        Max 
#-1.142e+00 -8.208e-01 -8.735e-09  3.549e-01  1.955e+00 

#Count model coefficients (poisson with log link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)     -0.8862     0.7659  -1.157    0.247
#Abs_Dev_Topt_S   0.1679     0.1275   1.317    0.188

#Zero-inflation model coefficients (binomial with logit link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)       527.9      724.2   0.729    0.466
#Abs_Dev_Topt_S   -241.9      331.5  -0.730    0.466

#Number of iterations in BFGS optimization: 5221 
#Log-likelihood: -17.13 on 4 Df


mean(TB1$Abs_Dev_Topt_S[TB1$CPI_407_all=="zero"], na.rm = TRUE)
3.551364

std.error(TB1$Abs_Dev_Topt_S[TB1$CPI_407_all=="zero"], na.rm = TRUE)
0.6829435



mean(TB1$Abs_Dev_Topt_S[TB1$CPI_407_all=="some"], na.rm = TRUE)
5.104937

std.error(TB1$Abs_Dev_Topt_S[TB1$CPI_407_all=="some"], na.rm = TRUE)
0.7734066



# MAF 10:

shapiro.test(TB1$Abs_Dev_Topt_S[TB1$CPI_MAF10_all=="zero"]) #p-value = 0.8047
shapiro.test(TB1$Abs_Dev_Topt_S[TB1$CPI_MAF10_all=="some"]) #p-value = 0.4644

var.test(TB1$Abs_Dev_Topt_S[TB1$CPI_MAF10_all=="zero"],TB1$Abs_Dev_Topt_S[TB1$CPI_MAF10_all=="some"])
#p-value = 0.1149

t.test(TB1$Abs_Dev_Topt_S ~ TB1$CPI_MAF10_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#	Two Sample t-test

#data:  TB1$Abs_Dev_Topt_S by TB1$CPI_MAF10_all
#t = 1.6152, df = 16, p-value = 0.1258
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.6371914  4.7160554
#sample estimates:
#  mean in group some mean in group zero 
#5.813994           3.774562 



# MAF 20:

shapiro.test(TB1$Abs_Dev_Topt_S[TB1$CPI_MAF20_all=="zero"]) #p-value = 0.6534
shapiro.test(TB1$Abs_Dev_Topt_S[TB1$CPI_MAF20_all=="some"]) #p-value = 0.0611

var.test(TB1$Abs_Dev_Topt_S[TB1$CPI_MAF20_all=="zero"],TB1$Abs_Dev_Topt_S[TB1$CPI_MAF20_all=="some"])
#p-value = 0.135

t.test(TB1$Abs_Dev_Topt_S ~ TB1$CPI_MAF20_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#	Two Sample t-test

#data:  TB1$Abs_Dev_Topt_S by TB1$CPI_MAF20_all
#t = 0.62729, df = 16, p-value = 0.5393
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.020415  3.718643
#sample estimates:
#  mean in group some mean in group zero 
#5.171663           4.322548 


TB1$CPI_407_all<-factor(TB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = TB1, aes(x=CPI_407_all, y=Abs_Dev_Topt_S)) + 
  labs(y= "Accuracy of thermoreg.", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

TB1$CPI_MAF10_all<-factor(TB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = TB1, aes(x=CPI_MAF10_all, y=Abs_Dev_Topt_S)) + 
  labs(y= "Accuracy of thermoreg.", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

TB1$CPI_MAF20_all<-factor(TB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = TB1, aes(x=CPI_MAF20_all, y=Abs_Dev_Topt_S)) + 
  labs(y= "Accuracy of thermoreg.", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)





## Abs dev Tpref S:

shapiro.test(TB1$Abs_Dev_Tpref_S[TB1$CPI_407_all=="zero"]) #p-value = 0.3336
shapiro.test(TB1$Abs_Dev_Tpref_S[TB1$CPI_407_all=="some"]) #p-value = 0.6472

var.test(TB1$Abs_Dev_Tpref_S[TB1$CPI_407_all=="zero"],TB1$Abs_Dev_Tpref_S[TB1$CPI_407_all=="some"])
#p-value = 0.9737

t.test(TB1$Abs_Dev_Tpref_S ~ TB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#	Two Sample t-test

#data:  TB1$Abs_Dev_Tpref_S by TB1$CPI_407_all
#t = 0.35389, df = 21, p-value = 0.727
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.3394819  0.4787148
#sample estimates:
#  mean in group zero mean in group some 
#1.660330           1.590713 



###



## Tb average A:

shapiro.test(TB1$Tb_average_A[TB1$CPI_407_all=="zero"]) #p-value = 0.8298
shapiro.test(TB1$Tb_average_A[TB1$CPI_407_all=="some"]) #p-value = 0.02613

var.test(TB1$Tb_average_A[TB1$CPI_407_all=="zero"],TB1$Tb_average_A[TB1$CPI_407_all=="some"])
#p-value = 0.2801

wilcox.test(TB1$Tb_average_A~TB1$CPI_407_all, mu=0, alt="two.sided", conf.level=0.95,paired=F)
#Wilcoxon rank sum exact test

#data:  TB1$Tb_average_A by TB1$CPI_407_all
#W = 43, p-value = 0.5414
#alternative hypothesis: true location shift is not equal to 0


TB1$CPI_407_all <- as.factor(TB1$CPI_407_all)

wt <- wilcox_test(Tb_average_A~CPI_407_all, data = TB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Tb_average_A by CPI_407_all (some, zero)
#Z = -0.67358, p-value = 0.5414
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -2.424206  1.280952
#sample estimates:
#  difference in location 
#-0.5273016

mean(TB1$Tb_average_A[TB1$CPI_407_all=="zero"], na.rm=TRUE)
#37.5463
mean(TB1$Tb_average_A[TB1$CPI_407_all=="some"], na.rm=TRUE)
#36.98894



# MAF 10:

shapiro.test(TB1$Tb_average_A[TB1$CPI_MAF10_all=="zero"]) #p-value = 0.9761
shapiro.test(TB1$Tb_average_A[TB1$CPI_MAF10_all=="some"]) #p-value = 0.03714

var.test(TB1$Tb_average_A[TB1$CPI_MAF10_all=="zero"],TB1$Tb_average_A[TB1$CPI_MAF10_all=="some"])
#p-value = 0.5202

wilcox.test(TB1$Tb_average_A~TB1$CPI_MAF10_all, mu=0, alt="two.sided", conf.level=0.95,paired=F)
#data:  TB1$Tb_average_A by TB1$CPI_MAF10_all
#W = 17, p-value = 0.07445
#alternative hypothesis: true location shift is not equal to 0


TB1$CPI_MAF10_all <- as.factor(TB1$CPI_MAF10_all)

wt <- wilcox_test(Tb_average_A~CPI_MAF10_all, data = TB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Tb_average_A by CPI_MAF10_all (some, zero)
#Z = -1.8283, p-value = 0.07445
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -2.76916666  0.02579365
#sample estimates:
#  difference in location 
#-1.358958

mean(TB1$Tb_average_A[TB1$CPI_MAF10_all=="zero"], na.rm=TRUE)
#37.98639
mean(TB1$Tb_average_A[TB1$CPI_MAF10_all=="some"], na.rm=TRUE)
#36.65968




# MAF 20:

shapiro.test(TB1$Tb_average_A[TB1$CPI_MAF20_all=="zero"]) #p-value = 0.7841
shapiro.test(TB1$Tb_average_A[TB1$CPI_MAF20_all=="some"]) #p-value = 0.2924

var.test(TB1$Tb_average_A[TB1$CPI_MAF20_all=="zero"],TB1$Tb_average_A[TB1$CPI_MAF20_all=="some"])
#p-value = 0.6851

t.test(TB1$Tb_average_A ~ TB1$CPI_MAF20_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#Two Sample t-test

#data:  TB1$Tb_average_A by TB1$CPI_MAF20_all
#t = -0.21369, df = 15, p-value = 0.8337
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.724961  1.410606
#sample estimates:
#  mean in group some mean in group zero 
#37.21005           37.36722 


ggplot(data = TB1, aes(x=CPI_407_all, y=Tb_average_A)) + 
  labs(y= "Body temperature (?C)", x = "Reproductive success") +
  ggtitle("Average Tb in autumn") +
  geom_boxplot(aes(fill=CPI_407_all)) 




TB1$CPI_407_all<-factor(TB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = TB1, aes(x=CPI_407_all, y=Tb_average_A)) + 
  labs(y= "Body temperature", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

TB1$CPI_MAF10_all<-factor(TB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = TB1, aes(x=CPI_MAF10_all, y=Tb_average_A)) + 
  labs(y= "Body temperature", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

TB1$CPI_MAF20_all<-factor(TB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = TB1, aes(x=CPI_MAF20_all, y=Tb_average_A)) + 
  labs(y= "Body temperature", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)




## Abs dev Topt A:

shapiro.test(TB1$Abs_Dev_Topt_A[TB1$CPI_407_all=="zero"]) #p-value = 0.5656
shapiro.test(TB1$Abs_Dev_Topt_A[TB1$CPI_407_all=="some"]) #p-value = 0.1415

var.test(TB1$Abs_Dev_Topt_A[TB1$CPI_407_all=="zero"],TB1$Abs_Dev_Topt_A[TB1$CPI_407_all=="some"])
#p-value = 0.3333

t.test(TB1$Abs_Dev_Topt_A ~ TB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#	Two Sample t-test

#ddata:  TB1$Abs_Dev_Topt_A by TB1$CPI_407_all
#t = -0.44336, df = 15, p-value = 0.6638
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -3.143096  2.060665
#sample estimates:
#  mean in group zero mean in group some 
#4.805960           5.347175 




# MAF 10:

shapiro.test(TB1$Abs_Dev_Topt_A[TB1$CPI_MAF10_all=="zero"]) #p-value = 0.9631
shapiro.test(TB1$Abs_Dev_Topt_A[TB1$CPI_MAF10_all=="some"]) #p-value = 0.5164

var.test(TB1$Abs_Dev_Topt_A[TB1$CPI_MAF10_all=="zero"],TB1$Abs_Dev_Topt_A[TB1$CPI_MAF10_all=="some"])
#p-value = 0.2295

t.test(TB1$Abs_Dev_Topt_A ~ TB1$CPI_MAF10_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#	Two Sample t-test

#data:  TB1$Abs_Dev_Topt_A by TB1$CPI_MAF10_all
#t = 1.1296, df = 15, p-value = 0.2764
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.181754  3.846500
#sample estimates:
#  mean in group some mean in group zero 
#5.687648           4.355275 



# MAF 20:

shapiro.test(TB1$Abs_Dev_Topt_A[TB1$CPI_MAF20_all=="zero"]) #p-value = 0.9743
shapiro.test(TB1$Abs_Dev_Topt_A[TB1$CPI_MAF20_all=="some"]) #p-value = 0.5993

var.test(TB1$Abs_Dev_Topt_A[TB1$CPI_MAF20_all=="zero"],TB1$Abs_Dev_Topt_A[TB1$CPI_MAF20_all=="some"])
#p-value = 0.1996

t.test(TB1$Abs_Dev_Topt_A ~ TB1$CPI_MAF20_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#	Two Sample t-test

#data:  TB1$Abs_Dev_Topt_A by TB1$CPI_MAF20_all
#t = 1.2276, df = 15, p-value = 0.2385
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.058631  3.934303
#sample estimates:
#  mean in group some mean in group zero 
#5.737278           4.299442 


TB1$CPI_407_all<-factor(TB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = TB1, aes(x=CPI_407_all, y=Abs_Dev_Topt_A)) + 
  labs(y= "Accuracy of thermoreg.", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

TB1$CPI_MAF10_all<-factor(TB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = TB1, aes(x=CPI_MAF10_all, y=Abs_Dev_Topt_A)) + 
  labs(y= "Accuracy of thermoreg.", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

TB1$CPI_MAF20_all<-factor(TB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = TB1, aes(x=CPI_MAF20_all, y=Abs_Dev_Topt_A)) + 
  labs(y= "Accuracy of thermoreg.", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)




## Abs dev Tpref A:

shapiro.test(TB1$Abs_Dev_Tpref_A[TB1$CPI_407_all=="zero"]) #p-value = 0.5298
shapiro.test(TB1$Abs_Dev_Tpref_A[TB1$CPI_407_all=="some"]) #p-value = 0.08869

var.test(TB1$Abs_Dev_Tpref_A[TB1$CPI_407_all=="zero"],TB1$Abs_Dev_Tpref_A[TB1$CPI_407_all=="some"])
#p-value = 0.222

t.test(TB1$Abs_Dev_Tpref_A ~ TB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  TB1$Abs_Dev_Tpref_A by TB1$CPI_407_all
#t = 1.0096, df = 15, p-value = 0.3287
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.4538157  1.2707063
#sample estimates:
#  mean in group zero mean in group some 
#2.338238           1.929792 