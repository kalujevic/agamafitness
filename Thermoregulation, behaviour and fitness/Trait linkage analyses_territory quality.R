library(ggplot2)
library(coin)
library(gridExtra)
library(ggpubr)



############################ T-TEST ############################


# (categories 'zero' or 'some' offspring; all that had offspring detected were pooled into category 'some' because when I split into 1, 2 or more offspring groups have too few datapoints per group)


TQ1 <- read.csv(file="Territory quality_offspring number_aov_2 categories_NEW.csv", header=TRUE, sep=",")

View(TQ1)

## Hours average S:

shapiro.test(TQ1$Hours_average_S[TQ1$CPI_407_all=="zero"]) #p-value = 0.3626
shapiro.test(TQ1$Hours_average_S[TQ1$CPI_407_all=="some"]) # p-value = 0.4519

var.test(TQ1$Hours_average_S[TQ1$CPI_407_all=="zero"],TQ1$Hours_average_S[TQ1$CPI_407_all=="some"])
#p-value = 0.04355

t.test(TQ1$Hours_average_S ~ TQ1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=FALSE, paired=F)
#data:  TQ1$Hours_average_S by TQ1$CPI_407_all
#t = 2.4445, df = 5.8739, p-value = 0.05103
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.009397776  3.034414805
#sample estimates:
#  mean in group some mean in group zero 
#3.293464           1.780955 

library(rstatix)


TQ1 %>% cohens_d(Hours_average_S~CPI_407_all, var.equal = F)
#.y.             group1 group2        effsize    n1    n2 magnitude
#* <chr>           <chr>  <chr>    <dbl> <int> <int> <ord>    
#  1 Hours_average_S some   zero      1.43     6     4 large    

### running the same thing with zero inflated glmm:

#https://stats.oarc.ucla.edu/r/dae/zip/

library(pscl)

test <- read_csv("Territory quality_fitness_test_zero inflated glm.csv")

m1 <- zeroinfl(fitness_all ~ Hours_average_S, data = test)

summary(m1)
#Call:
#  zeroinfl(formula = fitness_all ~ Hours_average_S, data = test)

#Pearson residuals:
#  Min      1Q  Median      3Q     Max 
#-0.8276 -0.6319 -0.2888  0.6729  1.3338 

#Count model coefficients (poisson with log link):
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)      -1.9195     1.0553  -1.819   0.0689 .
#Hours_average_S   0.5712     0.2745   2.081   0.0375 *
  
#  Zero-inflation model coefficients (binomial with logit link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)       -1.516        NaN     NaN      NaN
#Hours_average_S   -7.527        NaN     NaN      NaN
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#Number of iterations in BFGS optimization: 28 
#Log-likelihood: -10.01 on 4 Df


m1.1 <- zeroinfl(fitness_all ~ Hours_average_S+Mb, data = test)

summary(m1.1)
#Count model coefficients (poisson with log link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)      -6.1614     3.8749  -1.590    0.112
#Hours_average_S   0.4392     0.2980   1.473    0.141
#Mb                0.1961     0.1681   1.167    0.243

#Zero-inflation model coefficients (binomial with logit link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)         16.315 220181.086       0        1
#Hours_average_S     -1.208  31559.827       0        1
#Mb                  -1.572   7571.166       0        1

#Number of iterations in BFGS optimization: 13 
#Log-likelihood: -9.283 on 6 Df


#anova(m1,m1.1) --> doesn't work


plot(test$Hours_average_S, test$fitness_all)

#testing without the ind that had 3 offspring:

test1 <- read_csv("Territory quality_fitness_test_zero inflated glm_no3.csv")

m_no3 <- zeroinfl(fitness_all ~ Hours_average_S, data = test1)
summary(m_no3)


#autumn:
m2 <- zeroinfl(fitness_all ~ Hours_average_A, data = test)

summary(m2)
#Count model coefficients (poisson with log link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)      -0.9368     0.5791  -1.618    0.106
#Hours_average_A   0.2612     0.1609   1.623    0.105

#Zero-inflation model coefficients (binomial with logit link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)        29.35     110.05   0.267    0.790
#Hours_average_A   -43.96     157.78  -0.279    0.781

#Number of iterations in BFGS optimization: 862 
#Log-likelihood: -16.64 on 4 Df



#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/


TQ1$CPI_407_all<-factor(TQ1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = TQ1, aes(x=CPI_407_all, y=Hours_average_S)) + 
  labs(y= "Thermal quality", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a 

# Boxplots with SE:


library(ggpubr)

Thermo_fitness <- TQ1 %>%
  group_by(CPI_407_all)

plot <- ggerrorplot(Thermo_fitness, x = "CPI_407_all", y = "Hours_average_S", 
                 desc_stat = "mean_se",
                 error.plot = "errorbar",            
                 add = "mean",
                 xlab = "Reproductive success",
                 ylab = "Thermal quality index", 
                 size = 1)

plot <- plot + scale_x_discrete(limits = c("zero", "some"))

ggpar(plot, ylim = c(1, 4.5), font.x = c(14, "bold"),
      font.y = c(14, "bold"), font.tickslab = 12)





library(plotrix)

mean(TQ1$Hours_average_S[TQ1$CPI_407_all=="zero"], na.rm=TRUE)
#1.780955

std.error(TQ1$Hours_average_S[TQ1$CPI_407_all=="zero"])
#0.1790433



mean(TQ1$Hours_average_S[TQ1$CPI_407_all=="some"], na.rm=TRUE)
#3.293464

std.error(TQ1$Hours_average_S[TQ1$CPI_407_all=="some"])
#0.5922642



##### with MAF10:


shapiro.test(TQ1$Hours_average_S[TQ1$CPI_MAF10_all=="zero"]) #p-value = 005309
shapiro.test(TQ1$Hours_average_S[TQ1$CPI_MAF10_all=="some"]) # p-value = 0.8289

var.test(TQ1$Hours_average_S[TQ1$CPI_MAF10_all=="zero"],TQ1$Hours_average_S[TQ1$CPI_MAF10_all=="some"])
#p-value = 0.963

wilcox.test(TQ1$Hours_average_S~TQ1$CPI_MAF10_all, mu=0, alt="two.sided", conf.level=0.95,paired=F)
#data:  TQ1$Hours_average_S by TQ1$CPI_MAF10_all
#W = 16, p-value = 0.5476


TQ1$CPI_MAF10_all <- as.factor(TQ1$CPI_MAF10_all)

wt <- wilcox_test(Hours_average_S~CPI_MAF10_all, data = TQ1, distribution = "exact", conf.int = TRUE)

wt
#data:  Hours_average_S by CPI_MAF10 (some, zero)
#Z = 0.73113, p-value = 0.5476
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -1.425439  2.724873
#sample estimates:
#  difference in location 
#0.7809759 


mean(TQ1$Hours_average_S[TQ1$CPI_MAF10_all=="zero"], na.rm=TRUE)
#2.315192
mean(TQ1$Hours_average_S[TQ1$CPI_MAF10_all=="some"], na.rm=TRUE)
#3.061729




TQ1$CPI_MAF10_all<-factor(TQ1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = TQ1, aes(x=CPI_MAF10_all, y=Hours_average_S)) + 
  labs(y= "Thermal quality", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

##### with MAF20:


shapiro.test(TQ1$Hours_average_S[TQ1$CPI_MAF20_all=="zero"]) #p-value = 0.009435
shapiro.test(TQ1$Hours_average_S[TQ1$CPI_MAF20_all=="some"]) # p-value = 0.6649

var.test(TQ1$Hours_average_S[TQ1$CPI_MAF20_all=="zero"],TQ1$Hours_average_S[TQ1$CPI_MAF20_all=="some"])
#p-value = 0.5823

wilcox.test(TQ1$Hours_average_S~TQ1$CPI_MAF20_all, mu=0, alt="two.sided", conf.level=0.95,paired=F)
#data:  TQ1$Hours_average_S by TQ1$CPI_MAF20_all
#W = 20, p-value = 0.1508


TQ1$CPI_MAF20_all <- as.factor(TQ1$CPI_MAF20_all)

wt <- wilcox_test(Hours_average_S~CPI_MAF20_all, data = TQ1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Hours_average_S by CPI_MAF20 (some, zero)
#Z = 1.5667, p-value = 0.1508
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -1.425439  3.006808
#sample estimates:
#  difference in location 
#1.100411 


mean(TQ1$Hours_average_S[TQ1$CPI_MAF20_all=="zero"], na.rm=TRUE)
#2.158914
mean(TQ1$Hours_average_S[TQ1$CPI_MAF20_all=="some"], na.rm=TRUE)
#3.218006




TQ1$CPI_MAF20_all<-factor(TQ1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = TQ1, aes(x=CPI_MAF20_all, y=Hours_average_S)) + 
  labs(y= "Thermal quality", x = "Reproductive success") +
    geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)



## DIDN'T CHANGE THIS FOR EACH MICROSITE SEPARATELY, MOVING ONTO AVERAGE IN AUTUMN!

## Hours combined S:

shapiro.test(TQ1$Hours_combined_S[TQ1$CPI_407=="zero"]) #p-value = 0.4146
shapiro.test(TQ1$Hours_combined_S[TQ1$CPI_407=="some"]) # p-value = 0.5675

var.test(TQ1$Hours_combined_S[TQ1$CPI_407=="zero"],TQ1$Hours_combined_S[TQ1$CPI_407=="some"])
#p-value = 0.01225

t.test(TQ1$Hours_combined_S ~ TQ1$CPI_407, mu=0, alt="two.sided", conf=0.95, var.eq=FALSE, paired=F)
#	Welch Two Sample t-test

#data:  TQ1$Hours_combined_S by TQ1$CPI_407
#t = 2.8327, df = 5.3739, p-value = 0.03372
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.7317974 12.4316845
#sample estimates:
#  mean in group some mean in group zero 
#13.386538           6.804797 


ggplot(data = TQ1, aes(x=CPI_407, y=Hours_combined_S)) + 
  labs(y= "Thermal quality", x = "Reproductive success") +
  ggtitle("Combined # hours within individual B80 in summer") +
  geom_boxplot(aes(fill=CPI_407)) 


## Hours crevice S:

shapiro.test(TQ1$Hours_crevice_S[TQ1$CPI_407=="zero"]) #p-value = 0.08967
shapiro.test(TQ1$Hours_crevice_S[TQ1$CPI_407=="some"]) # p-value = 0.6182

var.test(TQ1$Hours_crevice_S[TQ1$CPI_407=="zero"],TQ1$Hours_crevice_S[TQ1$CPI_407=="some"])
#p-value = 0.02349

t.test(TQ1$Hours_crevice_S ~ TQ1$CPI_407, mu=0, alt="two.sided", conf=0.95, var.eq=FALSE, paired=F)
#	Welch Two Sample t-test

#data:  TQ1$Hours_crevice_S by TQ1$CPI_407
#t = 2.3446, df = 5.5785, p-value = 0.06068
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.1107511  3.6239090
#sample estimates:
#  mean in group some mean in group zero 
#2.791118           1.034539 


ggplot(data = TQ1, aes(x=CPI_407, y=Hours_crevice_S)) + 
  labs(y= "Thermal quality", x = "Reproductive success") +
  ggtitle("Crevice # hours within individual B80 in summer") +
  geom_boxplot(aes(fill=CPI_407))


## Hours full sun S:

shapiro.test(TQ1$Hours_full.sun_S[TQ1$CPI_407=="zero"]) #p-value = 0.5781
shapiro.test(TQ1$Hours_full.sun_S[TQ1$CPI_407=="some"]) # p-value = 0.2552

var.test(TQ1$Hours_full.sun_S[TQ1$CPI_407=="zero"],TQ1$Hours_full.sun_S[TQ1$CPI_407=="some"])
#p-value = 0.4262

t.test(TQ1$Hours_full.sun_S ~ TQ1$CPI_407, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#	Two Sample t-test

#data:  TQ1$Hours_full.sun_S by TQ1$CPI_407
#t = 2.5575, df = 8, p-value = 0.03378
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.1024174 1.9803627
#sample estimates:
#  mean in group some mean in group zero 
#2.978753           1.937363 


ggplot(data = TQ1, aes(x=CPI_407, y=Hours_full.sun_S)) + 
  labs(y= "Thermal quality", x = "Reproductive success") +
  ggtitle("Full sun # hours within individual B80 in summer") +
  geom_boxplot(aes(fill=CPI_407))


## Hours partly sun S:

shapiro.test(TQ1$Hours_partly.sun_S[TQ1$CPI_407=="zero"]) #p-value = 0.3446
shapiro.test(TQ1$Hours_partly.sun_S[TQ1$CPI_407=="some"]) # p-value = 0.6786

var.test(TQ1$Hours_partly.sun_S[TQ1$CPI_407=="zero"],TQ1$Hours_partly.sun_S[TQ1$CPI_407=="some"])
#p-value = 0.2078

t.test(TQ1$Hours_partly.sun_S ~ TQ1$CPI_407, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#	Two Sample t-test

#data:  TQ1$Hours_partly.sun_S by TQ1$CPI_407
#t = 2.5137, df = 8, p-value = 0.03616
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.1631183 3.7860046
#sample estimates:
#  mean in group some mean in group zero 
#4.070614           2.096053 


ggplot(data = TQ1, aes(x=CPI_407, y=Hours_partly.sun_S)) + 
  labs(y= "Thermal quality", x = "Reproductive success") +
  ggtitle("Partly sun # hours within individual B80 in summer") +
  geom_boxplot(aes(fill=CPI_407))


## Hours shade S:

shapiro.test(TQ1$Hours_shade_S[TQ1$CPI_407=="zero"]) #p-value = 0.5629
shapiro.test(TQ1$Hours_shade_S[TQ1$CPI_407=="some"]) # p-value = 0.6237

var.test(TQ1$Hours_shade_S[TQ1$CPI_407=="zero"],TQ1$Hours_shade_S[TQ1$CPI_407=="some"])
#p-value = 0.1187

t.test(TQ1$Hours_shade_S ~ TQ1$CPI_407, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#	Two Sample t-test

#data:  TQ1$Hours_shade_S by TQ1$CPI_407
#t = 1.8313, df = 8, p-value = 0.1044
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.4690022  4.0874233
#sample estimates:
#  mean in group some mean in group zero 
#3.546053           1.736842 


#######


## Hours average A:

shapiro.test(TQ1$Hours_average_A[TQ1$CPI_407_all=="zero"]) #p-value = 0.003784
shapiro.test(TQ1$Hours_average_A[TQ1$CPI_407_all=="some"]) # p-value = 0.6247

var.test(TQ1$Hours_average_A[TQ1$CPI_407_all=="zero"],TQ1$Hours_average_A[TQ1$CPI_407_all=="some"])
#p-value = 0.5079

wilcox.test(TQ1$Hours_average_A~TQ1$CPI_407_all, mu=0, alt="two.sided", conf.level=0.95,paired=F)
#Wilcoxon rank sum test

#data:  TQ1$Hours_average_A by TQ1$CPI_407_all
#W = 43, p-value = 0.2786


TQ1$CPI_407_all <- as.factor(TQ1$CPI_407_all)

wt <- wilcox_test(Hours_average_A~CPI_407_all, data = TQ1,
                 distribution = "exact", conf.int = TRUE)
wt
#data:  Hours_average_A by CPI_407_all (some, zero)
#Z = 1.1552, p-value = 0.2786
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.5373575  2.5476852
#sample estimates:
#  difference in location 
#0.7630253 


mean(TQ1$Hours_average_A[TQ1$CPI_407_all=="zero"], na.rm=TRUE)
#1.972954
mean(TQ1$Hours_average_A[TQ1$CPI_407_all=="some"], na.rm=TRUE)
#2.560702




# MAF 10:

shapiro.test(TQ1$Hours_average_A[TQ1$CPI_MAF10_all=="zero"]) #p-value = 0.1735
shapiro.test(TQ1$Hours_average_A[TQ1$CPI_MAF10_all=="some"]) # p-value = 0.566

var.test(TQ1$Hours_average_A[TQ1$CPI_MAF10_all=="zero"],TQ1$Hours_average_A[TQ1$CPI_MAF10_all=="some"])
#p-value = 0.05042

t.test(TQ1$Hours_average_A ~ TQ1$CPI_MAF10_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  TQ1$Hours_average_A by TQ1$CPI_MAF10_all
#t = 2.0713, df = 14, p-value = 0.05729
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.05342943  3.06512275
#sample estimates:
#  mean in group some mean in group zero 
#3.019751           1.513904 


# MAF 20:

shapiro.test(TQ1$Hours_average_A[TQ1$CPI_MAF20_all=="zero"]) #p-value = 0.09739
shapiro.test(TQ1$Hours_average_A[TQ1$CPI_MAF20_all=="some"]) # p-value = 0.369

var.test(TQ1$Hours_average_A[TQ1$CPI_MAF20_all=="zero"],TQ1$Hours_average_A[TQ1$CPI_MAF20_all=="some"])
#p-value = 0.03341

t.test(TQ1$Hours_average_A ~ TQ1$CPI_MAF20_all, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
#data:  TQ1$Hours_average_A by TQ1$CPI_MAF20_all
#t = 3.0823, df = 10.867, p-value = 0.01057
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.5340235 3.2154844
#sample estimates:
#  mean in group some mean in group zero 
#3.087033           1.212279 



TQ1$CPI_407_all<-factor(TQ1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = TQ1, aes(x=CPI_407_all, y=Hours_average_A)) + 
  labs(y= "Thermal quality", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

TQ1$CPI_MAF10_all<-factor(TQ1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = TQ1, aes(x=CPI_MAF10_all, y=Hours_average_A)) + 
  labs(y= "Thermal quality", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

TQ1$CPI_MAF20_all<-factor(TQ1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = TQ1, aes(x=CPI_MAF20_all, y=Hours_average_A)) + 
  labs(y= "Thermal quality", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)

