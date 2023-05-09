library(nlme)
library(lme4)
library(ggplot2)

############################ T-TEST ############################


# (categories 'zero' or 'some' offspring; all that had offspring detected were pooled into category 'some' because when I split into 1, 2 or more offspring groups have too few datapoints per group)


FB1 <- read.csv(file="Field behaviour_offspring number_t-test_NEW_no outlier.csv", header=TRUE, sep=",")

View(FB1)


## Thermoreg S:

shapiro.test(FB1$Sum_Thermoregulation_S[FB1$CPI_407_all=="zero"]) #p-value = 0.7634
shapiro.test(FB1$Sum_Thermoregulation_S[FB1$CPI_407_all=="some"]) #p-value = 0.6707

var.test(FB1$Sum_Thermoregulation_S[FB1$CPI_407_all=="zero"],FB1$Sum_Thermoregulation_S[FB1$CPI_407_all=="some"])
#p-value = 0.3829

t.test(FB1$Sum_Thermoregulation_S ~ FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  FB1$Sum_Thermoregulation_S by FB1$CPI_407_all
#t = -0.11341, df = 23, p-value = 0.9107
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.06402367  0.05736884
#sample estimates:
#  mean in group some mean in group zero 
#0.8317143          0.8350417


FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Sum_Thermoregulation_S)) + 
  labs(y= "Thermoregulation", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a


library(plotrix)

mean(FB1$Sum_Thermoregulation_S[FB1$CPI_407_all=="zero"], na.rm=TRUE)
0.8350417

std.error(FB1$Sum_Thermoregulation_S[FB1$CPI_407_all=="zero"])
0.01589438



mean(FB1$Sum_Thermoregulation_S[FB1$CPI_407_all=="some"], na.rm=TRUE)
0.8317143

std.error(FB1$Sum_Thermoregulation_S[FB1$CPI_407_all=="some"])
0.02723838


# DIDN'T CHANGE THIS!

# Boxplots with SE:

library(officer)
library(tidyverse)
library(rvg)
library(dplyr)

# standard error: sd(x)/sqrt(length(x))

##############################################
stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

library(plotrix)

Thermo_fitness <- FB1 %>%
  group_by(CPI_407) %>%
  summarise(se = std.error(Sum_Thermoregulation_S, na.rm = T),
            mean = mean(Sum_Thermoregulation_S, na.rm = T)
  )


theme_set(theme_classic())

Thermo_fitness.plot <- ggplot(
  Thermo_fitness, 
  aes(x = CPI_407, y = mean, ymin = mean-se, ymax = mean+se)
)


Thermo_fitness_plot <- Thermo_fitness.plot + geom_errorbar(width = 0.2, size = 1) +
  geom_point(size = 2) + 
  labs(x = "Reproductive success", y = "Time spent thermoregulating (%)") +
  ylim(0.75,0.9) + 
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12, face="bold"))

Thermo_fitness_plot

#################################

# USE THIS ONE INSTEAD:

library(ggpubr)

Thermo_fitness <- FB1 %>%
  group_by(CPI_407)

p <- ggerrorplot(Thermo_fitness, x = "CPI_407", y = "Sum_Thermoregulation_S", 
            desc_stat = "mean_se",
            error.plot = "errorbar",            
            add = "mean",
            xlab = "Reproductive success",
            ylab = "Time spent thermoregulating (%)", 
            size = 1)

ggpar(p, ylim = c(0.75, 0.9), font.x = c(14, "bold"),
      font.y = c(14, "bold"), font.tickslab = 12)


#CONTINUED CHANGING FROM HERE:

## MAF10:

shapiro.test(FB1$Sum_Thermoregulation_S[FB1$CPI_MAF10_all=="zero"]) #p-value = 0.9847
shapiro.test(FB1$Sum_Thermoregulation_S[FB1$CPI_MAF10_all=="some"]) #p-value = 0.9861

var.test(FB1$Sum_Thermoregulation_S[FB1$CPI_MAF10_all=="zero"],FB1$Sum_Thermoregulation_S[FB1$CPI_MAF10_all=="some"])
#p-value = 0.1812

t.test(FB1$Sum_Thermoregulation_S ~ FB1$CPI_MAF10_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#Two Sample t-test

#data:  FB1$Sum_Thermoregulation_S by FB1$CPI_MAF10_all
#t = -0.11597, df = 23, p-value = 0.9087
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.06280310  0.05613526
#sample estimates:
#  mean in group some mean in group zero 
#0.8318435          0.8351774 


## MAF20:

shapiro.test(FB1$Sum_Thermoregulation_S[FB1$CPI_MAF20_all=="zero"]) #p-value = 0.9871
shapiro.test(FB1$Sum_Thermoregulation_S[FB1$CPI_MAF20_all=="some"]) #p-value = 0.9992

var.test(FB1$Sum_Thermoregulation_S[FB1$CPI_MAF20_all=="zero"],FB1$Sum_Thermoregulation_S[FB1$CPI_MAF20_all=="some"])
#p-value = 0.4095

t.test(FB1$Sum_Thermoregulation_S ~ FB1$CPI_MAF20_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#Two Sample t-test

#data:  FB1$Sum_Thermoregulation_S by FB1$CPI_MAF20_all
#t = -0.24303, df = 23, p-value = 0.8101
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.06552222  0.05174517
#sample estimates:
#  mean in group some mean in group zero 
#0.8308129          0.8377014 


FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Sum_Thermoregulation_S)) + 
  labs(y= "Thermoregulation", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Sum_Thermoregulation_S)) + 
  labs(y= "Thermoregulation", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Sum_Thermoregulation_S)) + 
  labs(y= "Thermoregulation", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)




## Activity S:

shapiro.test(FB1$Sum_activity_S[FB1$CPI_407_all=="zero"]) #p-value = 0.128
shapiro.test(FB1$Sum_activity_S[FB1$CPI_407_all=="some"]) #p-value = 0.9203

var.test(FB1$Sum_activity_S[FB1$CPI_407_all=="zero"],FB1$Sum_activity_S[TB1$CPI_407_all=="some"])
#p-value = 0.06941

t.test(FB1$Sum_activity_S ~ FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#Two Sample t-test

#data:  FB1$Sum_activity_S by FB1$CPI_407_all
#t = 1.6905, df = 23, p-value = 0.1044
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.01845086  0.18341764
#sample estimates:
#  mean in group some mean in group zero 
#0.3317826          0.2492993 


FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Sum_activity_S)) + 
  labs(y= "Activity", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Sum_activity_S)) + 
  labs(y= "Activity", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Sum_activity_S)) + 
  labs(y= "Activity", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)




## Displays S:

shapiro.test(FB1$Sum_displays_S[FB1$CPI_407_all=="zero"]) #p-value = 0.6519
shapiro.test(FB1$Sum_displays_S[FB1$CPI_407_all=="some"]) #p-value = 0.5673

var.test(FB1$Sum_displays_S[FB1$CPI_407_all=="zero"],FB1$Sum_displays_S[FB1$CPI_407_all=="some"])
#p-value = 0.6484

t.test(FB1$Sum_displays_S ~ FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#Two Sample t-test

#data:  FB1$Sum_displays_S by FB1$CPI_407_all
#t = 1.9073, df = 22, p-value = 0.06962
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.04303917  1.02879717
#sample estimates:
#  mean in group some mean in group zero 


FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Sum_displays_S)) + 
  labs(y= "Displays", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Sum_displays_S)) + 
  labs(y= "Displays", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Sum_displays_S)) + 
  labs(y= "Displays", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)




###


## Thermoreg A:

shapiro.test(FB1$Sum_Thermoregulation_A[FB1$CPI_407_all=="zero"]) #p-value = 0.1667
shapiro.test(FB1$Sum_Thermoregulation_A[FB1$CPI_407_all=="some"]) #p-value = 0.7787

var.test(FB1$Sum_Thermoregulation_A[FB1$CPI_407_all=="zero"],FB1$Sum_Thermoregulation_A[FB1$CPI_407_all=="some"])
#p-value = 0.5215

t.test(FB1$Sum_Thermoregulation_A ~ FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#Two Sample t-test

#data:  FB1$Sum_Thermoregulation_A by FB1$CPI_407_all
#t = -0.095173, df = 14, p-value = 0.9255
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.06112923  0.05593462
#sample estimates:
#  mean in group some mean in group zero 
#0.9295273          0.9321246 



# MAF 10:

shapiro.test(FB1$Sum_Thermoregulation_A[FB1$CPI_MAF10_all=="zero"]) #p-value = 0.01956
shapiro.test(FB1$Sum_Thermoregulation_A[FB1$CPI_MAF10_all=="some"]) #p-value = 0.1515

var.test(FB1$Sum_Thermoregulation_A[FB1$CPI_MAF10_all=="zero"],FB1$Sum_Thermoregulation_A[FB1$CPI_MAF10_all=="some"])
#p-value = 0.8682

wilcox.test(FB1$Sum_Thermoregulation_A ~ FB1$CPI_MAF10_all, mu=0, alt="two.sided", conf.level=0.95,paired=F)
#data:  FB1$Sum_Thermoregulation_A by FB1$CPI_MAF10_all
#W = 18, p-value = 0.1738
#alternative hypothesis: true location shift is not equal to 0


FB1$CPI_MAF10_all <- as.factor(FB1$CPI_MAF10_all)

wt <- wilcox_test(Sum_Thermoregulation_A~CPI_MAF10_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Sum_Thermoregulation_A by CPI_MAF10_all (some, zero)
#Z = -1.429, p-value = 0.1738
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.07242252  0.06560176
#sample estimates:
#  difference in location 
#-0.03869668 



mean(FB1$Sum_Thermoregulation_A[FB1$CPI_MAF10_all=="zero"], na.rm=TRUE)
#0.9410353
mean(FB1$Sum_Thermoregulation_A[FB1$CPI_MAF10_all=="some"], na.rm=TRUE)
#0.9228854



# MAF 20:

shapiro.test(FB1$Sum_Thermoregulation_A[FB1$CPI_MAF20_all=="zero"]) #p-value = 0.09116
shapiro.test(FB1$Sum_Thermoregulation_A[FB1$CPI_MAF20_all=="some"]) #p-value = 0.2428

var.test(FB1$Sum_Thermoregulation_A[FB1$CPI_MAF20_all=="zero"],FB1$Sum_Thermoregulation_A[FB1$CPI_MAF20_all=="some"])
#p-value = 0.6531

FB1$CPI_MAF20_all <- as.factor(FB1$CPI_MAF20_all)

wt <- wilcox_test(Sum_Thermoregulation_A~CPI_MAF20_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Sum_Thermoregulation_A by CPI_MAF20_all (some, zero)
#Z = -0.94519, p-value = 0.3823
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.06119651  0.05164330
#sample estimates:
#  difference in location 
#-0.03154511  


mean(FB1$Sum_Thermoregulation_A[FB1$CPI_MAF20_all=="zero"], na.rm=TRUE)
#0.9362687
mean(FB1$Sum_Thermoregulation_A[FB1$CPI_MAF20_all=="some"], na.rm=TRUE)
#0.9253833



FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Sum_Thermoregulation_A)) + 
  labs(y= "Thermoregulation", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Sum_Thermoregulation_A)) + 
  labs(y= "Thermoregulation", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Sum_Thermoregulation_A)) + 
  labs(y= "Thermoregulation", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)



## Activity A:

shapiro.test(FB1$Sum_activity_A[FB1$CPI_407_all=="zero"]) #p-value = 0.2141
shapiro.test(FB1$Sum_activity_A[FB1$CPI_407_all=="some"]) #p-value = 0.4014

var.test(FB1$Sum_activity_A[FB1$CPI_407_all=="zero"],FB1$Sum_activity_A[TB1$CPI_407_all=="some"])
#p-value = 0.1926

t.test(FB1$Sum_activity_A~FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  FB1$Sum_activity_A by FB1$CPI_407_all
#t = 0.856, df = 14, p-value = 0.4064
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.06243825  0.14538033
#sample estimates:
#  mean in group some mean in group zero 
#0.2206179          0.1791468 


FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Sum_activity_A)) + 
  labs(y= "Activity", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Sum_activity_A)) + 
  labs(y= "Activity", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Sum_activity_A)) + 
  labs(y= "Activity", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)





## Displays A:

shapiro.test(FB1$Sum_displays_A[FB1$CPI_407_all=="zero"]) #p-value = 0.4826
shapiro.test(FB1$Sum_displays_A[FB1$CPI_407_all=="some"]) #p-value = 0.06552

var.test(FB1$Sum_displays_A[FB1$CPI_407_all=="zero"],FB1$Sum_displays_A[TB1$CPI_407_all=="some"])
#p-value = 0.01871

t.test(FB1$Sum_displays_A~FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
#data:  FB1$Sum_displays_A by FB1$CPI_407_all
#t = 1.2311, df = 9.1596, p-value = 0.249
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.2311718  0.7864047
#sample estimates:
#  mean in group some mean in group zero 
#0.9167212          0.6391047


FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Sum_displays_A)) + 
  labs(y= "Displays", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Sum_displays_A)) + 
  labs(y= "Displays", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Sum_displays_A)) + 
  labs(y= "Displays", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)





###

#Thigmothermy Summer:
  
  
shapiro.test(FB1$Thigmothermy_S[FB1$CPI_407_all=="zero"]) #p-value = 0.1329
shapiro.test(FB1$Thigmothermy_S[FB1$CPI_407_all=="some"]) #p-value = 0.3388

var.test(FB1$Thigmothermy_S[FB1$CPI_407_all=="zero"],FB1$Thigmothermy_S[FB1$CPI_407_all=="some"])
#p-value = 0.5866

t.test(FB1$Thigmothermy_S ~ FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  FB1$Thigmothermy_S by FB1$CPI_407_all
#t = 0.78671, df = 23, p-value = 0.4395
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.1067402  0.2377508
#sample estimates:
#  mean in group some mean in group zero 
#0.4622341          0.3967288 

FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Thigmothermy_S)) + 
  labs(y= "Time spent in thgmothermy", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a


#MAF 10:
shapiro.test(FB1$Thigmothermy_S[FB1$CPI_MAF10_all=="zero"]) #p-value = 0.366
shapiro.test(FB1$Thigmothermy_S[FB1$CPI_MAF10_all=="some"]) #p-value = 0.7222

var.test(FB1$Thigmothermy_S[FB1$CPI_MAF10_all=="zero"],FB1$Thigmothermy_S[FB1$CPI_MAF10_all=="some"])
#p-value = 0.8355

t.test(FB1$Thigmothermy_S ~ FB1$CPI_MAF10_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  FB1$Thigmothermy_S by FB1$CPI_MAF10_all
#t = 0.54453, df = 23, p-value = 0.5913
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.1251986  0.2146598
#sample estimates:
#  mean in group some mean in group zero 
#0.4471491          0.4024185 


FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Thigmothermy_S)) + 
  labs(y= "Thigmothermy", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Thigmothermy_S)) + 
  labs(y= "Thigmothermy", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Thigmothermy_S)) + 
  labs(y= "Thigmothermy", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)




#Thigmothermy autumn:


shapiro.test(FB1$Thigmothermy_A[FB1$CPI_407_all=="zero"]) #p-value = 0.1915
shapiro.test(FB1$Thigmothermy_A[FB1$CPI_407_all=="some"]) #p-value = 0.2936

var.test(FB1$Thigmothermy_A[FB1$CPI_407_all=="zero"],FB1$Thigmothermy_A[FB1$CPI_407_all=="some"])
#p-value = 0.9775

t.test(FB1$Thigmothermy_A ~ FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  FB1$Thigmothermy_A by FB1$CPI_407_all
#t = 0.30092, df = 14, p-value = 0.7679
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.3076474  0.4080633
#sample estimates:
#  mean in group some mean in group zero 
#0.6419027          0.5916948 



#MAF 10:
shapiro.test(FB1$Thigmothermy_A[FB1$CPI_MAF10_all=="zero"]) #p-value = 0.9257
shapiro.test(FB1$Thigmothermy_A[FB1$CPI_MAF10_all=="some"]) #p-value = 0.04476

var.test(FB1$Thigmothermy_A[FB1$CPI_MAF10_all=="zero"],FB1$Thigmothermy_A[FB1$CPI_MAF10_all=="some"])
#p-value = 0.7711

wilcox.test(FB1$Thigmothermy_A~FB1$CPI_MAF10_all, mu=0, alt="two.sided", conf.level=0.95,paired=F)
#data:  FB1$Thigmothermy_A by FB1$CPI_MAF10_all
#W = 41.5, p-value = 0.3143
#alternative hypothesis: true location shift is not equal to 0


FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Thigmothermy_A)) + 
  labs(y= "Thigmothermy", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Thigmothermy_A)) + 
  labs(y= "Thigmothermy", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Thigmothermy_A)) + 
  labs(y= "Thigmothermy", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)



#Basking Summer:


shapiro.test(FB1$Basking_S[FB1$CPI_407_all=="zero"]) #p-value = 0.05165
shapiro.test(FB1$Basking_S[FB1$CPI_407_all=="some"]) #p-value = 0.895

var.test(FB1$Basking_S[FB1$CPI_407_all=="zero"],FB1$Basking_S[FB1$CPI_407_all=="some"])
#p-value = 0.4395

t.test(FB1$Basking_S ~ FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  FB1$Basking_S by FB1$CPI_407_all
#t = -1.6994, df = 23, p-value = 0.1027
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.23200851  0.02273415
#sample estimates:
#  mean in group zero mean in group some 
#0.1861077          0.2907449 

FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Basking_S)) + 
  labs(y= "Basking", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a


#Head bobbing Summer:


shapiro.test(FB1$Head.bobbing_S[FB1$CPI_407_all=="zero"]) #p-value = 0.02385
shapiro.test(FB1$Head.bobbing_S[FB1$CPI_407_all=="some"]) #p-value = 0.3905

var.test(FB1$Head.bobbing_S[FB1$CPI_407_all=="zero"],FB1$Head.bobbing_S[FB1$CPI_407_all=="some"])
#p-value = 0.0005779

wilcox.test(FB1$Head.bobbing_S ~ FB1$CPI_407_all, mu=0, alt="two.sided", conf.level=0.95,paired=F)
#data:  FB1$Head.bobbing_S by FB1$CPI_407_all
#W = 27, p-value = 0.02297
#alternative hypothesis: true location shift is not equal to 0

wt <- wilcox_test(Head.bobbing_S~CPI_407_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Head.bobbing_S by CPI_407_all (zero, some)
#Z = -2.2658, p-value = 0.02297
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.217409277 -0.009713048
#sample estimates:
#  difference in location 
#-0.08462863


library(rstatix)

FB1 %>% cohens_d(Head.bobbing_S~CPI_407_all, var.equal = F)
#.y.            group1 group2 effsize    n1    n2 magnitude
#* <chr>          <chr>  <chr>    <dbl> <int> <int> <ord>    
#  1 Head.bobbing_S some   zero      1.15     8    16 large  

### trying this with zero inflated glms:

library(pscl)

test <- read_csv("Field behaviour_offspring number_NEW_no outlier_test_zero inflated.csv")

m1 <- zeroinfl(CPI_407_all ~ `Head bobbing_S` , data = test)

summary(m1)
#Count model coefficients (poisson with log link):
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)        1.0058     0.4316   2.330   0.0198 *
#  `Head bobbing_S`  -1.8012     2.2557  -0.799   0.4246  

#Zero-inflation model coefficients (binomial with logit link):
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)        2.3194     0.9734   2.383   0.0172 *
#  `Head bobbing_S` -19.2192     9.9774  -1.926   0.0541 .




mean(FB1$Head.bobbing_S[FB1$CPI_407_all=="zero"], na.rm=TRUE)
#0.05644568
mean(FB1$Head.bobbing_S[FB1$CPI_407_all=="some"], na.rm=TRUE)
#0.1686493



library(ggpubr)
library(plotrix)

Thermo_fitness <- FB1 %>%
  group_by(CPI_407_all)

p <- ggerrorplot(Thermo_fitness, x = "CPI_407_all", y = "Head.bobbing_S", 
                 desc_stat = "mean_se",
                 error.plot = "errorbar",            
                 add = "mean",
                 xlab = "Reproductive success",
                 ylab = "Head bobbing rate (min-1)", 
                 size = 1)

ggpar(p, font.x = c(14, "bold"),
      font.y = c(14, "bold"), font.tickslab = 12)



mean(FB1$Head.bobbing_S[FB1$CPI_407_all=="zero"], na.rm=TRUE)
0.05644568

std.error(FB1$Head.bobbing_S[FB1$CPI_407_all=="zero"])
0.01123418



mean(FB1$Head.bobbing_S[FB1$CPI_407_all=="some"], na.rm=TRUE)
0.1686493

std.error(FB1$Head.bobbing_S[FB1$CPI_407_all=="some"])
0.04629942

library(coin)


# MAF 10:

shapiro.test(FB1$Head.bobbing_S[FB1$CPI_MAF10_all=="zero"]) #p-value = 0.009464
shapiro.test(FB1$Head.bobbing_S[FB1$CPI_MAF10_all=="some"]) #p-value = 0.002922

var.test(FB1$Head.bobbing_S[FB1$CPI_MAF10_all=="zero"],FB1$Head.bobbing_S[FB1$CPI_MAF10_all=="some"])
#p-value = 0.1679

FB1$CPI_MAF10_all <- as.factor(FB1$CPI_MAF10_all)

wt <- wilcox_test(Head.bobbing_S~CPI_MAF10_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Head.bobbing_S by CPI_MAF10_all (some, zero)
#Z = -0.49923, p-value = 0.6433
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.05685891  0.06330752
#sample estimates:
#  difference in location 
#-0.00719758 

mean(FB1$Head.bobbing_S[FB1$CPI_MAF10_all=="zero"], na.rm=TRUE)
#0.0876239
mean(FB1$Head.bobbing_S[FB1$CPI_MAF10_all=="some"], na.rm=TRUE)
#0.1042185


# MAF 20:

shapiro.test(FB1$Head.bobbing_S[FB1$CPI_MAF20_all=="zero"]) #p-value = 0.003988
shapiro.test(FB1$Head.bobbing_S[FB1$CPI_MAF20_all=="some"]) #p-value = 0.0006959

var.test(FB1$Head.bobbing_S[FB1$CPI_MAF20_all=="zero"],FB1$Head.bobbing_S[FB1$CPI_MAF20_all=="some"])
#p-value = 0.6148

FB1$CPI_MAF20_all <- as.factor(FB1$CPI_MAF20_all)

wt <- wilcox_test(Head.bobbing_S~CPI_MAF20_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Head.bobbing_S by CPI_MAF20_all (some, zero)
#Z = -0.76643, p-value = 0.4668
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.05685891  0.06330752
#sample estimates:
#  difference in location 
#-0.01586499 


mean(FB1$Head.bobbing_S[FB1$CPI_MAF20_all=="zero"], na.rm=TRUE)
#0.09456082
mean(FB1$Head.bobbing_S[FB1$CPI_MAF20_all=="some"], na.rm=TRUE)
#0.09324279


FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Head.bobbing_S)) + 
  labs(y= "Head bobbing", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Head.bobbing_S)) + 
  labs(y= "Head bobbing", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Head.bobbing_S)) + 
  labs(y= "Head bobbing", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)




#Push ups Summer:


shapiro.test(FB1$Push.ups_S[FB1$CPI_407_all=="zero"]) #p-value = 0.2458
shapiro.test(FB1$Push.ups_S[FB1$CPI_407_all=="some"]) #p-value = 0.4459

var.test(FB1$Push.ups_S[FB1$CPI_407_all=="zero"],FB1$Push.ups_S[FB1$CPI_407_all=="some"])
#p-value = 0.1777

t.test(FB1$Push.ups_S ~ FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  FB1$Push.ups_S by FB1$CPI_407_all
#t = -2.9238, df = 22, p-value = 0.007864
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.0546339 -0.1793521
#sample estimates:
#  mean in group zero mean in group some 
#0.841484           1.458477 

FB1 %>% cohens_d(Push.ups_S~CPI_407_all, var.equal = T)
#.y.        group1 group2 effsize    n1    n2 magnitude
#* <chr>      <chr>  <chr>    <dbl> <int> <int> <ord>    
#  1 Push.ups_S some   zero      1.27     8    16 large   


#trying with zero inflated glms:
m2 <- zeroinfl(CPI_407_all ~ `Push-ups_S`, data = test)

summary(m2)

#Call:
#  zeroinfl(formula = CPI_407_all ~ `Push-ups_S`, data = test)

#Pearson residuals:
#  Min       1Q   Median       3Q      Max 
#-1.07470 -0.47573 -0.22681  0.01947  5.73776 

#Count model coefficients (poisson with log link):
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)    1.7946     0.7221   2.485   0.0129 *
#  `Push-ups_S`  -0.8306     0.5177  -1.605   0.1086  

#Zero-inflation model coefficients (binomial with logit link):
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)     5.971      2.785   2.144   0.0321 *
#  `Push-ups_S`   -5.298      2.692  -1.968   0.0491 *


mean(FB1$Push.ups_S[FB1$CPI_407_all=="zero"], na.rm=TRUE)
0.841484

std.error(FB1$Push.ups_S[FB1$CPI_407_all=="zero"])
0.1030624



mean(FB1$Push.ups_S[FB1$CPI_407_all=="some"], na.rm=TRUE)
1.458477

std.error(FB1$Push.ups_S[FB1$CPI_407_all=="some"])
0.2185925



#MAF10:


shapiro.test(FB1$Push.ups_S[FB1$CPI_MAF10_all=="zero"]) #p-value = 0.0384
shapiro.test(FB1$Push.ups_S[FB1$CPI_MAF10_all=="some"]) #p-value = 0.9767

var.test(FB1$Push.ups_S[FB1$CPI_MAF10_all=="zero"],FB1$Push.ups_S[FB1$CPI_MAF10_all=="some"])
#p-value = 0.9833

FB1$CPI_MAF10_all <- as.factor(FB1$CPI_MAF10_all)

wt <- wilcox_test(Push.ups_S~CPI_MAF10_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Push.ups_S by CPI_MAF10_all (some, zero)
#Z = -0.22188, p-value = 0.849
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.5118465  0.4156494
#sample estimates:
#  difference in location 
#-0.08306695 


mean(FB1$Push.ups_S[FB1$CPI_MAF10_all=="zero"], na.rm=TRUE)
#1.04458
mean(FB1$Push.ups_S[FB1$CPI_MAF10_all=="some"], na.rm=TRUE)
#1.051429

#MAF20:


shapiro.test(FB1$Push.ups_S[FB1$CPI_MAF20_all=="zero"]) #p-value = 0.008578
shapiro.test(FB1$Push.ups_S[FB1$CPI_MAF20_all=="some"]) #p-value = 0.872

var.test(FB1$Push.ups_S[FB1$CPI_MAF20_all=="zero"],FB1$Push.ups_S[FB1$CPI_MAF20_all=="some"])
#p-value = 0.7261

wt <- wilcox_test(Push.ups_S~CPI_MAF20_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Push.ups_S by CPI_MAF20_all (some, zero)
#Z = 0.43796, p-value = 0.6867
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.3541527  0.5477505
#sample estimates:
#  difference in location 
#0.09870374 

mean(FB1$Push.ups_S[FB1$CPI_MAF20_all=="zero"], na.rm=TRUE)
#0.9748375
mean(FB1$Push.ups_S[FB1$CPI_MAF20_all=="some"], na.rm=TRUE)
#1.108334


FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Push.ups_S)) + 
  labs(y= "Push ups", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Push.ups_S)) + 
  labs(y= "Push ups", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Push.ups_S)) + 
  labs(y= "Push ups", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)




#Head bobbing Autumn:


shapiro.test(FB1$Head.bobbing_A[FB1$CPI_407_all=="zero"]) #p-value = 0.07391
shapiro.test(FB1$Head.bobbing_A[FB1$CPI_407_all=="some"]) #p-value = 0.01739

var.test(FB1$Head.bobbing_A[FB1$CPI_407_all=="zero"],FB1$Head.bobbing_A[FB1$CPI_407_all=="some"])
#p-value = 0.8778

FB1$CPI_407_all <- as.factor(FB1$CPI_407_all)

wt <- wilcox_test(Head.bobbing_A~CPI_407_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Head.bobbing_A by CPI_407_all (some, zero)
#Z = -0.31599, p-value = 0.798
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.07177034  0.03032581
#sample estimates:
#  difference in location 
#-0.004906205 


mean(FB1$Head.bobbing_A[FB1$CPI_407_all=="zero"], na.rm=TRUE)
#0.05213922
mean(FB1$Head.bobbing_A[FB1$CPI_407_all=="some"], na.rm=TRUE)
#0.04239143


# MAF 10:

shapiro.test(FB1$Head.bobbing_A[FB1$CPI_MAF10_all=="zero"]) #p-value = 0.3383
shapiro.test(FB1$Head.bobbing_A[FB1$CPI_MAF10_all=="some"]) #p-value = 0.01141

var.test(FB1$Head.bobbing_A[FB1$CPI_MAF10_all=="zero"],FB1$Head.bobbing_A[FB1$CPI_MAF10_all=="some"])
#p-value = 0.6837

FB1$CPI_MAF10_all <- as.factor(FB1$CPI_MAF10_all)

wt <- wilcox_test(Head.bobbing_A~CPI_MAF10_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Head.bobbing_A by CPI_MAF10_all (some, zero)
#Z = -0.37157, p-value = 0.7567
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.06818182  0.05478469
#sample estimates:
#  difference in location 
#-0.005050505 

mean(FB1$Head.bobbing_A[FB1$CPI_MAF10_all=="zero"], na.rm=TRUE)
#0.05045734
mean(FB1$Head.bobbing_A[FB1$CPI_MAF10_all=="some"], na.rm=TRUE)
#0.04478264


# MAF 20:

shapiro.test(FB1$Head.bobbing_A[FB1$CPI_MAF20_all=="zero"]) #p-value = 0.1237
shapiro.test(FB1$Head.bobbing_A[FB1$CPI_MAF20_all=="some"]) #p-value = 0.03966

var.test(FB1$Head.bobbing_A[FB1$CPI_MAF20_all=="zero"],FB1$Head.bobbing_A[FB1$CPI_MAF20_all=="some"])
#p-value = 0.5424

FB1$CPI_MAF20_all <- as.factor(FB1$CPI_MAF20_all)

wt <- wilcox_test(Head.bobbing_A~CPI_MAF20_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt
#data:  Head.bobbing_A by CPI_MAF20_all (some, zero)
#Z = -0.10533, p-value = 0.9594
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.05781283  0.06433596
#sample estimates:
#  difference in location 
#-0.0001443


mean(FB1$Head.bobbing_A[FB1$CPI_MAF20_all=="zero"], na.rm=TRUE)
#0.04507947
mean(FB1$Head.bobbing_A[FB1$CPI_MAF20_all=="some"], na.rm=TRUE)
#0.04945118



FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Head.bobbing_A)) + 
  labs(y= "Head bobbing", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Head.bobbing_A)) + 
  labs(y= "Head bobbing", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Head.bobbing_A)) + 
  labs(y= "Head bobbing", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)




#Push ups Autumn:


shapiro.test(FB1$Push.ups_A[FB1$CPI_407_all=="zero"]) #p-value = 0.06975
shapiro.test(FB1$Push.ups_A[FB1$CPI_407_all=="some"]) #p-value = 0.2493

var.test(FB1$Push.ups_A[FB1$CPI_407_all=="zero"],FB1$Push.ups_A[FB1$CPI_407_all=="some"])
#p-value = 0.008633

t.test(FB1$Push.ups_A ~ FB1$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
#data:  FB1$Push.ups_A by FB1$CPI_407_all
#t = 0.5802, df = 8.4825, p-value = 0.5769
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.4181939  0.7031152
#sample estimates:
#  mean in group some mean in group zero 
#0.6944217          0.5519611 



#MAF10:


shapiro.test(FB1$Push.ups_A[FB1$CPI_MAF10_all=="zero"]) #p-value = 0.006839
shapiro.test(FB1$Push.ups_A[FB1$CPI_MAF10_all=="some"]) #p-value = 0.1387

var.test(FB1$Push.ups_A[FB1$CPI_MAF10_all=="zero"],FB1$Push.ups_A[FB1$CPI_MAF10_all=="some"])
#p-value = 0.03568

wt <- wilcox_test(Push.ups_A~CPI_MAF10_all, data = FB1,
                  distribution = "exact", conf.int = TRUE)

wt 
#data:  Push.ups_A by CPI_MAF10_all (some, zero)
#Z = 1.9582, p-value = 0.0549
#alternative hypothesis: true mu is not equal to 0
#95 percent confidence interval:
#  -0.0351874  0.7203008
#sample estimates:
#  difference in location 
#0.2796252 


mean(FB1$Push.ups_A[FB1$CPI_MAF10_all=="zero"], na.rm=TRUE)
#0.4085773
mean(FB1$Push.ups_A[FB1$CPI_MAF10_all=="some"], na.rm=TRUE)
#0.7901134



#MAF20:


shapiro.test(FB1$Push.ups_A[FB1$CPI_MAF20_all=="zero"]) #p-value = 0.1714
shapiro.test(FB1$Push.ups_A[FB1$CPI_MAF20_all=="some"]) #p-value = 0.1328

var.test(FB1$Push.ups_A[FB1$CPI_MAF20_all=="zero"],FB1$Push.ups_A[FB1$CPI_MAF20_all=="some"])
#p-value = 0.06693

t.test(FB1$Push.ups_A ~ FB1$CPI_MAF20_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  FB1$Push.ups_A by FB1$CPI_MAF20_all
#t = 1.2294, df = 14, p-value = 0.2392
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.2160853  0.7964983
#sample estimates:
#  mean in group some mean in group zero 
#0.7682946          0.4780881 



FB1$CPI_407_all<-factor(FB1$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = FB1, aes(x=CPI_407_all, y=Push.ups_A)) + 
  labs(y= "Push ups", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

FB1$CPI_MAF10_all<-factor(FB1$CPI_MAF10_all, c("zero", "some")) 

b <- ggplot(data = FB1, aes(x=CPI_MAF10_all, y=Push.ups_A)) + 
  labs(y= "Push ups", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF10_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

FB1$CPI_MAF20_all<-factor(FB1$CPI_MAF20_all, c("zero", "some")) 

c <- ggplot(data = FB1, aes(x=CPI_MAF20_all, y=Push.ups_A)) + 
  labs(y= "Push ups", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_MAF20_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)

