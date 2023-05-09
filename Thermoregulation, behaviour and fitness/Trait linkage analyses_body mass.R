library(nlme)
library(lme4)
library(ggplot2)

############################ T-TEST ############################


# (categories 'zero' or 'some' offspring; all that had offspring detected were pooled into category 'some' because when I split into 1, 2 or more offspring groups have too few datapoints per group)


MB <- read.csv(file="Body mass_subset_offspring number_t-test_NEW.csv", header=TRUE, sep=",")

View(MB)


shapiro.test(MB$Mb[MB$CPI_407_all=="zero"]) #p-value = 0.2067
shapiro.test(MB$Mb[MB$CPI_407_all=="some"]) #p-value = 0.8657

var.test(MB$Mb[MB$CPI_407_all=="zero"],MB$Mb[MB$CPI_407_all=="some"])
#p-value = 0.1194


t.test(MB$Mb ~ MB$CPI_407_all, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#data:  MB$Mb by MB$CPI_407_all
#t = 2.4143, df = 15, p-value = 0.029
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.3225235 5.1828154
#sample estimates:
#  mean in group some mean in group zero 
#24.47443           21.72176 

MB %>% cohens_d(Mb ~ CPI_407_all, var.equal = T)
#.y.   group1 group2 effsize    n1    n2 magnitude
#* <chr> <chr>  <chr>    <dbl> <int> <int> <ord>    
#  1 Mb    some   zero      1.17     8     9 large  

## MAF 10:

shapiro.test(MB$Mb[MB$MAF_10=="zero"]) #p-value = 0.6138
shapiro.test(MB$Mb[MB$MAF_10=="some"]) #p-value = 0.5405

var.test(MB$Mb[MB$MAF_10=="zero"],MB$Mb[MB$MAF_10=="some"])
#p-value = 0.5791


t.test(MB$Mb ~ MB$MAF_10, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)

#	Two Sample t-test

#data:  MB$Mb by MB$MAF_10
#t = 1.2676, df = 15, p-value = 0.2243
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.103095  4.340114
#sample estimates:
#  mean in group some mean in group zero 
#23.87399           22.25548 



## MAF 20:

shapiro.test(MB$Mb[MB$MAF_20=="zero"]) #p-value = 0.6386
shapiro.test(MB$Mb[MB$MAF_20=="some"]) #p-value = 0.428

var.test(MB$Mb[MB$MAF_20=="zero"],MB$Mb[MB$MAF_20=="some"])
#p-value = 0.9871


t.test(MB$Mb ~ MB$MAF_20, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)

#	Two Sample t-test

#data:  MB$Mb by MB$MAF_20
#t = 2.2063, df = 15, p-value = 0.04337
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.08739014 5.06386263
#sample estimates:
#  mean in group some mean in group zero 
#24.22919           21.65356 



MB$CPI_407_all<-factor(MB$CPI_407_all, c("zero", "some")) 

a <- ggplot(data = MB, aes(x=CPI_407_all, y=Mb)) + 
  labs(y= "Body mass", x = "Reproductive success") +
  geom_boxplot(aes(fill=CPI_407_all)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


a

MB$MAF_10<-factor(MB$MAF_10, c("zero", "some")) 

b <- ggplot(data = MB, aes(x=MAF_10, y=Mb)) + 
  labs(y= "Body mass", x = "Reproductive success") +
  geom_boxplot(aes(fill=MAF_10)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


b

MB$MAF_20<-factor(MB$MAF_20, c("zero", "some")) 

c <- ggplot(data = MB, aes(x=MAF_20, y=Mb)) + 
  labs(y= "Body mass", x = "Reproductive success") +
  geom_boxplot(aes(fill=MAF_20)) + geom_jitter(color="black", size=2, alpha=0.9) + theme(legend.position="none")


c


ggarrange(a, b, c + rremove("x.text"), 
          labels = c("407", "MAF10", "MAF20"),
          ncol = 3, nrow = 1)

