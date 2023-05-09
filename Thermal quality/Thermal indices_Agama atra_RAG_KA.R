###########################################################
####### Classifying Te according to B80 and CTmax #########
###########################################################


#path <- "C:/Users/kalujevic/OneDrive - Stellenbosch University/karla backup/Data analyses/Chapter_2 analyses/OTMs/OTMs_March/"
#path <- "/Users/raquelgarcia/Documents/Karla/"
path <- "C:/Users/karla/Desktop/All_OTMs_March/"



### A. atra B80 range (average of individual lower and upper B80 from sprint speed performance curves)
B80_1 <- 27.4
B80_2 <- 41.6

### A. atra Tpref (from Jenna's study)
tpref <- 36.6

### A. atra CTmax (from Susana's paper) 
ctmax <- 44.9


#Lizard_ID <- c("BGR", "BGW", "BWG", "GBG", "GBR", "GBW", "GRB", "GYB", "RBR", "RBY", "RGB", "WBG", "WGB", "WYR", "YGW", "YWY")
#micro.types <- c("crevice", "full.sun", "partly.sun", "shade")

classes <- c("within.B80", "above.ctmax")


alldata <- get(load(paste0(path, "OTM_data_March")))
head(alldata)

data2 <- get(load(paste0(path, "categorised_data_")))
head(data2)
Lizard_ID <- unique(data2$Territory)
micro.types <- unique(data2$Microsite)


#for (s in 1:length(Lizard_ID))
#{
  #cat(sites[s], " ")
  
  #site.path <- paste(path, "data/", sites[s], "/",  sep="")
  
#  for (t in 1:length(micro.types))
#  {
#    mydata <- get(load(paste(path, Lizard_ID[s], "_", micro.types[t], "_", "OTM_data_March", sep=""))) # How does this work since OTM_data_March doesn't contain any info on Lizard ID or microsite? Did you have 1 file for each site in your case, and each treatment? Because for me, all OTMs are in one file and only then we categorised it based on the data in OTM_data.txt, and then saved it as categorised_data_.
    ### object to store the results
#    Teclasses <- array(NA, dim=c(nrow(mydata), ncol(mydata), length(classes)), dimnames=list(rownames(mydata), colnames(mydata), classes))
    
#    Teclasses[,,1] <- ifelse(mydata>=B80_1 & mydata<=B80_2, 1,0)
#    Teclasses[,,2] <- ifelse(mydata>=ctmax, 1,0)
    
#    save(Teclasses, file=paste(path, "output_2/",  Lizard_ID[s], "_", micro.types[t], "_classified_OTM_data_according_to_B80_CTmax", sep=""))
#  }
#}


# Raquel:

str(data2)

# classifying each value of Te irrespective of territory or microsite type

          data2[,classes[1]] <- ifelse(data2$Te >= B80_1 & data2$Te <= B80_2, 1, 0)
          data2[,classes[2]] <- ifelse(data2$Te > ctmax, 1, 0)

          head(data2)
          #save(data2, file=paste(path, "output_2/",  Lizard_ID[s], "_", micro.types[t], "_classified_OTM_data_according_to_B80_CTmax", sep=""))
          #this save line didn't work, error: Error in NextMethod("[") : object 's' not found
          save(data2, file=paste(path, "output_2/",  "_classified_OTM_data_according_to_B80_CTmax", sep="")) #modified to exclude micro.types and Lizard_ID
          
########################################################################
############ Daily stats of time within B80 & above CTmax ##############
########################################################################

library(reshape2)

time.indices <- c("perc.time.lizard.within.B80", "perc.time.lizard.above.CTmax")

time.indices.names <- c("% daily time within B80 (1 lizard)", "% daily time above CTmax (1 site)")


days <- unique(data2$Date)  # NOTE 39 days -- is this correct??

# running code for each territory and each day 
time.indices.results <- array(NA, dim=c(length(days), length(time.indices), length(Lizard_ID)), dimnames=list(days, time.indices, Lizard_ID))

for (t in 1:length(Lizard_ID))
{
  cat(Lizard_ID[t], " ")
  
  mydata <- subset(data2, Territory==Lizard_ID[t]) # selecting the territory
  
    for (d in 1:length(days))
    {
      d.data <- subset(mydata, Date==days[d]) # selecting the day
      
      #B80
      b80.data <- dcast(d.data, Time ~ OTM, value.var="within.B80") # changing the format of the dataframe so that it is easier to sum the number of microsites that are suitable and calculating percentage of time when that sum is at least 1
      time.indices.results[d,"perc.time.lizard.within.B80",t] <- length(which(apply(b80.data[,2:ncol(b80.data)],1,sum,na.rm=T)>0))*100/nrow(b80.data)   
      
      #CTmax
      ctmax.data <- dcast(d.data, Time ~ OTM, value.var="above.ctmax") # same as above
      time.indices.results[d,"perc.time.lizard.above.CTmax",t] <- length(which(apply(ctmax.data[,2:ncol(ctmax.data)],1,sum,na.rm=T)>0))*100/nrow(ctmax.data)   
    }
}

#write.csv(time.indices.results, file="Thermal_indices.csv")


save(time.indices.results, file=paste(path, "output_2/Thermal_indices_March", sep=""))


march <- get(load(paste0(path, "output_2/Thermal_indices_March")))

write.table(march, file="Thermal_indices_March.txt", row.names=T, sep="\t")


#####################################################################################

library(reshape2)

time.indices <- c("perc.time.lizard.within.B80", "perc.time.lizard.above.CTmax")

time.indices.names <- c("% daily time within B80 (1 lizard)", "% daily time above CTmax (1 site)")


days <- unique(data2$Date)  # NOTE 39 days -- is this correct??

# running code for each territory and each day
time.indices.results <- array(NA, dim=c(length(days), length(time.indices), length(Lizard_ID), length(micro.types)), dimnames=list(days, time.indices, Lizard_ID, micro.types))

for (t in 1:length(Lizard_ID))
{
  cat(Lizard_ID[t], " ")
  
  mydata <- subset(data2, Territory==Lizard_ID[t]) # selecting the territory
  
  for (d in 1:length(days))
  {
    d.data <- subset(mydata, Date==days[d]) # selecting the day
    
    for (m in 1:length(micro.types))
    {
      m.data <- subset(d.data, Microsite==micro.types[m])
      
      #B80
      b80.data <- dcast(m.data, Time ~ OTM, value.var="within.B80") # changing the format of the dataframe so that it is easier to sum the number of microsites that are suitable and calculating percentage of time when that sum is at least 1
      time.indices.results[d,"perc.time.lizard.within.B80",t,m] <- length(which(apply(b80.data[,2:ncol(b80.data)],1,sum,na.rm=T)>0))*100/nrow(b80.data)  
      
      #CTmax
      ctmax.data <- dcast(m.data, Time ~ OTM, value.var="above.ctmax") # same as above
      time.indices.results[d,"perc.time.lizard.above.CTmax",t,m] <- length(which(apply(ctmax.data[,2:ncol(ctmax.data)],1,sum,na.rm=T)>0))*100/nrow(ctmax.data)  
    }
  }
}

path <- "C:/Users/karla/Desktop/All_OTMs_March/"
name <- get(load(paste0(path, "Thermal_indices_per_microsite_December_RAG")))

write.table(name, file="Thermal_indices_per_microsite_December.txt", row.names=T, sep="\t")

###############

path <- "C:/Users/karla/Desktop/All_OTMs_December/"
name <- get(load(paste0(path, "Thermal_indices_December_RAG")))

write.table(name, file="Thermal_indices_December.txt", row.names=T, sep="\t")

#########
name <- get(load(paste0(path, "categorised_data_")))
write.table(name, file="Categorised data March.txt", row.names=T, sep="\t")



############################################################################

#CHECKING THE Te MEAN SD AND VARIANCE FOR EACH LIZARD TERRITORY IN EACH SEASON (ALL OTMs POOLED PER TERRITORY)


## MARCH:

path <- "C:/Users/karla/Desktop/All_OTMs_March/"

Te_March <- get(load(paste0(path, "categorised_data_")))
write.table(name, file="Categorised data March.txt", row.names=T, sep="\t")

#Te_March <- read.csv(file="Categorised data March.csv", header=T, sep=",")

#Lizard_ID <- c("BGR", "BGW", "BWG", "GBG", "GBR", "GBW", "GRB", "GYB", "RBR", "RBY", "RGB", "WBG", "WGB", "WYR", "YGW", "YWY")

attach(Categorised_data_March)


Te_March_BGR <- subset(Categorised_data_March, Territory=="BGR")


mean(Te_March_BGR$Te, na.rm=T)
#25.20885

sd(Te_March_BGR$Te, na.rm=T)
#8.704511

min(Te_March_BGR$Te, na.rm=T)
#6.07

max(Te_March_BGR$Te, na.rm=T)
#57.966

var(Te_March_BGR$Te, na.rm=T)
#75.76851

##

Te_March_BGW <- subset(Categorised_data_March, Territory=="BGW")


mean(Te_March_BGW$Te, na.rm=T)
#25.33712

sd(Te_March_BGW$Te, na.rm=T)
#8.752544

min(Te_March_BGW$Te, na.rm=T)
#6.5

max(Te_March_BGW$Te, na.rm=T)
#61.499

var(Te_March_BGW$Te, na.rm=T)
#76.60703

##

Te_March_BWG <- subset(Categorised_data_March, Territory=="BWG")


mean(Te_March_BWG$Te, na.rm=T)
#25.20885

sd(Te_March_BWG$Te, na.rm=T)
#8.704511

min(Te_March_BWG$Te, na.rm=T)
#6.07

max(Te_March_BWG$Te, na.rm=T)
#57.966

var(Te_March_BWG$Te, na.rm=T)
#75.76851

##

Te_March_GBG <- subset(Categorised_data_March, Territory=="GBG")


mean(Te_March_GBG$Te, na.rm=T)
#24.71001

sd(Te_March_GBG$Te, na.rm=T)
#8.236068

min(Te_March_GBG$Te, na.rm=T)
#7.024

max(Te_March_GBG$Te, na.rm=T)
#56.036

var(Te_March_GBG$Te, na.rm=T)
#67.83282

##

Te_March_GBR <- subset(Categorised_data_March, Territory=="GBR")


mean(Te_March_GBR$Te, na.rm=T)
#23.80479

sd(Te_March_GBR$Te, na.rm=T)
#8.45305

min(Te_March_GBR$Te, na.rm=T)
#5.565

max(Te_March_GBR$Te, na.rm=T)
#55.5

var(Te_March_GBR$Te, na.rm=T)
#71.45406

##

Te_March_GBW <- subset(Categorised_data_March, Territory=="GBW")


mean(Te_March_GBW$Te, na.rm=T)
#24.85918

sd(Te_March_GBW$Te, na.rm=T)
#9.116366

min(Te_March_GBW$Te, na.rm=T)
#6.048

max(Te_March_GBW$Te, na.rm=T)
#58.501

var(Te_March_GBW$Te, na.rm=T)
#83.10812

##

Te_March_GRB <- subset(Categorised_data_March, Territory=="GRB")


mean(Te_March_GRB$Te, na.rm=T)
#24.73077

sd(Te_March_GRB$Te, na.rm=T)
#8.558381

min(Te_March_GRB$Te, na.rm=T)
#6

max(Te_March_GRB$Te, na.rm=T)
#61.5

var(Te_March_GRB$Te, na.rm=T)
#73.24589

##

Te_March_GYB <- subset(Categorised_data_March, Territory=="GYB")


mean(Te_March_GYB$Te, na.rm=T)
#26.17462

sd(Te_March_GYB$Te, na.rm=T)
#9.34278

min(Te_March_GYB$Te, na.rm=T)
#6

max(Te_March_GYB$Te, na.rm=T)
#59.015

var(Te_March_GYB$Te, na.rm=T)
#87.28754

##

Te_March_RBR <- subset(Categorised_data_March, Territory=="RBR")


mean(Te_March_RBR$Te, na.rm=T)
#25.15427

sd(Te_March_RBR$Te, na.rm=T)
#8.808786

min(Te_March_RBR$Te, na.rm=T)
#5.5

max(Te_March_RBR$Te, na.rm=T)
#55

var(Te_March_RBR$Te, na.rm=T)
#77.59471

##

Te_March_RBY <- subset(Categorised_data_March, Territory=="RBY")


mean(Te_March_RBY$Te, na.rm=T)
#25.64

sd(Te_March_RBY$Te, na.rm=T)
#9.156486

min(Te_March_RBY$Te, na.rm=T)
#5.572

max(Te_March_RBY$Te, na.rm=T)
#54.024

var(Te_March_RBY$Te, na.rm=T)
#83.84124

##

Te_March_RGB <- subset(Categorised_data_March, Territory=="RGB")


mean(Te_March_RGB$Te, na.rm=T)
#25.18772

sd(Te_March_RGB$Te, na.rm=T)
#8.498155

min(Te_March_RGB$Te, na.rm=T)
#6.5

max(Te_March_RGB$Te, na.rm=T)
#55.5

var(Te_March_RGB$Te, na.rm=T)
#72.21864

##

Te_March_WBG <- subset(Categorised_data_March, Territory=="WBG")


mean(Te_March_WBG$Te, na.rm=T)
#26.13682

sd(Te_March_WBG$Te, na.rm=T)
#8.62787

min(Te_March_WBG$Te, na.rm=T)
#6.556

max(Te_March_WBG$Te, na.rm=T)
#57.03

var(Te_March_WBG$Te, na.rm=T)
#74.44014

##

Te_March_WGB <- subset(Categorised_data_March, Territory=="WGB")


mean(Te_March_WGB$Te, na.rm=T)
#25.68124

sd(Te_March_WGB$Te, na.rm=T)
#8.889274

min(Te_March_WGB$Te, na.rm=T)
#5.109

max(Te_March_WGB$Te, na.rm=T)
#59.996

var(Te_March_WGB$Te, na.rm=T)
#79.0192

##

Te_March_WYR <- subset(Categorised_data_March, Territory=="WYR")


mean(Te_March_WYR$Te, na.rm=T)
#25.82661

sd(Te_March_WYR$Te, na.rm=T)
#9.008579

min(Te_March_WYR$Te, na.rm=T)
#7.5

max(Te_March_WYR$Te, na.rm=T)
#69

var(Te_March_WYR$Te, na.rm=T)
#81.15449

##

Te_March_YGW <- subset(Categorised_data_March, Territory=="YGW")


mean(Te_March_YGW$Te, na.rm=T)
#25.73433

sd(Te_March_YGW$Te, na.rm=T)
#8.826625

min(Te_March_YGW$Te, na.rm=T)
#6.566

max(Te_March_YGW$Te, na.rm=T)
#58.522

var(Te_March_YGW$Te, na.rm=T)
#77.90931

##

Te_March_YWY <- subset(Categorised_data_March, Territory=="YWY")


mean(Te_March_YWY$Te, na.rm=T)
#24.33742

sd(Te_March_YWY$Te, na.rm=T)
#8.584407

min(Te_March_YWY$Te, na.rm=T)
#6.581

max(Te_March_YWY$Te, na.rm=T)
#61.441

var(Te_March_YWY$Te, na.rm=T)
#73.69205

############################################

## DECEMBER:


path <- "C:/Users/karla/Desktop/All_OTMs_December/"

Te_December <- get(load(paste0(path, "categorised_data_")))
write.table(Te_December, file="Categorised data December.txt", row.names=T, sep="\t")

#Lizard_ID <- c("BGR", "BRY", "BWB", "BWG", "BYG", "GBG", "GBR", "GBW", "GYB", "RBR", "RGB", "RGW", "WBY", "WGB", "WYB", "YBR", "YWY")

attach(Categorised_data_December)


Te_December_BGR <- subset(Categorised_data_December, Territory=="BGR")


mean(Te_December_BGR$Te, na.rm=T)
#30.94092

sd(Te_December_BGR$Te, na.rm=T)
#10.97568

min(Te_December_BGR$Te, na.rm=T)
#11.602

max(Te_December_BGR$Te, na.rm=T)
#71.959

var(Te_December_BGR$Te, na.rm=T)
#120.4656

##

Te_December_BWB <- subset(Categorised_data_December, Territory=="BWB")


mean(Te_December_BWB$Te, na.rm=T)
#31.57685

sd(Te_December_BWB$Te, na.rm=T)
#11.38678

min(Te_December_BWB$Te, na.rm=T)
#9.5

max(Te_December_BWB$Te, na.rm=T)
#78.389

var(Te_December_BWB$Te, na.rm=T)
#129.6588

##

Te_December_BWG <- subset(Categorised_data_December, Territory=="BWG")


mean(Te_December_BWG$Te, na.rm=T)
#33.13107

sd(Te_December_BWG$Te, na.rm=T)
#12.01878

min(Te_December_BWG$Te, na.rm=T)
#11.569

max(Te_December_BWG$Te, na.rm=T)
#81.81

var(Te_December_BWG$Te, na.rm=T)
#144.4511

##

Te_December_BYG <- subset(Categorised_data_December, Territory=="BYG")


mean(Te_December_BYG$Te, na.rm=T)
#30.81046

sd(Te_December_BYG$Te, na.rm=T)
#10.87248

min(Te_December_BYG$Te, na.rm=T)
#10.141

max(Te_December_BYG$Te, na.rm=T)
#70.434

var(Te_December_BYG$Te, na.rm=T)
#118.2108

##

Te_December_GBG <- subset(Categorised_data_December, Territory=="GBG")


mean(Te_December_GBG$Te, na.rm=T)
#31.22908

sd(Te_December_GBG$Te, na.rm=T)
#11.76062

min(Te_December_GBG$Te, na.rm=T)
#10

max(Te_December_GBG$Te, na.rm=T)
#75.5

var(Te_December_GBG$Te, na.rm=T)
#138.3121

##

Te_December_GBR <- subset(Categorised_data_December, Territory=="GBR")


mean(Te_December_GBR$Te, na.rm=T)
#33.30953

sd(Te_December_GBR$Te, na.rm=T)
#11.90226

min(Te_December_GBR$Te, na.rm=T)
#10

max(Te_December_GBR$Te, na.rm=T)
#75.378

var(Te_December_GBR$Te, na.rm=T)
#141.6638

##

Te_December_GBW <- subset(Categorised_data_December, Territory=="GBW")


mean(Te_December_GBW$Te, na.rm=T)
#32.03104

sd(Te_December_GBW$Te, na.rm=T)
#12.26835

min(Te_December_GBW$Te, na.rm=T)
#11.5

max(Te_December_GBW$Te, na.rm=T)
#76.34

var(Te_December_GBW$Te, na.rm=T)
#150.5125


##

Te_December_GYB <- subset(Categorised_data_December, Territory=="GYB")


mean(Te_December_GYB$Te, na.rm=T)
#31.82178

sd(Te_December_GYB$Te, na.rm=T)
#11.37155

min(Te_December_GYB$Te, na.rm=T)
#11

max(Te_December_GYB$Te, na.rm=T)
#75

var(Te_December_GYB$Te, na.rm=T)
#129.3121


##

Te_December_RBR <- subset(Categorised_data_December, Territory=="RBR")


mean(Te_December_RBR$Te, na.rm=T)
#32.03445

sd(Te_December_RBR$Te, na.rm=T)
#11.88824

min(Te_December_RBR$Te, na.rm=T)
#11.114

max(Te_December_RBR$Te, na.rm=T)
#76.34

var(Te_December_RBR$Te, na.rm=T)
#141.3304


##

Te_December_RGB <- subset(Categorised_data_December, Territory=="RGB")


mean(Te_December_RGB$Te, na.rm=T)
#33.63365

sd(Te_December_RGB$Te, na.rm=T)
#11.0562

min(Te_December_RGB$Te, na.rm=T)
#10.136

max(Te_December_RGB$Te, na.rm=T)
#73

var(Te_December_RGB$Te, na.rm=T)
#122.2396


##

Te_December_RGW <- subset(Categorised_data_December, Territory=="RGW")


mean(Te_December_RGW$Te, na.rm=T)
#31.63873

sd(Te_December_RGW$Te, na.rm=T)
#10.90312

min(Te_December_RGW$Te, na.rm=T)
#10.5

max(Te_December_RGW$Te, na.rm=T)
#75.342

var(Te_December_RGW$Te, na.rm=T)
#118.878


##

Te_December_WBY <- subset(Categorised_data_December, Territory=="WBY")


mean(Te_December_WBY$Te, na.rm=T)
#31.28421

sd(Te_December_WBY$Te, na.rm=T)
#11.15973

min(Te_December_WBY$Te, na.rm=T)
#10.5

max(Te_December_WBY$Te, na.rm=T)
#77.5

var(Te_December_WBY$Te, na.rm=T)
#124.5396


##

Te_December_WGB <- subset(Categorised_data_December, Territory=="WGB")


mean(Te_December_WGB$Te, na.rm=T)
#31.78082

sd(Te_December_WGB$Te, na.rm=T)
#11.34117

min(Te_December_WGB$Te, na.rm=T)
#10.609

max(Te_December_WGB$Te, na.rm=T)
#72.443

var(Te_December_WGB$Te, na.rm=T)
#128.6222


##

Te_December_WYB <- subset(Categorised_data_December, Territory=="WYB")


mean(Te_December_WYB$Te, na.rm=T)
#31.84574

sd(Te_December_WYB$Te, na.rm=T)
#12.56887

min(Te_December_WYB$Te, na.rm=T)
#9.541

max(Te_December_WYB$Te, na.rm=T)
#77.438

var(Te_December_WYB$Te, na.rm=T)
#157.9765

##

Te_December_YWY <- subset(Categorised_data_December, Territory=="YWY")


mean(Te_December_YWY$Te, na.rm=T)
#30.91831

sd(Te_December_YWY$Te, na.rm=T)
#11.56795

min(Te_December_YWY$Te, na.rm=T)
#8.5

max(Te_December_YWY$Te, na.rm=T)
#74.97

var(Te_December_YWY$Te, na.rm=T)
#133.8174

######################################

## testing if there is a relationship between B80 and variance for each season:

summer <- subset(Te_B80, Season=="summer")

mean(summer$Variance)
#133.3327
sd(summer$Variance)
#12.07039

summer_B80_Te_variance <- lm(Variance ~ B80_range, data = summer )
summary(summer_B80_Te_variance)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 114.8929    13.7506   8.355 3.19e-05 ***
#  B80_range     1.3690     0.9116   1.502    0.172    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 9.23 on 8 degrees of freedom
#(5 observations deleted due to missingness)
#Multiple R-squared:  0.2199,	Adjusted R-squared:  0.1224 
#F-statistic: 2.255 on 1 and 8 DF,  p-value: 0.1716


autumn <- subset(Te_B80, Season=="autumn")

mean(autumn$Variance)
#76.93375
sd(autumn$Variance)
#5.065574

autumn_B80_Te_variance <- lm(Variance ~ B80_range, data = autumn )
summary(autumn_B80_Te_variance)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  72.3643     6.9856  10.359 6.02e-08 ***
#  B80_range     0.3063     0.4602   0.666    0.516    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.162 on 14 degrees of freedom
#Multiple R-squared:  0.03067,	Adjusted R-squared:  -0.03857 
#F-statistic: 0.443 on 1 and 14 DF,  p-value: 0.5165