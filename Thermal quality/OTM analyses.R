############################################################
############  MERGING AND ORGANISING OTM DATA ##############
############################################################

library(lubridate)
library(reshape)

##################################### March 2018 #####################################

path <- "C:/Users/karla/Desktop/All_OTMs_March/"

path <- "E:/karla backup/Data analyses/Chapter_2 analyses/OTMs/OTMs_March/All_OTMs_March/"

Lizard_ID <- c("BGR", "BGW", "BWG", "GBG", "GBR", "GBW", "GRB", "GYB", "RBR", "RBY", "RGB", "WBG", "WGB", "WYR", "YGW", "YWY")
temp.type <- c("rock", "soil")
dates <- seq(from=ymd_hm("2018/3/16 00:00", tz=""), to=ymd_hm("2018/4/24 07:30", tz=""), by="15 mins")
hours <- sprintf("%02d", 0:23)


############# Collating data ###############

otms <- 1:288

#not retrieved: 38, 68, 80, 98, 117, 163, 275, 279, 281
#faulty: 99, 280
#redeployed (April 9 @ 11:30): 62, 122, 155, 156

out <- c(38, 62, 68, 80, 98, 99, 117, 122, 155, 156, 163, 275, 279, 280, 281)


otms <- otms[-out]
otms



mydata <- array(NA, dim=c(length(dates), length(otms)), dimnames=list(as.character(dates),otms))

for (o in 1:length(otms))
{
  #o=62
  cat(o, "/")
  otm.data <- read.csv(paste(path, otms[o], ".csv", sep=""), header=F, sep=",")
  
  colnames(otm.data) <- c("date", "temperature", "unit")
  otm.data[,"date"] <- as.character(otm.data[,"date"])
  
  #fixing formatting issue You might not need this if this doesn't happen with your data 
  if(substr(otm.data[1,1],1,3)=="﻿"){otm.data[1,1] <- gsub("﻿", "", otm.data[1,1])}
  
  # vector of dates from the otm.data file in the same format as the dates vector
  otm.dates <- as.POSIXct(gsub("/", "-", otm.data[,"date"]), tz="")
  
  # identifying temporal resolution of otm.data
  res <- otm.dates[2] - otm.dates[1]
  
  # if resolution of otm.data is 15minutes I can use the dates vector as originally defined at that resolution; if resolution of otm.data is 30min I need to subset the dates vector to include only every other timestamp, i.e. 00 and 30 minutes after the hour. We also need to subset the vector of rows that we need to fill in in the storage object (which is at 15min resolution), i.e. when resolution is 15min we only have to fill in every other row (and every other row will be NA.
  # You can run both lines below but only one of them will be true (epending on whether res==15 or res==30 is TRUE) and thus only what's inside the correspinding curly brackets will run.
  # Below we will use the seq.dates and seq.rows created here
  if(res==15) {seq.dates <- dates; seq.rows <- 1:length(dates); thres <- "15 mins"}
  if(res==30) {seq.dates <- dates[substr(dates, 15,16) %in% c("00", "30")]; seq.rows <- seq(1,length(dates), by=2); thres <- "30 mins"}
  
  # For each element of my vector of dates or rows as defined above, find the closest date in the otm.data file and fill the corresponding row in the storage object with the temperaure value from the otm.data file for that date
  
  for (i in 1:length(seq.rows)) 
  {
    # identify date in the position i of the dates vector (the dates vector will be the one at 15 or 30min resolution as defined above)
    my.date <- seq.dates[i]
    
    # identify the position in the otm.data file that has the closest date to my.date (minimum of the differences between my.date and each of the dates in the otm.data file)
    position <- which(abs(my.date - otm.dates) == min(abs(my.date - otm.dates)))
    
    # fil l the row seq.rows[i] (my.date) and column o (the otm for this otm.data file) of the storage object with the temperature value taken from the selected position of the otm.data file 
    mydata[seq.rows[i],as.character(otms[o])] <- otm.data[position,"temperature"]
  }
}
#write.table(mydata, file=paste(path, "OTM_data_March", sep=""), row.names = T, sep="\t")
save(mydata, file=paste(path, "OTM_data_March", sep=""))




#mydata <- get(load(paste(path, "OTM_data", sep="")))
#o=1
#which(mydata[,o]<0)
#which(mydata[,o]>100)

#removing outliers


################################################################
########## REARRANGING AND CATEGORISING THE DATASET ############
################################################################

# Territories
BGR.otm <- as.character(c(1:18))

BGW.otm <- as.character(c(19:36))

BWG.otm <- as.character(c(37, 39:54))

GBG.otm <- as.character(c(55:61, 63:67, 69:72))

GBR.otm <- as.character(c(73:79, 81:90))

GBW.otm <- as.character(c(91:97, 100:108))

GRB.otm <- as.character(c(109:116, 118:121, 123:126))

GYB.otm <- as.character(c(127:144))

RBR.otm <- as.character(c(145:154, 157:162))

RBY.otm <- as.character(c(164:180))

RGB.otm <- as.character(c(181:198))

WBG.otm <- as.character(c(199:216))

WGB.otm <- as.character(c(217:234))

WYR.otm <- as.character(c(235:252))

YGW.otm <- as.character(c(253:270))

YWY.otm <- as.character(c(271:278, 282:288))



### Loading file with OTM characteristics
OTMs <- read.table(paste0(path, "OTM_list_March.txt"), header=T, sep="\t")


### Loading the merged OTM data
mydata <- get(load(paste(path, "OTM_data_March", sep="")))


### Remove nighttime
daytime <- sprintf("%02d", 8:18)

mydata <- mydata[which(substr(rownames(mydata), 12,13) %in% daytime==TRUE),]
mydata <- as.data.frame(mydata)              


### Days
days <- unique(substr(rownames(mydata), 6, 10))


### Adding columns for Date and Time
Date <- substr(rownames(mydata), 6, 10)
Time <- substr(rownames(mydata), 12, 19)
mydata <- cbind(Date, Time, mydata)


### Converting to long format 
dataf <- melt(as.data.frame(mydata), id=c("Date", "Time"))
head(dataf)
colnames(dataf) <- c("Date", "Time", "OTM", "Te")
head(dataf)


### Categorising by territory
Lizard_ID <- unique(OTMs$Lizard_ID)
for (tt in 1:length(Lizard_ID))
{
  numbers <- OTMs[OTMs$Lizard_ID==Lizard_ID[tt], "OTM"]
  rows <- dataf$OTM %in% as.character(numbers)
  dataf[rows, "Territory"] <- Lizard_ID[tt]
}


### Categorising by microsite type
microsites_ <- unique(OTMs$Sun)
microsites <- gsub(" ", ".", microsites_)

for (m in 1:length(microsites))
{
  numbers <- OTMs[OTMs$Sun==microsites_[m], "OTM"]
  rows <- dataf$OTM %in% as.character(numbers)
  dataf[rows, "Microsite"] <- microsites[m]
}


### Categorising by substrate
temp.types <- unique(OTMs$Temp)

for (s in 1:length(temp.types))
{
  numbers <- OTMs[OTMs$Temp==temp.types[s], "OTM"]
  rows <- dataf$OTM %in% as.character(numbers)
  dataf[rows, "Substrate"] <- temp.types[s]
}

head(dataf)
#write.table(dataf, file=paste0(path, "categorised_data_"), row.names=T, sep="\t")
save(dataf, file=paste0(path, "categorised_data_"))



################################################################
########## CALCULATING NUMBER OF HOURS WITHIN Tpref ############
################################################################

### Loading list of tprefs for each territory

tpref.list <- read.table(paste0(path, "B80.csv"), header=T, sep=",")

names(tpref.list)[1] <- "Lizard_ID"


### Classifiying each Te value as within or outside Tpref range for a given territory
for (tt in 1:length(Lizard_ID))
{
  #identifying the tpref range for selected territory
  tpref1 <- tpref.list[which(tpref.list$Lizard_ID==as.character(Lizard_ID[tt])),"B80_lower"]
  tpref2 <- tpref.list[which(tpref.list$Lizard_ID==as.character(Lizard_ID[tt])),"B80_upper"]
  
  #rows within Tpref
  rows <- which(dataf$Territory==Lizard_ID[tt] & dataf$Te >= tpref1 & dataf$Te <= tpref2)
  dataf[rows, "Tpref.class"] <- 1
  
  #rows outside Tpref
  rows <- which(dataf$Territory==Lizard_ID[tt] & dataf$Te < tpref1 | dataf$Te > tpref2)
  dataf[rows, "Tpref.class"] <- 0

  rows <- which(dataf$Territory==Lizard_ID[tt] & dataf$Te < tpref1)
  dataf[rows, "EnvT_toolow"] <- 1
  
  rows <- which(dataf$Territory==Lizard_ID[tt] & dataf$Te >= tpref1 )
  dataf[rows, "EnvT_toolow"] <- 0
  
  rows <- which(dataf$Territory==Lizard_ID[tt] & dataf$Te > tpref2)
  dataf[rows, "EnvT_toohigh"] <- 1 
  
  rows <- which(dataf$Territory==Lizard_ID[tt] & dataf$Te <= tpref2)
  dataf[rows, "EnvT_toohigh"] <- 0
}

head(dataf)
 
write.csv(dataf, "tpref_match_te.csv")
#dataf<-read.csv("tpref_match_te.csv")

# percent of values either too high or too low 

library(tidyverse)

# March:

dataf_m<-read.csv("tpref_match_te_march_1.csv")

df<-dataf_m %>% filter(!(is.na(Te))) %>%
  mutate(EnvT_toolow = ifelse(is.na(EnvT_toolow), 0, EnvT_toolow),
         EnvT_toohigh = ifelse(is.na(EnvT_toohigh), 0, EnvT_toohigh)) %>%
  group_by(Territory) %>%
  summarize(perc_toolow = sum(EnvT_toolow)/length(EnvT_toolow)*100,
             perc_toohigh = sum(EnvT_toohigh)/length(EnvT_toohigh)*100)

write.csv(df, "tpref_match_te_march_indices.csv")

# December:

dataf_d<-read.csv("tpref_match_te_december_1.csv")

df<-dataf_d %>% filter(!(is.na(Te))) %>%
  mutate(EnvT_toolow = ifelse(is.na(EnvT_toolow), 0, EnvT_toolow),
         EnvT_toohigh = ifelse(is.na(EnvT_toohigh), 0, EnvT_toohigh)) %>%
  group_by(Territory) %>%
  summarize(perc_toolow = sum(EnvT_toolow)/length(EnvT_toolow)*100,
            perc_toohigh = sum(EnvT_toohigh)/length(EnvT_toohigh)*100)

write.csv(df, "tpref_match_te_dec_indices.csv")


# % time below B80 summer:

test_OTMs_freq %>% filter(Season=="summer") %>%
ggplot(aes(x=Hours_average,y=perc_toolow)) + 
         geom_point()

# % time above B80 summer:

test_OTMs_freq %>% filter(Season=="summer") %>%
  ggplot(aes(x=Hours_average,y=perc_toohigh)) + 
  geom_point()

# % time below B80 autumn:

test_OTMs_freq %>% filter(Season=="autumn") %>%
  ggplot(aes(x=Hours_average,y=perc_toolow)) + 
  geom_point()

# % time above B80 autumn:

test_OTMs_freq %>% filter(Season=="autumn") %>%
  ggplot(aes(x=Hours_average,y=perc_toohigh)) + 
  geom_point()

# across both seasons:

test_OTMs_freq %>% 
  ggplot(aes(x=Hours_average,y=perc_toolow)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x="Thermal quality", y="Time below individual prefered thermal range (%)") +
  theme_bw()

ggsave("belowB80.pdf", height = 4, width = 5)

test_OTMs_freq %>% 
  ggplot(aes(x=Hours_average,y=perc_toohigh)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x="Thermal quality", y="Time above individual prefered thermal range (%)") +
  theme_bw()

ggsave("aboveB80.pdf", height = 4, width = 5)


test_OTMs_freq %>% 
  ggplot(aes(x=perc_toolow, y=perc_toohigh)) + 
  geom_point(aes(size=Hours_average)) +
  geom_smooth(method = "lm") +
  labs(x="Time below individual B80 (%)", y="Time above individual B80 (%)", fill="xx") +
  theme_bw()
  
ggsave("combined cold and hig with Tq.pdf", height = 4, width = 6.5)

# models:

low <- lme(perc_toolow ~ Hours_average*Season, random = ~1|Lizard_ID, data = test_OTMs_freq, na.action = na.omit, method="ML")
summary(low)


low1 <- lme(perc_toolow ~ Hours_average+Season, random = ~1|Lizard_ID, data = test_OTMs_freq, na.action = na.omit, method="ML")
summary(low1)
#Fixed effects:  perc_toolow ~ Hours_average + Season 
#                   Value Std.Error DF   t-value p-value
#(Intercept)    81.46068 1.3471131 15  60.47055       0
#Hours_average  -9.41673 0.4598917  8 -20.47598       0
#Seasonsummer  -16.68784 1.3893109  8 -12.01160       0


high <- lme(perc_toohigh ~ Hours_average*Season, random = ~1|Lizard_ID, data = test_OTMs_freq, na.action = na.omit, method="ML")
summary(high)

high1 <- lme(perc_toohigh ~ Hours_average+Season, random = ~1|Lizard_ID, data = test_OTMs_freq, na.action = na.omit, method="ML")
anova(high,high1)

summary(high1)
#Fixed effects:  perc_toohigh ~ Hours_average + Season 
#Value Std.Error DF   t-value p-value
#(Intercept)    2.117832 1.1137173 15  1.901588  0.0766
#Hours_average  1.729650 0.3915757  8  4.417152  0.0022
#Seasonsummer  10.722533 0.8117811  8 13.208650  0.0000

report(high1)

### Calculating the mean number of hours (across OTMs for a given microsite) for each day and territory
for (tt in 1:length(Lizard_ID))
{
  cat(as.character(Lizard_ID[tt]), "/")
  
  # Object to store the results for each territory
  htpref <- array(NA, dim=c(length(days), length(microsites)), dimnames = list(days, microsites))
  
  # Loop for each microsite
  for (m in 1:length(microsites))
  {
    # selecting the territory and microsite
    subdata <- subset(dataf, Territory==Lizard_ID[tt] & Microsite==microsites[m])
    
    # Loop for each day
    for (d in 1:length(days))
    {
      data.day <- subset(subdata, Date==days[d])
      
      # identifying the OTM numbers
      my.otms <- unique(data.day$OTM)
      
      # temporary object to store the results for each otm 
      temporary.object <- rep(NA, length(my.otms))
      
      # Loop for each otm
      for (o in 1:length(my.otms))
      {
        # selecting data for a given otm
        data.otm <- subset(data.day, OTM==my.otms[o])
        
        # summing the ones for that otm
        sum.1s <- length(which(data.otm$Tpref.class==1))
        
        # calculating the number of hours corresponding to that sum -- this will depend on the temporal resolution of the data for this given otm (15 min means dividing the number of ones by 4, otherwise by 2)
        res <- as.numeric(substr(data.otm[2,"Time"],4,5)) - as.numeric(substr(data.otm[1,"Time"],4,5))
        if(res==15){temporary.object[o] <- sum.1s/4}
        if(res==30){temporary.object[o] <- sum.1s/2}
      }
      # filling in the mean of the number of hours for all otms (for a given day and microsite) in my storage object for a given territory (raw is the day and column is the microsite)
      htpref[d,m] <- mean(temporary.object, na.rm=T)
    }
  }
  
  # once all days and microsites done, save the territory object
  write.table(htpref, file=paste0(path, "Mean_number_hours_within_Tpref_across_OTMs_for_each_day_for_territory_", Lizard_ID[tt]), row.names = T, sep="\t")
}

str(htpref)
head(htpref)
boxplot(htpref)


################################### December 2017 ###################################


path <- "C:/Users/karla/Desktop/All_OTMs_December/"

Lizard_ID <- c("BGR", "BRY", "BWB", "BWG", "BYG", "GBG", "GBR", "GBW", "GYB", "RBR", "RGB", "RGW", "WBY", "WGB", "WYB", "YBR", "YWY")
temp.type <- c("rock", "soil")
dates <- seq(from=ymd_hm("2017/12/3 00:00", tz=""), to=ymd_hm("2018/1/9 14:00", tz=""), by="15 mins")
hours <- sprintf("%02d", 0:23)


############# Collating data ###############

otms <- 1:357

#not retrieved: 344, 345
#faulty: 160, 250

out <- c(160, 250, 344, 345)


otms <- otms[-out]
otms



mydata <- array(NA, dim=c(length(dates), length(otms)), dimnames=list(as.character(dates),otms))

for (o in 1:length(otms))
{
  #o=62
  cat(o, "/")
  otm.data <- read.csv(paste(path, otms[o], ".csv", sep=""), header=F, sep=",")
  
  colnames(otm.data) <- c("date", "temperature", "unit")
  otm.data[,"date"] <- as.character(otm.data[,"date"])
  
  #fixing formatting issue You might not need this if this doesn't happen with your data 
  if(substr(otm.data[1,1],1,3)=="﻿"){otm.data[1,1] <- gsub("﻿", "", otm.data[1,1])}
  
  # vector of dates from the otm.data file in the same format as the dates vector
  otm.dates <- as.POSIXct(gsub("/", "-", otm.data[,"date"]), tz="")
  
  # identifying temporal resolution of otm.data
  res <- otm.dates[2] - otm.dates[1]
  
  # if resolution of otm.data is 15minutes I can use the dates vector as originally defined at that resolution; if resolution of otm.data is 30min I need to subset the dates vector to include only every other timestamp, i.e. 00 and 30 minutes after the hour. We also need to subset the vector of rows that we need to fill in in the storage object (which is at 15min resolution), i.e. when resolution is 15min we only have to fill in every other row (and every other row will be NA.
  # You can run both lines below but only one of them will be true (epending on whether res==15 or res==30 is TRUE) and thus only what's inside the correspinding curly brackets will run.
  # Below we will use the seq.dates and seq.rows created here
  if(res==15) {seq.dates <- dates; seq.rows <- 1:length(dates); thres <- "15 mins"}
  if(res==30) {seq.dates <- dates[substr(dates, 15,16) %in% c("00", "30")]; seq.rows <- seq(1,length(dates), by=2); thres <- "30 mins"}
  
  # For each element of my vector of dates or rows as defined above, find the closest date in the otm.data file and fill the corresponding row in the storage object with the temperaure value from the otm.data file for that date
  
  for (i in 1:length(seq.rows)) 
  {
    # identify date in the position i of the dates vector (the dates vector will be the one at 15 or 30min resolution as defined above)
    my.date <- seq.dates[i]
    
    # identify the position in the otm.data file that has the closest date to my.date (minimum of the differences between my.date and each of the dates in the otm.data file)
    position <- which(abs(my.date - otm.dates) == min(abs(my.date - otm.dates)))
    
    # fil l the row seq.rows[i] (my.date) and column o (the otm for this otm.data file) of the storage object with the temperature value taken from the selected position of the otm.data file 
    mydata[seq.rows[i],as.character(otms[o])] <- otm.data[position,"temperature"]
  }
}
#write.table(mydata, file=paste(path, "OTM_data_December", sep=""), row.names = T, sep="\t")
save(mydata, file=paste(path, "OTM_data_December", sep=""))




################################################################
########## REARRANGING AND CATEGORISING THE DATASET ############
################################################################

# Territories
BGR.otm <- as.character(c(1:21))

BRY.otm <- as.character(c(22:42))

BWB.otm <- as.character(c(43:63))

BWG.otm <- as.character(c(64:84))

BYG.otm <- as.character(c(85:105))

GBG.otm <- as.character(c(106:126))

GBR.otm <- as.character(c(127:147))

GBW.otm <- as.character(c(148:159, 161:168))

GYB.otm <- as.character(c(169:189))

RBR.otm <- as.character(c(190:210))

RGB.otm <- as.character(c(211:231))

RGW.otm <- as.character(c(232:249, 251, 252))

WBY.otm <- as.character(c(253:273))

WGB.otm <- as.character(c(274:294))

WYB.otm <- as.character(c(295:315))

YBR.otm <- as.character(c(316:336))

YWY.otm <- as.character(c(337:343, 346:357))


### Loading file with OTM characteristics
OTMs <- read.table(paste0(path, "OTM_list_December.txt"), header=T, sep="\t")


### Loading the merged OTM data
mydata <- get(load(paste(path, "OTM_data_December", sep="")))


### Remove nighttime
daytime <- sprintf("%02d", 8:18)

mydata <- mydata[which(substr(rownames(mydata), 12,13) %in% daytime==TRUE),]
mydata <- as.data.frame(mydata)              


### Days
days <- unique(substr(rownames(mydata), 6, 10))


### Adding columns for Date and Time
Date <- substr(rownames(mydata), 6, 10)
Time <- substr(rownames(mydata), 12, 19)
mydata <- cbind(Date, Time, mydata)


### Converting to long format 
dataf <- melt(as.data.frame(mydata), id=c("Date", "Time"))
head(dataf)
colnames(dataf) <- c("Date", "Time", "OTM", "Te")
head(dataf)


### Categorising by territory
Lizard_ID <- unique(OTMs$Lizard_ID)
for (tt in 1:length(Lizard_ID))
{
  numbers <- OTMs[OTMs$Lizard_ID==Lizard_ID[tt], "OTM"]
  rows <- dataf$OTM %in% as.character(numbers)
  dataf[rows, "Territory"] <- Lizard_ID[tt]
}


### Categorising by microsite type
microsites_ <- unique(OTMs$Sun)
microsites <- gsub(" ", ".", microsites_)

for (m in 1:length(microsites))
{
  numbers <- OTMs[OTMs$Sun==microsites_[m], "OTM"]
  rows <- dataf$OTM %in% as.character(numbers)
  dataf[rows, "Microsite"] <- microsites[m]
}


### Categorising by substrate
temp.types <- unique(OTMs$Temp)

for (s in 1:length(temp.types))
{
  numbers <- OTMs[OTMs$Temp==temp.types[s], "OTM"]
  rows <- dataf$OTM %in% as.character(numbers)
  dataf[rows, "Substrate"] <- temp.types[s]
}

head(dataf)
#write.table(dataf, file=paste0(path, "categorised_data_"), row.names=T, sep="\t")
save(dataf, file=paste0(path, "categorised_data_"))



################################################################
########## CALCULATING NUMBER OF HOURS WITHIN Tpref ############
################################################################

### Loading list of tprefs for each territory
tpref.list <- read.table(paste0(path, "Tpref_SS_80_bestrun.txt"), header=T, sep="\t")


### Classifiying each Te value as within or outside Tpref range for a given territory
for (tt in 1:length(Lizard_ID))
{
  #identifying the tpref range for selected territory
  tpref1 <- tpref.list[which(tpref.list$Lizard_ID==as.character(Lizard_ID[tt])),"Topt1_80"]
  tpref2 <- tpref.list[which(tpref.list$Lizard_ID==as.character(Lizard_ID[tt])),"Topt2_80"]
  
  #rows within Tpref
  rows <- which(dataf$Territory==Lizard_ID[tt] & dataf$Te >= tpref1 & dataf$Te <= tpref2)
  dataf[rows, "Tpref.class"] <- 1
  
  #rows outside Tpref
  rows <- which(dataf$Territory==Lizard_ID[tt] & dataf$Te < tpref1 | dataf$Te > tpref2)
  dataf[rows, "Tpref.class"] <- 0
}
head(dataf)


### Calculating the mean number of hours (across OTMs for a given microsite) for each day and territory
for (tt in 1:length(Lizard_ID))
{
  cat(as.character(Lizard_ID[tt]), "/")
  
  # Object to store the results for each territory
  htpref <- array(NA, dim=c(length(days), length(microsites)), dimnames = list(days, microsites))
  
  # Loop for each microsite
  for (m in 1:length(microsites))
  {
    # selecting the territory and microsite
    subdata <- subset(dataf, Territory==Lizard_ID[tt] & Microsite==microsites[m])
    
    # Loop for each day
    for (d in 1:length(days))
    {
      data.day <- subset(subdata, Date==days[d])
      
      # identifying the OTM numbers
      my.otms <- unique(data.day$OTM)
      
      # temporary object to store the results for each otm 
      temporary.object <- rep(NA, length(my.otms))
      
      # Loop for each otm
      for (o in 1:length(my.otms))
      {
        # selecting data for a given otm
        data.otm <- subset(data.day, OTM==my.otms[o])
        
        # summing the ones for that otm
        sum.1s <- length(which(data.otm$Tpref.class==1))
        
        # calculating the number of hours corresponding to that sum -- this will depend on the temporal resolution of the data for this given otm (15 min means dividing the number of ones by 4, otherwise by 2)
        res <- as.numeric(substr(data.otm[2,"Time"],4,5)) - as.numeric(substr(data.otm[1,"Time"],4,5))
        if(res==15){temporary.object[o] <- sum.1s/4}
        if(res==30){temporary.object[o] <- sum.1s/2}
      }
      # filling in the mean of the number of hours for all otms (for a given day and microsite) in my storage object for a given territory (raw is the day and column is the microsite)
      htpref[d,m] <- mean(temporary.object, na.rm=T)
    }
  }
  
  # once all days and microsites done, save the territory object
  write.table(htpref, file=paste0(path, "Mean_number_hours_within_Tpref_across_OTMs_for_each_day_for_territory_", Lizard_ID[tt]), row.names = T, sep="\t")
}

str(htpref)
head(htpref)
boxplot(htpref)

