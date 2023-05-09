#########################################################################################
################################### CALCULATING HOME RANGE ##############################
#########################################################################################

### SOURCES:

#https://cran.r-project.org/web/packages/adehabitatHR/vignettes/adehabitatHR.pdf
#https://ecosystems.psu.edu/research/labs/walter-lab/manual/home-range-estimation/link-to-pdf
#http://mikemeredith.net/blog/1212_Data_for_home_range_analysis_in_R.htm


library(adehabitatHR)
#library(maptools)
library(rgdal)
library(ggplot2)


### PREPARING THE DATA:

GPS_coord <- read.csv("GPS_UTM.csv", header = T)

head(GPS_coord)

ggplot(GPS_coord, aes(Lat_UTM, Long_UTM, col=Lizard)) + 
  geom_point(size=2) +   # draw points
  labs(title="Territories")


## turning my point sighting data into a SpatialPointsDataFrame by specifying that the "Lat_UTM" and "Long_UTM" columns are the coordinates:

coordinates(GPS_coord) <- c("Lat_UTM", "Long_UTM")
class(GPS_coord)
#[1] "SpatialPointsDataFrame"
#attr(,"package")
#[1] "sp"

plot(GPS_coord)
plot(GPS_coord, col=GPS_coord$Lizard)

# the function clusthr implements the single-linkage clustering algorithm (see next sections) - Using the function clusthr on this object
#returns an object of class SpatialPolygonsDataFrame
clu <- clusthr(GPS_coord)
class(clu)
#[1] "MCHu"

plot(clu)


## CALCULATING THE MINIMUM CONVEX POLYGON (MCP):

#Note: At least 5 relocations are required to fit an home range

cp <- mcp(GPS_coord[,1], percent=95, unout="m2")  #calculating MCP by excluding 5% of the most extreme relocations 
class(cp)

#excl <- mcp.area(GPS_coord[,1], percent=seq(50, 95, by = 5)) #checking what is the optimal % of extreme values to exclude
#can't run it because it is too big to plot, error: figure margins too large (too many individuals) - but try to run it with subsets
#excl


plot(cp)
plot(GPS_coord, col=GPS_coord$Lizard, add=TRUE)


# Home range area for each individual
as.data.frame(cp)

write.table(cp, file="MCP_area_95_m2.txt", row.names = T, sep="\t")

# exporting shape files for GIS:

#writePolyShape(cp, "homerange") doesn't work

dir.create("MCP_95")
writeOGR(obj=cp, dsn="MCP_95", layer="cp", driver="ESRI Shapefile")


# calculating MCP with 50% (it doesn't seem that I can do this in one step for both 50 and 95%):

cp_2 <- mcp(GPS_coord[,1], percent=50, unout="m2")

as.data.frame(cp_2)

write.table(cp_2, file="MCP_area_50_m2.txt", row.names = T, sep="\t")

dir.create("MCP_50")
writeOGR(obj=cp_2, dsn="MCP_50", layer="cp_2", driver="ESRI Shapefile")



## CALCULATING KERNEL UTILIZATION DISTRIBUTION (kernelUD):


# using the Reference smoothing parameters (REF):

kud <- kernelUD(GPS_coord[,1], h="href")
kud
#Type: probability density
#Smoothing parameter estimated with a  href smoothing parameter
#This object is a list with one component per animal.
#Each component is an object of class estUD
#See estUD-class for more information

image(kud)

#The values of the smoothing parameters are stored in the slot "h" of each
#element of the list. For example, to get the h-value for the first animal:

kud[[1]]@h
#$h
#[1] 44.19264
#$meth
#[1] "href"


#specifying the same grid for all individuals
#kus <- kernelUD(GPS_coord[,1], h="href", same4all=TRUE)
#image(kus)

#coerce the object as spatial data frame
#ii <- estUDm2spixdf(kus)
#class(ii)


homerange <- getverticeshr(kud)
#Error in getverticeshr.estUD(x[[i]], percent, ida = names(x)[i], unin,  : 
#                               The grid is too small to allow the estimation of home-range.
#                             You should rerun kernelUD with a larger extent parameter

kus <- kernelUD(GPS_coord[,1], h="href", grid=80, extent=3)
homerange_95 <- getverticeshr(kus, 95, unout='m2')
homerange_50 <- getverticeshr(kus, 50, unout='m2')

as.data.frame(homerange_95)


kus <- kernelUD(GPS_coord[,1], h="href", extent=1.5) # 1.5 was the minimum sufficient extent that wanted to run
homerange_95 <- getverticeshr(kus, 95, unout='m2')
homerange_50 <- getverticeshr(kus, 50, unout='m2')

as.data.frame(homerange_95)
as.data.frame(homerange_50)

class(homerange_95)
class(homerange_50)

plot(homerange_95, col=1:36) #assigns different colour to each lizard
plot(homerange_50, col=1:36)


write.table(homerange_95, file="KUD_href_95_area_m2.txt", row.names = T, sep="\t")
write.table(homerange_50, file="KUD_href_50_area_m2.txt", row.names = T, sep="\t")


dir.create("KUD_50")
writeOGR(obj=homerange_50, dsn="KUD_50", layer="homerange_50", driver="ESRI Shapefile")

dir.create("KUD_95")
writeOGR(obj=homerange_95, dsn="KUD_95", layer="homerange_95", driver="ESRI Shapefile")



# using the Least Square Cross Validation smoothing parameters instead of REF (LSCV):

#The LSCV algorithm searches for the optimum value of h in the interval specified by the parameter hlim

kud2 <- kernelUD(GPS_coord[,1], h="LSCV")
#getting some warning messages that the model did not converge --> tried increasing hlim but it didn't work
#apparently LSCV often doesn't converge when there is a high density of data points present

image(kud2)

homerange_95 <- getverticeshr(kud2, 95, unout='m2') #extent parameter not large enough (tried different options but none work)
homerange_50 <- getverticeshr(kud2, 50, unout='m2') #extent parameter not large enough (tried different options but none work)

# DOESN'T WORK


##########################################################################################################################################

###### SPLITTING PER SEASON ######


## SUMMER:


### PREPARING THE DATA:

#GPS_coord <- read.csv("GPS_UTM_summer.csv", header = T, sep = ",")

attach(GPS_UTM_summer)

head(GPS_UTM_summer)

ggplot(GPS_UTM_summer, aes(Lat_UTM, Long_UTM, col=Lizard)) + 
  geom_point(size=2) +   # draw points
  labs(title="Territories")


## turning my point sighting data into a SpatialPointsDataFrame by specifying that the "Lat_UTM" and "Long_UTM" columns are the coordinates:

coordinates(GPS_UTM_summer) <- c("Lat_UTM", "Long_UTM")
class(GPS_UTM_summer)
#[1] "SpatialPointsDataFrame"
#attr(,"package")
#[1] "sp"

plot(GPS_UTM_summer)
#plot(GPS_UTM_summer, col=Lizard)

# the function clusthr implements the single-linkage clustering algorithm (see next sections) - Using the function clusthr on this object
#returns an object of class SpatialPolygonsDataFrame
clu <- clusthr(GPS_UTM_summer)
class(clu)
#[1] "MCHu"

plot(clu)


## CALCULATING THE MINIMUM CONVEX POLYGON (MCP):

#Note: At least 5 relocations are required to fit an home range

cp <- mcp(GPS_UTM_summer[,1], percent=95, unout="m2")  #calculating MCP by excluding 5% of the most extreme relocations 
class(cp)

#excl <- mcp.area(GPS_coord[,1], percent=seq(50, 95, by = 5)) #checking what is the optimal % of extreme values to exclude
#can't run it because it is too big to plot, error: figure margins too large (too many individuals) - but try to run it with subsets
#excl


plot(cp)
#plot(GPS_UTM_summer, col=GPS_UTM_summer$Lizard, add=TRUE)


# Home range area for each individual
as.data.frame(cp)


# exporting shape files for GIS:

#writePolyShape(cp, "homerange") doesn't work

dir.create("MCP_95_summer")

write.table(cp, file="MCP_area_95_m2_summer.txt", row.names = T, sep="\t")

writeOGR(obj=cp, dsn="MCP_95_summer", layer="cp", driver="ESRI Shapefile")


# calculating MCP with 50% (it doesn't seem that I can do this in one step for both 50 and 95%):

cp_2 <- mcp(GPS_UTM_summer[,1], percent=50, unout="m2")

as.data.frame(cp_2)

write.table(cp_2, file="MCP_area_50_m2_summer.txt", row.names = T, sep="\t")

writeOGR(obj=cp_2, dsn="MCP_50_summer", layer="cp_2", driver="ESRI Shapefile")


detach(GPS_UTM_summer)


###########################


## AUTUMN:


### PREPARING THE DATA:


attach(GPS_UTM_autumn)

head(GPS_UTM_autumn)

ggplot(GPS_UTM_autumn, aes(Lat_UTM, Long_UTM, col=Lizard)) + 
  geom_point(size=2) +   # draw points
  labs(title="Territories")


## turning my point sighting data into a SpatialPointsDataFrame by specifying that the "Lat_UTM" and "Long_UTM" columns are the coordinates:

coordinates(GPS_UTM_autumn) <- c("Lat_UTM", "Long_UTM")
class(GPS_UTM_autumn)
#[1] "SpatialPointsDataFrame"
#attr(,"package")
#[1] "sp"

plot(GPS_UTM_autumn)

# the function clusthr implements the single-linkage clustering algorithm (see next sections) - Using the function clusthr on this object
#returns an object of class SpatialPolygonsDataFrame
clu <- clusthr(GPS_UTM_autumn)
class(clu)
#[1] "MCHu"

plot(clu)


## CALCULATING THE MINIMUM CONVEX POLYGON (MCP):

#Note: At least 5 relocations are required to fit an home range

cp <- mcp(GPS_UTM_autumn[,1], percent=95, unout="m2")  #calculating MCP by excluding 5% of the most extreme relocations 
class(cp)

#excl <- mcp.area(GPS_coord[,1], percent=seq(50, 95, by = 5)) #checking what is the optimal % of extreme values to exclude
#can't run it because it is too big to plot, error: figure margins too large (too many individuals) - but try to run it with subsets
#excl


plot(cp)

# Home range area for each individual
as.data.frame(cp)


# exporting shape files for GIS:

#writePolyShape(cp, "homerange") doesn't work

dir.create("MCP_95_autumn")

write.table(cp, file="MCP_area_95_m2_autumn.txt", row.names = T, sep="\t")

writeOGR(obj=cp, dsn="MCP_95_autumn", layer="cp", driver="ESRI Shapefile")


# calculating MCP with 50% (it doesn't seem that I can do this in one step for both 50 and 95%):

cp_2 <- mcp(GPS_UTM_autumn[,1], percent=50, unout="m2")

as.data.frame(cp_2)


dir.create("MCP_50_autumn")

write.table(cp_2, file="MCP_area_50_m2_autumn.txt", row.names = T, sep="\t")

writeOGR(obj=cp_2, dsn="MCP_50_autumn", layer="cp_2", driver="ESRI Shapefile")


detach(GPS_UTM_autumn)
