# Author: Thanos Strantzalis
# date: December, 2013
# Description: this script calculates the burned vegetation area between two modis
# images, in hectares and as a percentage

rm(list = ls())
source("functions.R")

library(raster)
library(sp)
library(MODIS)
library(rgdal)
library(RCurl)
library(bitops)
library(rgeos)




# Download the study area (Ilia province) as a vector file  
studyArea<-raster::getData("GADM", country = "GR",level = 2)
studyArea<-studyArea[studyArea@data$NAME_2=="Ilia",]

# download MODIS ndvi data. 1 image at 13/8/2007(before the fire)
# and 1 image at 14/09/2007 (after the fire) convert and save at working directory
# THIS IS GOING TO TAKE A WHILE (2 IMAGES * 75MB EACH)
runGdal( job="Ilia20070813", product="MOD13Q1", extent=studyArea, begin="2007-08-10", end="2007-08-20",
         SDSstring="101", outProj="EPSG:32634",outDirPath="data/MODIS")
runGdal( job="Ilia20070914", product="MOD13Q1", extent=studyArea, begin="2007-09-10", end="2007-09-20",
         SDSstring="101", outProj="EPSG:32634",outDirPath="data/MODIS")

# call to function ModisCropNDVI() at functions.R
# from a modis tif directory returns only the NDVI layer as a rasterLayer 
# cropped at the desired area. it also sets to NA values outside the study area
# and saves the produced tif image to file
NDVIbefore<-ModisCropNDVI("data/MODIS/Ilia20070813","data/IliaNDVIbefore.tif",studyArea)
NDVIafter<-ModisCropNDVI("data/MODIS/Ilia20070914","data/IliaNDVIafter.tif",studyArea)

# create rasterBrick from the NDVI rasterLayers and fix the map names
ilia<-brick(NDVIbefore,NDVIafter)
names(ilia[[1]])<-"NDVI 13/08/2007"
names(ilia[[2]])<-"NDVI 14/09/2007"


#create vegetation mask (ndvi>6000) and add to rasterBrick
vegetation<-ilia[[1]]
vegetation[vegetation<6000]<-NA
names(vegetation)<-"vegetation_in_Ilia_province"
ilia<-addLayer(ilia,vegetation)


# find diference as a percentage between values at vegetation areas before the fire
# and their values after the fire and add to rasterBrick
differencePerCent<-(((ilia[[3]]-ilia[[2]])/ilia[[3]]) * 100)
names(differencePerCent)<-"NDVI_Difference_Per_Cent"
ilia<-addLayer(ilia,differencePerCent)

#evaluate vegetation areas where ndvi has drop more than 60% and add to rasterBrick
burned<-(ilia[[4]])
burned[burned<60]<-NA
names(burned)<-"Burned_Areas"
ilia<-addLayer(ilia,burned)

#plot the rasterBrick
plot(ilia)


#compute burned area in hectares and print the results
# more info at functions.R, burnedAreaHA() function
burnedVegetationHA<-burnedAreaHA(ilia[[5]])
string<-c('the burned area is',burnedVegetationHA,"hectares")
print(string)

# compute burned area as a percentage of the vegetation area and print the result
# more info at function burnedAreaPerCent() can be found at functions.R
burnedVegetationPercentage<-burnedAreaPerCent(ilia[[5]],ilia[[3]])
string2<-c(burnedVegetationPercentage,"percent,of the vegetation area has been burned")
print(string2)