# Author: Thanos Strantzalis
# date: December 2013
# description: This is an auxilary file containing functions to be used by main.R
#  regerding the final assigment of the geoScripting course


# the ModisCropNDVI() function returns a rasterLayer object containing NDVI values
# cropped in the desired area and sets values<0 to NAs.It also writes the output tif to file
# It take as arguments:
# 1. the path for the MODIS tif images
# 2 the outpout path and name of the produced tif image
# 3. the area which the image is going to be cropped
ModisCropNDVI<-function (inpath,outPath,cropArea){
  
# Find the NDVI layer out of the list of tif files
lstImages<-list.files(inpath, full.names=TRUE)
index<- grep(".NDVI.",lstImages,value=F)
out<-raster(lstImages[index])

#crop the image to the study area and filled values outside the study area with NAs
out<-crop(out,cropArea,filename=outPath,overwrite=TRUE)
out<-mask(out,cropArea)

# set as NAs values less than 0
out[out<0]<- NA

return(out)
}


# compute area burned in hectares(argument rasterLayer of burned area) 
burnedAreaHA <- function (burnedRaster){
  
#First convert to data.frame because for some reason ncell did not work
  out<-as.data.frame(burnedRaster)
  out<-nrow(na.omit(out))
  
# multiply nr of pixel with pixel size and divide by 10000 for HA  
  out<-(out * 250 * 250)/10000
  
  return(out)
}


# the function burnedAreaPerCent takes 2 arguments, a burned raster and the original
# vegetation raster and returns the area burned as a percentage of the original raster
burnedAreaPerCent<-function(burnedRaster,originalRaster){
  
#First convert to data.frame because for some reason ncell did not work  
  burned<-as.data.frame(burnedRaster)
  original<-as.data.frame(originalRaster)
  
# evaluate nr of pixels that are not NAs  
  burned<-nrow(na.omit(burned))
  original<-nrow(na.omit(original))
  
# burned area express as percentage 
  out<-(1-(original - burned)/original)*100
  
  return(out)
  
}


