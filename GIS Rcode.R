
##########################################################################################################
#####Normalized Difference Water Index (NDWI) #############
#### Using the landsat image for the time 01/05/2015-30/09/2015
###Load packages into workspace#####
library(raster)
library(landsat)
library(hsdar)
library(rasterVis)
library(sp)

##For ease of access, lets do a quick way of indexing and loading all bands

raslist <- paste0('C:/Users/david/Downloads/Landsat/LC08_L1TP_185051_20150909_20170404_01_T1_B', 1:7, ".tif")
raslist
landsat <- stack(raslist)

###Access first raster (Band 1) in the stack with double square brackets 
landsat[[1]] 


### The formula for NDWI is: (Green-NIR)/(green+NIR)#######
####Values lie between -1 and 1#####
####-1 is absence of water, and 1 represents area covered by water#####
## Lets create a function for the water index (wi), where bands k and i are used for calculations####
wi <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  wi <- (bk - bi) / (bk + bi)
  return(wi)
}

### Now lets get NDWI values
### NDWI uses green and NIR bands, which means, bands 3 & 5 in LANDSAT
ndwi <- wi(landsat, 5, 3)
plot(ndwi, col = rev(terrain.colors(10)), main = "Landsat-NDWI")


### Lets try another index, the Modified Normalized Water index (MNDWI)####
####The MNDWI is a variation of the NDWI, and an improvement#####
####The formula is as follows: MNWI =(green-SWIR1)/(green+SWIR1)####

wi2 <- function(img, k, i ,n) {
  bk <- img[[k]]
  bi <- img[[i]]
  bn <- img[[n]]
  wi2 <- (bk - bn) /(bk + bn)
  return(wi2) 
}

mndwi <- wi2(landsat, 5, 3, 6)
plot(mndwi, col = rev(terrain.colors(10)), main = "Landsat-MNDWI")


####Compare NDWI plot and MNDWI plot##########
par(mfrow = c(1,2))
plot(ndwi, col = rev(terrain.colors(10)), main = "Landsat-NDwI")
plot(mndwi, col = rev(terrain.colors(10)), main = "Landsat-MNDWI")

