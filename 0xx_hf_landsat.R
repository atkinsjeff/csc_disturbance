# load spatial packages
library(raster)
library(rgdal)
library(rgeos)
## Error in library(rgeos): there is no package called 'rgeos'
library(RColorBrewer)
# turn off factors
options(stringsAsFactors = FALSE)


# Atkins redo of CVI paper on NDVI change using R
# 
# Band 1 Visible (0.45 - 0.52 µm) 30 m.  BLUE
# Band 2 Visible (0.52 - 0.60 µm) 30 m.  GREEN
# Band 3 Visible (0.63 - 0.69 µm) 30 m.  RED
# Band 4 Near-Infrared (0.76 - 0.90 µm) 30 m. NIR
# Band 5 Near-Infrared (1.55 - 1.75 µm) 30 m. SWIR
# Band 6 Thermal (10.40 - 12.50 µm) 120 m. THERMAL IR
# Band 7 SWIR (2.09 - 2.35) 30 m 
# https://landsat.usgs.gov/what-are-best-spectral-bands-use-my-study
library(maptools)  ## For wrld_simpl
library(raster)
library(rgdal)


hf.2010 <- list.files(path = "d:/landsat/hf2010/", pattern = "*\\.tif$", full.names  = TRUE)
hf.2016 <- list.files(path = "d:/landsat/hf2016bad/", pattern = "*\\.tif$", full.names  = TRUE)


plot(raster(hf.2010[1]))

stack.10 <- stack(hf.2010[4:10]) 
stack.16 <- stack(hf.2016[4:10])

# stack.10[[4]] <- (stack.10[[4]] / 16) * 0.001
# stack.10[[3]] <- (stack.10[[3]] / 16) * 0.001
# stack.16[[4]] <- (stack.16[[4]] / 16) * 0.001
# stack.16[[3]] <- (stack.16[[3]] / 16) * 0.001

plotRGB(stack.10, r = 3, g = 2, b = 1, axes = TRUE, stretch = 'lin')

# NDVI uses band 4 of the data (NIR) and band 3 (the red)
ndvi.calc <- function(x) {
  (x[[4]] - x[[3]]) / (x[[4]] + x[[3]])
}

ndvi.calc.landsat8 <- function(x) {
  (x[[5]] - x[[4]]) / (x[[5]] + x[[4]])
}

ndvi.10 <- ndvi.calc(stack.10)
ndvi.16 <- ndvi.calc.landsat8(stack.16)

plot(ndvi.10)
plot(ndvi.16)

####
ndvi.10[ndvi.10 == Inf] <- NA
ndvi.10[ndvi.10 == -Inf] <- NA

ndvi.10[ndvi.10 > 1] <- NA
ndvi.10[ndvi.10 < -1] <- NA

ndvi.16[ndvi.16 == Inf] <- NA
ndvi.16[ndvi.16 == -Inf] <- NA

ndvi.16[ndvi.16 > 1] <- NA
ndvi.16[ndvi.16 < -1] <- NA

##### shapefile hopefully?   shapefile("C:/github/cvi_ndvi_redux/GIS/wymerbasin")
#bound <- shapefile("./data/hf_shapefile/Hemlock_Grid")
plot.corners <- read.csv("./data/hemlock_coordinates.csv")

str(plot.corners)
r <- crs("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" )
# or

bound <- SpatialPointsDataFrame(plot.corners[,3:4], plot.corners, proj4string = r)
# Define the Proj.4 spatial reference 
# http://spatialreference.org/ref/epsg/26915/proj4/
sr <- "+proj=utm +zone=18  +datum=WGS84 +units=m +no_defs" 

# Project Raster
ndvi.10zn19 <- projectRaster(ndvi.10, crs = sr)

hf.ndvi.10 <- crop(ndvi.10zn19, extent(bound), snap = "near")
hf.ndvi.16 <- crop(ndvi.16, extent(bound), snap = "near")
x11()
plot(hf.ndvi.10)
x11()
plot(hf.ndvi.16)


library(rasterVis)


## Set up color gradient with 100 values between 0.0 and 1.0
breaks <- seq(0.70, 0.91, by=0.01)
cols <- colorRampPalette(c("blue", "yellow", "dark green"))(length(breaks)-1)

## Use `at` and `col.regions` arguments to set the color scheme
x11()
levelplot(hf.ndvi.10, at=breaks, col.regions=cols, main = "2010")

x11()
levelplot(hf.ndvi.16, at=breaks, col.regions=cols, main = "2016")


rasterOptions(tolerance = 0.5)
hf.change <- hf.ndvi.10 - hf.ndvi.16

mergedraster <- raster::merge(hf.ndvi.10, hf.ndvi.16, tolerance = 0.5)

plot(mergedraster)


ndvi.16.df <- as.data.frame( rasterToPoints(hf.ndvi.16) )
ndvi.10.df <- as.data.frame( rasterToPoints(hf.ndvi.10) )

dim(ndvi.10.df)
dim(ndvi.16.df)

ndvi.change <- cbind(ndvi.10.df, ndvi.16.df)
write.csv(ndvi.change, "ndvi_change.csv")


##### look at whole stack
crop.16 <- crop(stack.16, extent(bound), snap = "near")
x11()
plot(crop.16)

