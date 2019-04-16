require(plyr)
require(dplyr)
require(tidyverse)
### FERN
fern.pcl <- read.csv("./data/fernow_pcl_transects_2016.CSV")

# make a plotID column

fern.pcl$plotID <-  substr(fern.pcl$plot, 0, 10)

#drop watershed 15
fern <- subset(fern.pcl, fern.pcl$watershedID != "W15")
#fern <- fern.pcl
#### classes
# 2 = W3
#  4 = W7
#  5 = W4
#### CLASSIFCATION

require(randomForest)

#fern$watershedID <- factor(fern$watershedID)

fern.og <- fern

#removing watershed 4
fern %>% filter(!watershedID == "W4") -> x

x$watershedID2 <- ifelse(x$watershedID == "W3", "Treatment", "Control")

x$watershedID <- factor(x$watershedID)
