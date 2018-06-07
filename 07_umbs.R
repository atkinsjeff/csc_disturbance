# Comparing Colonial Point from 2014 to 2017.
require(plyr)
require(dplyr)
require(tidyr)
require(raster)
require(rgdal)
require(maps)
require(ggplot2)
require(cluster)

umbs <- read.csv("./data/umbs_data.csv")

  #removing watershed 4
 umbs %>% filter(year == "2012") -> umbs.12

umbs.12$year <- factor(umbs.12$year)

require(randomForest)
set.seed(400)

umbs.12.ks <- randomForest(as.factor(site) ~ mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el +
                          mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                          top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl, 
                        data = umbs.12,
                        importance = TRUE,
                        ntree = 2000)

umbs.12.ks

varImpPlot(umbs.12.ks)

umbs.12.fit <- randomForest(as.factor(site) ~  mean.height.rms + mean.height + mode.el + mean.max.ht + porosity +
                              mode.2 + enl + rugosity + clumping.index + height.2,
                         data = umbs.12,
                         importance = TRUE,
                         ntree = 2000)

umbs.12.fit

varImpPlot(umbs.12.fit)

umbs %>% filter(year == "2016") -> umbs.16

umbs.16.ks <- randomForest(as.factor(site) ~ mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el +
                             mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                             top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl, 
                           data = umbs.16,
                           importance = TRUE,
                           ntree = 2000)

umbs.16.ks
varImpPlot(umbs.16.ks)

umbs.16.fit <- randomForest(as.factor(site) ~ rugosity + max.can.ht + top.rugosity + clumping.index + porosity + mean.max.ht, 
                           data = umbs.16,
                           importance = TRUE,
                           ntree = 2000)

umbs.16.fit

varImpPlot(umbs.16.fit)