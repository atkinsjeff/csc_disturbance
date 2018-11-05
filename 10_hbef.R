# Comparing Colonial Point from 2014 to 2017.
require(maptools)
require(rgdal)
require(sf)
require(plyr)
require(dplyr)
require(tidyverse)




#### Importing 
hbef <- read.csv("./data/hbef_ise_2015_to_2017.CSV")


#### CLASSIFCATION

require(randomForest)

# for reproducibility, best to set a seed
set.seed(666)

hbef %>%
  filter(!year == 2017) -> hbef.one

# I always do a comparison at first of making a kitchen sink model, i.e. one that contains as much info as the model can take. You can't have more predictors than rows though. So that can be a problem sometimes. 
hbef.one.kitchen.sink <- randomForest(as.factor(year) ~  mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el + mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                                  top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl,
                                data = hbef.one,
                                importance = TRUE,
                                ntree = 2000)


# View the forest results.
print(hbef.one.kitchen.sink) 

# Importance of each predictor.
print(importance((hbef.kitchen.sink)) )

# This plots eveything
varImpPlot(hbef.one.kitchen.sink)


### more tuned model
set.seed(420)
hbef.one.fit <- randomForest(as.factor(year) ~ mean.peak.vai + max.can.ht+  mean.max.ht,
                       data = hbef.one,
                       importance = TRUE,
                       ntree = 2000)

hbef.one.fit

varImpPlot(hbef.one.fit)

# show means
hbef.one %>%
  group_by(year) %>%
  summarise(mean(mean.peak.vai),
            mean(max.can.ht),
            mean(mean.max.ht))


#### year two
hbef %>%
  filter(!year == 2016) -> hbef.two

# I always do a comparison at first of making a kitchen sink model, i.e. one that contains as much info as the model can take. You can't have more predictors than rows though. So that can be a problem sometimes. 
hbef.two.kitchen.sink <- randomForest(as.factor(year) ~  mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el + mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                                        top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl,
                                      data = hbef.two,
                                      importance = TRUE,
                                      ntree = 2000)


# View the forest results.
print(hbef.two.kitchen.sink) 

# Importance of each predictor.
print(importance((hbef.two.kitchen.sink)) )

# This plots eveything
varImpPlot(hbef.two.kitchen.sink)


### more tuned model
set.seed(420)
hbef.two.fit <- randomForest(as.factor(year) ~ mean.peak.vai + rugosity + enl + sky.fraction,
                             data = hbef.two,
                             importance = TRUE,
                             ntree = 2000)

hbef.two.fit

varImpPlot(hbef.two.fit)

hbef.two %>% group_by(year) %>%
  summarise(mean(mean.peak.vai),
            mean(rugosity),
            mean(enl),
            mean(sky.fraction))