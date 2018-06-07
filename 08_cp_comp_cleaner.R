# Comparing Colonial Point from 2014 to 2017.
# refined script
#### Importing 
cp <- read.csv("./data/cp_comparison_14_17_pcl.CSV")

require(plyr)
require(dplyr)
require(tidyverse)
require(randomForest)


# Total site
cp %>% group_by(year) %>%
  summarise(mean(rugosity),
            sd(rugosity),
            mean(mean.height),
            mean(max.el),
            mean(mean.max.ht),
            mean(porosity),
            mean(enl),
            mean(sky.fraction),
            mean(deep.gap.fraction),
            mean(rumple),
            mean(clumping.index)
  ) -> cp.stats

cp.stats <- data.frame(cp.stats)

# By plot
cp %>% group_by(year) %>%
  summarise(mean(rugosity),
            sd(rugosity),
            mean(mean.height),
            mean(max.el),
            mean(mean.max.ht),
            mean(porosity),
            mean(enl),
            mean(sky.fraction),
            mean(deep.gap.fraction),
            mean(rumple),
            mean(clumping.index)
  ) -> cp.stats

cp.stats <- data.frame(cp.stats)

#### CLASSIFCATION

# for reproducibility, best to set a seed
set.seed(666)

# I always do a comparison at first of making a kitchen sink model, i.e. one that contains as much info as the model can take. You can't have more predictors than rows though. So that can be a problem sometimes. 
cp.kitchen.sink <- randomForest(as.factor(year) ~  mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el + mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                                  top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl,
                                data = cp,
                                importance = TRUE,
                                ntree = 2000)


# View the forest results.
print(cp.kitchen.sink) 

# Importance of each predictor.
print(importance((cp.kitchen.sink)) )

# This plots eveything
varImpPlot(cp.kitchen.sink)


### more tuned model
cp.fit <- randomForest(as.factor(year) ~ porosity + mean.peak.vai + max.el,
                       data = cp,
                       importance = TRUE,
                       ntree = 2000)

cp.fit

varImpPlot(cp.fit)


### looking at what changes
# By plot
cp %>% group_by(year) %>%
  summarise(mean(rugosity),
            mean(max.el),
            mean(mean.peak.vai),
            mean(porosity))