# this script analyzes the pre (2016) and post (2017) fire data from NEON site GRSM - Great Smoky Mountains National Park
# 
# 
# 

require(forestr)
require(plyr)
require(dplyr)
require(ggplot2)
require(tidyr)
require(randomForest)

# import grsm data
grsm <- read.csv("./data/grsm_comparison_1113.csv")

str(grsm)

# change year to factor
grsm$year <- factor(grsm$year)


#######################?
# 
# This first model looks at what changes the most from year to year, regardless of severity
# #

# RANDOMFOREST MODEL
set.seed(400)

#kitchen sink test

grsm.ks.fit <- randomForest(as.factor(year) ~  mean.height + height.2 + mean.height.var + mean.height.rms + mode.el +
                              max.el + mode.2 + max.can.ht + mean.max.ht + mean.vai + mean.peak.vai + deep.gaps +
                              deep.gap.fraction + porosity + rugosity + top.rugosity + sky.fraction + rumple + 
                              clumping.index + enl,
                         data = grsm,
                         importance = TRUE,
                         ntree = 2000)
# View the forest results.
print(grsm.ks.fit) 


# Importance of each predictor.
print(importance(grsm.ks.fit)) 

varImpPlot(grsm.ks.fit)

# refined model
grsm.fit <- randomForest(as.factor(year) ~  max.el + mean.vai + mean.max.ht + mean.peak.vai,
                            data = grsm,
                            importance = TRUE,
                            ntree = 2000)
# View the forest results.
print(grsm.fit) 


# Importance of each predictor.
print(importance(grsm.fit)) 

varImpPlot(grsm.fit)

###
###GRSM_55
#GRSM_57 - LOW
#GRSM_64 - LOW
#GRSM_65 - LOW
#GRSM_66 - LOW

# looking at what changes
grsm %>% group_by(year) %>%
  summarise(mean(max.el),
            mean(mean.vai),
            mean(mean.max.ht),
            mean(mean.peak.vai)
  )
