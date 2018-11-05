# Comparing Colonial Point from 2014 to 2017.
require(plyr)
require(dplyr)
require(tidyr)
require(raster)
require(rgdal)
require(maps)
require(ggplot2)
require(cluster)
require(corrplot)

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

# umbs.12.fit <- randomForest(as.factor(site) ~  mean.height + height.2 + mean.height.rms + mode.el + mean.max.ht + porosity + rugosity +
#                               mode.2 + enl + rugosity + clumping.index,
#                          data = umbs.12,
#                          importance = TRUE,
#                          ntree = 2000)

umbs.12.fit <- randomForest(as.factor(site) ~  mean.height  +  porosity + rugosity + mode.2,
                            data = umbs.12,
                            importance = TRUE,
                            ntree = 2000, 
                            set.seed(666))

umbs.12.fit



varImpPlot(umbs.12.fit)

# correlations
m.12 <- cor(umbs.12[,c(4,5,7,9, 11, 13,18, 21, 30, 31)])
corrplot(m.12)

# show means
umbs.12 %>%
  group_by(as.factor(site)) %>%
  summarise(mean(mean.height),
            mean(porosity),
            mean(rugosity),
            mean(mode.2))


umbs %>% filter(year == "2016") -> umbs.16

umbs.16.ks <- randomForest(as.factor(site) ~ mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el +
                             mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                             top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl, 
                           data = umbs.16,
                           importance = TRUE,
                           ntree = 2000)

umbs.16.ks
varImpPlot(umbs.16.ks)

umbs.16.fit <- randomForest(as.factor(site) ~ rugosity + max.can.ht + top.rugosity +  porosity, 
                           data = umbs.16,
                           importance = TRUE,
                           ntree = 2000, 
                           set.seed(666))


umbs.16.fit

varImpPlot(umbs.16.fit)

m.16 <- cor(umbs.16[,c(12, 21, 30, 22, 13, 9)])
corrplot(m.16, title = "UMBS/FASET 2016",mar=c(0,0,1,0))

# show means
umbs.16 %>%
  group_by(as.factor(site)) %>%
  summarise(mean(rugosity),
            mean(max.can.ht),
            mean(top.rugosity),
            mean(porosity))

