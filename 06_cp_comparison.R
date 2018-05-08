# Comparing Colonial Point from 2014 to 2017.
require(maptools)
require(rgdal)
require(sf)

cp.gps <- readOGR("./spatial/LTREB_Plot_Centers_6_27_2014_MichGeoRef.shp")
cp.gps2 <- readOGR("./spatial/LE_Plots_Corrected_2016.shp")

str(cp.gps2)


#### Importing 
cp <- read.csv("./data/cp_comparison_14_17_pcl.CSV")

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

require(randomForest)
set.seed(666)

cp.kitchen.sink <- randomForest(as.factor(year) ~  mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el +
                         mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                         top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl,
                         data = cp,
                         importance = TRUE,
                         ntree = 2000)


# View the forest results.
print(cp.kitchen.sink) 

# Importance of each predictor.
print(importance((cp.kitchen.sink)) )

varImpPlot(cp.kitchen.sink)


### more tuned model
cp.fit <- randomForest(as.factor(year) ~ porosity + mean.peak.vai + max.el,
                       data = cp,
                       importance = TRUE,
                       ntree = 2000)

cp.fit

varImpPlot(cp.fit)