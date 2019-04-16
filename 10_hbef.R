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

###### ggplot2 theme
theme_jeff <- function () { theme_bw()+                    
    #Setting all text to size 20 and colour to purple/pink                                  
    theme(text=element_text(size=20, colour="black"),    
          #Changing the colour of the axis text to pink  
          axis.text = element_text(colour="black"),
          #..and the axis ticks..        
          axis.ticks = element_blank(),  
          #..and the panel border to purple/pink       
          panel.border = element_rect(colour="black"),  
          #Changing the colour of the major..
          panel.grid.major = element_blank(), 
          #..and minor grid lines to light pink     
          panel.grid.minor = element_blank(),    
          #Setting legend title to size 16..
          #legend.title = element_text(size=16),   
          #..and the legend text to size 14                
          legend.text = element_text(size=14),  
          #Centering plot title text and making it bold                     
          plot.title = element_text(hjust = 0.5, face="bold"),  
          #Centering subtitle                  
          plot.subtitle = element_text(hjust = 0.5))     
}



# for reproducibility, best to set a seed
set.seed(666)

# by treatment level

#### HEAVY
hbef %>%
  filter(treatment == 'heavy' & year != 2017) -> hbef.heavy

# I always do a comparison at first of making a kitchen sink model, i.e. one that contains as much info as the model can take. You can't have more predictors than rows though. So that can be a problem sometimes. 
hbef.heavy.ks <- randomForest(year ~  mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el + mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                                        top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl,
                                      data = hbef.heavy,
                                      importance = TRUE,
                                      ntree = 2000)


# View the forest results.
print(hbef.heavy.ks) 

### more tuned model
set.seed(420)
# by year model

#### HEAVY
hbef.heavy.fit <- randomForest(as.factor(year) ~ mean.peak.vai + mode.2,
                             data = hbef.heavy,
                             importance = TRUE,
                             ntree = 2000)

hbef.heavy.fit

varImpPlot(hbef.heavy.fit)

# plots
p.heavy.peak.vai <- ggplot(hbef.heavy, aes(x = as.factor(year), y = mean.peak.vai, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(VAI[peak]))+
  guides(fill=FALSE)

ggsave(p.heavy.peak.vai,  filename = "./plots/hbef_heavy_peak_vai.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.heavy.mode2 <- ggplot(hbef.heavy, aes(x = as.factor(year), y = mode.2, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(sigma*Z[max] ))+
  guides(fill=FALSE)

ggsave(p.heavy.mode2,  filename = "./plots/hbef_heavy_mode2.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

#means
hbef.heavy %>%
  group_by(year) %>%
  summarise(mean(mean.peak.vai),
            mean(mode.2))
############################
#### MODERATE
hbef %>%
  filter(treatment %in% c('moderate', 'double') & year != 2017) -> hbef.mod

# I always do a comparison at first of making a kitchen sink model, i.e. one that contains as much info as the model can take. You can't have more predictors than rows though. So that can be a problem sometimes. 
hbef.mod.ks <- randomForest(as.factor(year) ~  mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el + mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                                top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl,
                              data = hbef.mod,
                              importance = TRUE,
                              ntree = 2000)


# View the forest results.
hbef.mod.ks
varImpPlot(hbef.mod.ks)

### more tuned model
set.seed(420)

hbef.mod.fit <- randomForest(as.factor(year) ~ mean.peak.vai + mode.2,
                               data = hbef.mod,
                               importance = TRUE,
                               ntree = 2000)

hbef.mod.fit

varImpPlot(hbef.mod.fit)

# plots
p.mod.peak.vai <- ggplot(hbef.mod, aes(x = as.factor(year), y = mean.peak.vai, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(VAI[peak]))+
  guides(fill=FALSE)

ggsave(p.mod.peak.vai,  filename = "./plots/hbef_mod_peak_vai.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.mod.mode2 <- ggplot(hbef.mod, aes(x = as.factor(year), y = mode.2, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(sigma*Z[max] ))+
  guides(fill=FALSE)

ggsave(p.mod.mode2,  filename = "./plots/hbef_mod_mode2.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

#means
hbef.mod %>%
  group_by(year) %>%
  summarise(mean(mean.peak.vai),
            mean(mode.2))
############################
#### LIGHT
hbef %>%
  filter(treatment == 'light' & year != 2017) -> hbef.light

# I always do a comparison at first of making a kitchen sink model, i.e. one that contains as much info as the model can take. You can't have more predictors than rows though. So that can be a problem sometimes. 
hbef.light.ks <- randomForest(as.factor(year) ~  mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el + mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                                top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl,
                              data = hbef.light,
                              importance = TRUE,
                              ntree = 2000)


# View the forest results.
hbef.light.ks
varImpPlot(hbef.light.ks)

### more tuned model
set.seed(420)

hbef.light.fit <- randomForest(as.factor(year) ~ mean.peak.vai + max.el,
                               data = hbef.light,
                               importance = TRUE,
                               ntree = 2000)

hbef.light.fit

varImpPlot(hbef.light.fit)

# plots
p.light.peak.vai <- ggplot(hbef.light, aes(x = as.factor(year), y = mean.peak.vai, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(VAI[peak]))+
  guides(fill=FALSE)

ggsave(p.light.peak.vai,  filename = "./plots/hbef_light_peak_vai.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.light.max.el <- ggplot(hbef.light, aes(x = as.factor(year), y = max.el, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(VAI[max] ))+
  guides(fill=FALSE)

ggsave(p.light.max.el,  filename = "./plots/hbef_light_maxel.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

#means
hbef.light %>%
  group_by(year) %>%
  summarise(mean(mean.peak.vai),
            mean(max.el))

########################

# by disturbance class
hbef.one.fit <- randomForest(treatment ~ rugosity + mode.2,
                             data = hbef.one,
                             importance = TRUE,
                             ntree = 2000)
hbef.one.fit

varImpPlot(hbef.one.fit)






# Importance of each predictor.

# This plots eveything
varImpPlot(hbef.heavy.ks)


# old analysis
hbef %>%
  filter(year == 2016) -> hbef.one

# I always do a comparison at first of making a kitchen sink model, i.e. one that contains as much info as the model can take. You can't have more predictors than rows though. So that can be a problem sometimes. 
hbef.one.kitchen.sink <- randomForest(treatment ~  mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el + mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                                  top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl,
                                data = hbef.one,
                                importance = TRUE,
                                ntree = 2000)


# View the forest results.
print(hbef.one.kitchen.sink) 

# Importance of each predictor.
print(importance((hbef.one.kitchen.sink)) )

# This plots eveything
varImpPlot(hbef.one.kitchen.sink)


### more tuned model
set.seed(420)
# by year model
hbef.one.fit <- randomForest(as.factor(year) ~ mean.peak.vai + mode.2 + mean.max.ht,
                       data = hbef.one,
                       importance = TRUE,
                       ntree = 2000)
# by disturbance class
hbef.one.fit <- randomForest(treatment ~ rugosity+ mode.2,
                             data = hbef.one,
                             importance = TRUE,
                             ntree = 2000)
hbef.one.fit

varImpPlot(hbef.one.fit)

# show means
hbef.one %>%
  group_by(year) %>%
  summarise(mean(mean.peak.vai),
            mean(mode.2),
            mean(mean.max.ht))

# plots for year one
p.vai.max.16 <- ggplot(hbef.one, aes(x = as.factor(year), y = mean.peak.vai, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(VAI[peak]))+
  guides(fill=FALSE)

ggsave(p.vai.max.16,  filename = "./plots/hbef_vai_peak_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.moch.16 <- ggplot(hbef.one, aes(x = as.factor(year), y = mean.max.ht, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(MOCH (m)))+
  guides(fill=FALSE)

ggsave(p.moch.16,  filename = "./plots/hbef_moch_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.mode2.16 <- ggplot(hbef.one, aes(x = as.factor(year), y = mode.2, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(sigma*Z[max]*" (m)" ))+
  guides(fill=FALSE)

ggsave(p.mode2.16,  filename = "./plots/hbef_mode2_peak_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')



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
            mean(sky.fraction))

p.vai.max.17 <- ggplot(hbef.two, aes(x = as.factor(year), y = mean.peak.vai, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(VAI[peak]))+
  guides(fill=FALSE)

ggsave(p.vai.max.17,  filename = "./plots/hbef_vai_peak_change17.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.gf.17 <- ggplot(hbef.two, aes(x = as.factor(year), y = sky.fraction, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(theta))+
  guides(fill=FALSE)

ggsave(p.gf.17,  filename = "./plots/hbef_gf_change17.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.rc.17 <- ggplot(hbef.two, aes(x = as.factor(year), y = rugosity, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(R[C]))+
  guides(fill=FALSE)

ggsave(p.rc.17,  filename = "./plots/hbef_rugosity_change17.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.enl.17 <- ggplot(hbef.two, aes(x = as.factor(year), y = enl, fill = as.factor(year) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab("ENL")+
  guides(fill=FALSE)

ggsave(p.enl.17,  filename = "./plots/hbef_enl_change17.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

