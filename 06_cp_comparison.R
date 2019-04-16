# Comparing Colonial Point from 2014 to 2017.
require(maptools)
require(rgdal)
require(sf)
require(plyr)
require(dplyr)
require(tidyverse)

# cp.gps <- readOGR("./spatial/LTREB_Plot_Centers_6_27_2014_MichGeoRef.shp")
# cp.gps2 <- readOGR("./spatial/LE_Plots_Corrected_2016.shp")
# 
# str(cp.gps2)

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
cp.fit <- randomForest(as.factor(year) ~ porosity + max.el ,
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

x11(width = 4, height = 4)
p.cp.pc <- ggplot(cp, aes(x = as.factor(year), y = porosity, fill = as.factor(year)))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(P[C]))+
  guides(fill=FALSE)

ggsave(p.cp.pc, filename = "./plots/cp_pc_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.cp.vai.peak <- ggplot(cp, aes(x = as.factor(year), y = mean.peak.vai, fill = as.factor(year)))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(VAI[peak]))+
  guides(fill=FALSE)

ggsave(p.cp.vai.peak, filename = "./plots/cp_peak_vai_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.cp.max.el <- ggplot(cp, aes(x = as.factor(year), y = max.el, fill = as.factor(year)))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(VAI[max]))+
  guides(fill=FALSE)

ggsave(p.cp.max.el, filename = "./plots/cp_maxel_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')


#### loking at data
cp %>%
  dplyr::select(plot, year, mean.peak.vai, max.el, porosity) -> cp.2

cp.2 %>%
  group_by(plot, year) %>%
  summarize(mean(max.el)) -> cp.3

cp.4 <- spread(cp.3, year, `mean(max.el)`)

ggplot(cp.4, aes(x = `2014`, y = `2017`))+
  geom_point(size = 2)+
  theme_jeff()
 


