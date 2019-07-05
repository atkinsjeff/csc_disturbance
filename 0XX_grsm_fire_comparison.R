# this script analyzes the pre (2016) and post (2017) fire data from NEON site GRSM - Great Smoky Mountains National Park
# 
# 
# 

require(forestr)
require(plyr)
require(dplyr)
require(ggplot2)
require(tidyverse)
require(randomForest)
require(ggridges)

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
set.seed(420)
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
  summarise(
            mean(mean.vai),
            mean(mean.max.ht),
            mean(mean.peak.vai)
  )
#test  
# One Way Anova (Completely Randomized Design)
fit <- aov(mean.vai ~ year, data = grsm)

#### rigde lines graph
require(ggplot2)
require(ggridges)

x11(width = 8, height = 8)
ggplot(grsm, aes(y = year, x = max.el, fill = year))+
  geom_density_ridges(alpha = 0.2, scale =5, rel_min_height = 0.01, size = 0.5)+
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), guide = FALSE)+
  ylab("")+
  xlab("VAImax")+
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0))+
  scale_fill_cyclical(breaks = c("2016", "2017"),
                      labels = c(`2016` = "2016", `2017` = "2017"),
                      values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff"),
                      name = "Year", guide = "legend")


x11(width = 8, height = 8)
ggplot(grsm, aes(y = plot))+
  geom_density_ridges(aes(x = max.el, fill = paste(year)),
                      alpha = 0.8, color = "white")+
  labs(x = "VAImax",
       y = "Plot",
       title = "Change in VAImax from 2016 to 2017 by plot at GRSM")+
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_cyclical(breaks = c("2016", "2017"),
                      labels = c(`2016` = "2016", `2017` = "2017"),
                      values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff"),
                      name = "Option", guide = "legend") +
  theme_ridges(grid = FALSE)
  

#####
# grsm %>%
#   dplyr::select(transect, year, max.el) %>%
#   spread(key = year, value = max.el) -> grsm.max.el
# 
# ggplot(grsm.max.el, aes(y = `2016`, x = `2017`))+
#   geom_point(size = 2)+
#   theme_classic()+
#   geom_abline(slope = 1, intercept = 0)+
#   xlim(0,8)+
#   ylim(0,8)
#   




p.grsm.max.el <- ggplot(grsm, aes(x = year, y = max.el, fill = year))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(VAI[max]))+
  guides(fill=FALSE)


ggsave(p.grsm.max.el, filename = "./plots/grsm_vaimax_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

x11(width = 4, height = 4)
p.grsm.vai <- ggplot(grsm, aes(x = year, y = mean.vai, fill = year))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab("VAI")+
  guides(fill=FALSE)

ggsave(p.grsm.vai, filename = "./plots/grsm_vai_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')
  
x11(width = 4, height = 4)
p.grsm.moch<- ggplot(grsm, aes(x = year, y = mean.max.ht, fill = year))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab("MOCH (m)")+
  guides(fill=FALSE)

ggsave(p.grsm.moch, filename = "./plots/grsm_moch_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

x11(width = 4, height = 4)
p.grsm.vai.peak <- ggplot(grsm, aes(x = year, y = mean.peak.vai, fill = year))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(VAI[peak]))+
  guides(fill=FALSE)

ggsave(p.grsm.vai.peak, filename = "./plots/grsm_vai_peak_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

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

### VAI plot

grsm %>%
  select(plot, transect, year, mean.vai) -> grsm.vai

# make long
grsm.vai %>% 
  spread(year, mean.vai) -> grsm.vai.long
  
ggplot(grsm.vai.long, aes(x = `2017`, y = `2016`))+
  geom_point(size = 2)+
  geom_abline(slope = 1)+
  geom_vline(xintercept = 5)+
  theme_jeff()+
  ylim(c(5,8))+
  xlim(c(5,8))
