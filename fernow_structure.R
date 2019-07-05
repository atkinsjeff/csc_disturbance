# working with FERNOW watershed data
require(plyr)
require(dplyr)
require(tidyr)
require(raster)
require(rgdal)
require(maps)
require(ggplot2)
require(cluster)
require(wesanderson)

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

set.seed(400)

fern.ks <- randomForest(as.factor(watershedID) ~ mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el +
  mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
  top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl, 
  data = x,
  importance = TRUE,
  ntree = 2000)


fern.fit <- randomForest(as.factor(watershedID) ~  porosity + max.can.ht + mode.2 + height.2,
                      data = x,
                      importance = TRUE,
                      ntree = 2000,
                      set.seed(666))


# View the forest results.
``


# Importance of each predictor.
print(importance(fern.fit)) 

varImpPlot(fern.fit)

#plots for manuscript

x11(width = 4, height = 4)
p.fernow.pc<- ggplot(x, aes(x = watershedID2, y = porosity, fill = watershedID2))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(P[c]))+
  guides(fill=FALSE)

ggsave(p.fernow.pc, filename = "./plots/fern_pc_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.fernow.max.can<- ggplot(x, aes(x = watershedID2, y = max.can.ht, fill = watershedID2))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(H[max]))+
  guides(fill=FALSE)

ggsave(p.fernow.max.can, filename = "./plots/fern_max_ht_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')
x11()
p.fernow.mode2<- ggplot(x, aes(x = watershedID2, y = mode.2, fill = watershedID2))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(sigma*Z[max]*" (m)" ))+
  guides(fill=FALSE)

ggsave(p.fernow.mode2, filename = "./plots/fern_mode2_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

x11()
p.fernow.h2<- ggplot(x, aes(x = watershedID2, y = height.2, fill = watershedID2))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(sigma*H))+
  guides(fill=FALSE)

ggsave(p.fernow.h2, filename = "./plots/fern_height2_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')


















# mean.height + mean.max.ht + 
fern %>%
  group_by(watershedID) %>%
  summarise(mean(porosity),
            mean(max.can.ht),
            mean(mode.2),
            mean(height.2),
            mean(mean.vai)) -> fern.means
       
   
            

fern.means <-data.frame(fern.means)

# One Way Anova (Completely Randomized Design)
fit <- aov(mean.vai ~ watershedID, data = fern)

##################
# mean.height + mode.2 + height.2 + porosity + rugosity + rumple + max.el + mean.height + max.can.ht + mean.max.ht + top.rugosity +  mode.el
# covariance matrix

drops <- c("X","plot", "siteID", "watershedID", "transect.length", "deep.gaps", "deep.gap.fraction", "plotID")
fern2 <- fern[ , !(names(fern) %in% drops)]
m <- cor(fern2, method = "pearson")

require(corrplot)

x11()
corrplot(m, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#####
##### here are my transparent plots
gg <- ggplot(fern, aes(x = watershedID, y = porosity, fill = watershedID))+ 
  geom_boxplot(color = "white")+
  scale_color_manual(values=c("#FF0000", "#00A08A", "#F2AD00"))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.title = element_text(face="bold", colour="#FFFFFF", size=20),
        axis.text  = element_text(color = "#FFFFFF", size = 16),
        axis.line.x=element_line(color = "#FFFFFF"),
        axis.line.y=element_line(color = "#FFFFFF"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="#FFFFFF", size=10, 
                                   face="bold"),
        legend.position = "bottom")+
  ylab("Porosity")+
  xlab("")

ggsave("fernow_porosity.png", gg, width = 6, height = 4, units = "in", bg = "transparent")


hh <- ggplot(x, aes(x = watershedID, y = max.can.ht, fill = watershedID))+ 
  geom_boxplot(color = "white")+
  scale_color_manual(values=c("#FF0000", "#00A08A", "#F2AD00"))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.title = element_text(face="bold", colour="#FFFFFF", size=20),
        axis.text  = element_text(color = "#FFFFFF", size = 16),
        axis.line.x=element_line(color = "#FFFFFF"),
        axis.line.y=element_line(color = "#FFFFFF"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="#FFFFFF", size=10, 
                                   face="bold"),
        legend.position = "bottom")+
  ylab("Max Canopy Height")+
  xlab("")

ggsave("fernow_rug.png", hh, width = 6, height = 4, units = "in", bg = "transparent")

ii <- ggplot(fern, aes(x = watershedID, y = rumple, fill = watershedID))+ 
  geom_boxplot(color = "white")+
  scale_color_manual(values=c("#FF0000", "#00A08A", "#F2AD00"))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.title = element_text(face="bold", colour="#FFFFFF", size=20),
        axis.text  = element_text(color = "#FFFFFF", size = 16),
        axis.line.x=element_line(color = "#FFFFFF"),
        axis.line.y=element_line(color = "#FFFFFF"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="#FFFFFF", size=10, 
                                   face="bold"),
        legend.position = "bottom")+
  ylab("Rumple")+
  xlab("")

ggsave("fernow_rump.png", ii, width = 6, height = 4, units = "in", bg = "transparent")

### looking at differences
fern$plot[which.min(fern$porosity)]
fern$plot[which.max(fern$porosity)]