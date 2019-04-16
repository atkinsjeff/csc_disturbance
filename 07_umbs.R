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
            mean(mode.2),
            mean(mean.vai)) -> umbs.12.means

data.frame(umbs.12.means)

umbs %>% filter(year == "2016") -> umbs.16

umbs.16.ks <- randomForest(as.factor(site) ~ mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el +
                             mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                             top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl, 
                           data = umbs.16,
                           importance = TRUE,
                           ntree = 2000)

umbs.16.ks
varImpPlot(umbs.16.ks)

umbs.16.fit <- randomForest(as.factor(site) ~ rugosity +  top.rugosity +  porosity, 
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
            mean(top.rugosity),
            sd(porosity))

# plots
#2016
x11(width = 4, height = 4)
p.16.rc <- ggplot(umbs.16, aes(x = as.factor(site), y = rugosity, fill = as.factor(site)))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(R[C]))+
  guides(fill=FALSE)

ggsave(p.16.rc, filename = "./plots/umbs16_rc_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.16.rt <- ggplot(umbs.16, aes(x = as.factor(site), y = top.rugosity, fill = as.factor(site)))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(R[T]))+
  guides(fill=FALSE)

ggsave(p.16.rt, filename = "./plots/umbs16_rt_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.16.pc <- ggplot(umbs.16, aes(x = as.factor(site), y = max.el, fill = as.factor(site)))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(P[C]))+
  guides(fill=FALSE)

ggsave(p.16.pc , filename = "./plots/umbs16_pc_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

#2012
x11(width = 4, height = 4)
p.12.rc <- ggplot(umbs.12, aes(x = as.factor(site), y = rugosity, fill = as.factor(site)))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(R[C]))+
  guides(fill=FALSE)

ggsave(p.12.rc, filename = "./plots/umbs12_rc_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.12.pc <- ggplot(umbs.12, aes(x = as.factor(site), y = max.el, fill = as.factor(site)))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(P[C]))+
  guides(fill=FALSE)

ggsave(p.12.pc , filename = "./plots/umbs12_pc_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.12.height <- ggplot(umbs.12, aes(x = as.factor(site), y = mean.height, fill = as.factor(site)))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab("H")+
  guides(fill=FALSE)

ggsave(p.12.height, filename = "./plots/umbs12_height_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.12.sd.vai <- ggplot(umbs.12, aes(x = as.factor(site), y = mode.2, fill = as.factor(site)))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(sigma*Z[max]*" (m)" ))+  
  guides(fill=FALSE)

ggsave(p.12.sd.vai, filename = "./plots/umbs12_mode2_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')
