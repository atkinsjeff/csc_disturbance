### HARVARD

#
library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
require(ggplot2)
require(tidyverse)
require(randomForest)

hf <- read.csv("./data/hemlock_2017_output.csv")


##### clustering
# K-Means Cluster Analysis
fit <- kmeans(hf[, 8:34], 3) # 5 cluster solution
# get cluster means 
aggregate(hf, by=list(fit$cluster), FUN=mean)

hf %>% group_by(fit.cluster) %>%
  summarise(mean(rugosity),
            mean(mode.2),
            mean(max.can.ht))

# append cluster assignment
hf <- data.frame(hf, fit$cluster)

#################
hf %>%
  group_by(class) %>%
  summarise(mean(mean.vai))
  

# One Way Anova (Completely Randomized Design)
fit <- aov(mean.vai ~ class, data = hf)



#### playing with rugosity
x11()
ggplot(hf, aes(x = as.factor(fit.cluster), y = rugosity))+
  geom_boxplot()

x11()
ggplot(hf, aes(x = as.factor(column), y = rugosity))+
  geom_boxplot()+
  theme_bw()+
  xlab("Column")

x11()
ggplot(hf, aes(x = as.factor(row * 20), y = rugosity))+
  geom_boxplot()+
  theme_bw()+
  xlab("Distance (m)")+
  ylab("Canopy Rugosity (m)")
# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(hf[, 8:34], fit$cluster)

#### remove cluster 2 as that is red pine forest
hf <- subset(hf, hf$fit.cluster != 2)
#### CLASSIFCATION

# for reproducibility, best to set a seed
set.seed(666)

# I always do a comparison at first of making a kitchen sink model, i.e. one that contains as much info as the model can take. You can't have more predictors than rows though. So that can be a problem sometimes. 
hf.kitchen.sink <- randomForest(as.factor(fit.cluster) ~  mean.height +	height.2 + mean.height.var +	mean.height.rms +	mode.el +	max.el + mode.2 + max.can.ht	+ mean.max.ht +	mean.vai +	mean.peak.vai +	deep.gap.fraction + porosity	+	rugosity +
                                  top.rugosity + sky.fraction	+ rumple +	clumping.index +	enl,
                                data = hf,
                                importance = TRUE,
                                ntree = 2000)


# View the forest results.
print(hf.kitchen.sink) 

# Importance of each predictor.
print(importance((hf.kitchen.sink)) )

# This plots eveything
varImpPlot(hf.kitchen.sink)


### more tuned model
hf.fit <- randomForest(as.factor(fit.cluster) ~ rugosity + mode.2 + max.can.ht,
                       data = hf,
                       importance = TRUE,
                       ntree = 2000)

hf.fit

varImpPlot(hf.fit)


# 
# 
# hf.fit <- randomForest(x = hf[,8:34], y = NULL, ntree = 2000, proximity = TRUE, oob.prox = TRUE)
# 
# hclust.hf <- hclust(as.dist(1 - hf.fit$proximity), method = "ward.D2")
# 
# hf.cluster = cutree(hclust.hf, k=3)
# 
# hf$rf.clusters <- hf.cluster
# table(hf.cluster, hf$row)
# 
# 
# 
#logistic regression to test against mortality data
hf.test <- read.csv("hf_mortality_cluster.csv")

str(hf.test)

hf.test %>% group_by(cluster) %>%
  summarise(mean(dead.per),
            sd(dead.per))

#make cluster a factor
hf.test$cluster <- as.factor(hf.test$logit)

#logistic regression
hf.glm <- glm(logit ~ dead.per, data = hf.test, family = "binomial")

summary(hf.glm)

x11()
plot(hf.test$dead.per, hf.test$logit)
curve(predict(hf.glm, data.frame(dead.per = x), type = "resp"), add = TRUE)
points(hf.test$dead.per, fitted(hf.glm),pch=20)


####  filter for plotting
# hf %>%
#   filter(!rf.clusters == 1) -> hfx

#add class
hf$class[hf$fit.cluster == 1] <- "HIGH"
hf$class[hf$fit.cluster == 3] <- "LOW"


#### #plots
x11(width = 4, height = 4)
p.hf.rc <- ggplot(hf, aes(x = as.factor(class), y = rugosity, fill = as.factor(fit.cluster) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(R[C]))+
  guides(fill=FALSE)

ggsave(p.hf.rc, filename = "./plots/hf_rc_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.hf.mode2 <- ggplot(hf, aes(x = as.factor(class), y = mode.2, fill = as.factor(rf.clusters) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(sigma*Z[max]*" (m)"))+
  guides(fill=FALSE)

ggsave(p.hf.mode2, filename = "./plots/hf_mode2_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

p.hf.max.ht <- ggplot(hfx, aes(x = as.factor(class), y = max.can.ht, fill = as.factor(rf.clusters) ))+
  geom_boxplot()+
  theme_jeff()+
  xlab("")+
  ylab(expression(H[max]))+
  guides(fill=FALSE)

ggsave(p.hf.max.ht, filename = "./plots/hf_maxht_change.tiff", width = 3, height = 3, units = "in", dpi = 600, device='tiff')

##### looking at rows
hf(which.min[hf$rugosity])
hf$plot[which.min(hf$rugosity)]
hf$plot[which.max(hf$rugosity)]