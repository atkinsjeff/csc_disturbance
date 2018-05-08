# looking at the GRSM data post fire

require(forestr)
require(plyr)
require(dplyr)
require(ggplot2)
require(tidyr)

data_dir <- ("./data/grsm/")

#process_multi_pcl(data_dir, user_height = 1.05, marker.spacing = 10, max.vai = 8)
#
#
#process_pcl("./data/grsm/GRSM_064W_2017.CSV", max.vai = 8, user_height = 1.05, marker.spacing = 10, pavd = TRUE, hist = FALSE)

grsm <- read.csv("grsm_comparison_1113.CSV")

#Making new variable, canopy half depth

grsm$half.depth <- grsm$mean.max.ht - grsm$mean.height

grsm %>% group_by(year) %>%
  summarise(mean(rugosity),
            mean(mean.height),
            mean(mean.vai),
            mean(mean.max.ht)
            )

# lookin at rugosity
grsm %>% select(plot, year, rugosity) -> grsm.rc

grsm.rc %>% spread(year, rugosity) -> grsm.rc.grouped

colnames(grsm.rc.grouped)[2] <- "rc.16"
colnames(grsm.rc.grouped)[3] <- "rc.17"

grsm.rc.grouped$diff <- grsm.rc.grouped$rc.17 - grsm.rc.grouped$rc.16

#Rugosity
ggplot(grsm.rc.grouped, aes(x = rc.16, y = rc.17))+
  geom_point(size = 3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab("Canopy Rugosity (m) (2017)")+
  xlab("Canopy Rugosity (m) (2016)")+
  ggtitle("Canopy Rugosity")+
  theme_classic()+
  xlim(0,50)+
  ylim(0,50)


grsm %>% select(transect, year, mean.height) %>%
  spread(year, mean.height) -> df.mean.ht

colnames(df.mean.ht)[2] <- "ht.16"
colnames(df.mean.ht)[3] <- "ht.17"

df.mean.ht$diff <- df.mean.ht$ht.17 - df.mean.ht$ht.16

df.mean.ht$percent.change <- (df.mean.ht$diff / df.mean.ht$ht.17) * 100

ggplot(df.mean.ht, aes(x = ht.16, y = ht.17))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  ylab("Mean Leaf Height (m) (2017)")+
  xlab("Mean Leaf Height (m) (2016)")+
  theme_classic()+
  ggtitle("Mean Leaf Height")+
  xlim(0,20)+
  ylim(0,20)

# canopy half depth


grsm %>% select(transect, year, half.depth) %>%
  spread(year, half.depth) -> df.hd

colnames(df.hd)[2] <- "hd.16"
colnames(df.hd)[3] <- "hd.17"

df.hd$diff <- df.hd$hd.17 - df.hd$hd.16

ggplot(df.hd, aes(x = hd.16, y = hd.17))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  ylab("Canopy Half Depth (m) (2017)")+
  xlab("Canopy Half Depth (m) (2016)")+
  theme_classic()+
  ggtitle("Canopy Half Depth (MOCH - mean leaf height)")+
  xlim(0,20)+
  ylim(0,20)



#################

#label for VAI
vai.label =  expression(paste(VAI~(m^2 ~m^-2)))


m.66e <- read.csv("./output/GRSM_64E_output_hit_matrix.CSV")
m.66w <- read.csv("./output/GRSM_64W_output_hit_matrix.CSV")
m.66c <- read.csv("./output/GRSM_64C_output_hit_matrix.CSV")

m.66.16 <- merge(m.66e, m.66c, m.66w,  by = c("xbin", "zbin"))

left_join(m.66c, m.66e, by = c("xbin", "zbin")) %>% 
  left_join(., m.66w, by = c("xbin", "zbin")) -> m.66.16

m.66.16$meanvai <- (m.66.16$vai.x + m.66.16$vai.y + m.66.16$vai) / 3


m.66 <- read.csv("./output/GRSM_064E_2017_output_hit_matrix.CSV")
m.66w <- read.csv("./output/GRSM_064W_2017_output_hit_matrix.CSV")
m.66c <- read.csv("./output/GRSM_064C_2017_output_hit_matrix.CSV")


left_join(m.66c, m.66e, by = c("xbin", "zbin")) %>% 
  left_join(., m.66w, by = c("xbin", "zbin")) -> m.66.17

m.66.17$meanvai <- (m.66.17$vai.x + m.66.17$vai.y + m.66.17$vai) / 3


m5.17 <- read.csv("./output/GRSM_064E_2017_output_hit_matrix.CSV")
  
new.m5 <- merge(m5, m5.17, by = c("xbin", "zbin"))

###select what we want
m.66.16 %>% select(xbin, zbin, meanvai) -> m.16
m.66.17 %>% select(xbin, zbin, meanvai) -> m.17

m6 <- merge(m.16, m.17, by = c("xbin", "zbin"), keep.all = TRUE)

m6 <- setNames(m6, c("xbin", "zbin",  "vai.16", "vai.17"))

plot(m6$vai.16, m6$vai.17)

m6$diff <- m6$vai.17 - m6$vai.16

m6$diff[m6$diff == 0] <- NA


hit.grid.16 <-
  x11(height = 6, width = 8)
  ggplot2::ggplot(m6, ggplot2::aes(x = xbin, y = zbin))+
  ggplot2::geom_tile(ggplot2::aes(fill = diff), color = "white")+
  ggplot2::scale_fill_gradient2(low = "red", mid = "white", high = "blue",
    #ggplot2::scale_fill_gradient(colors = terrain.colors(24),
                        na.value = "white",
                        limits = c(-8, 8),
                        # values = c(0, 0.01, 8),
                        name= vai.label)+
    #scale_fill_gradient(limits=c(-0.75,0.75), breaks=seq(-0.75,0.75,0.25),     low="green", high="red", na.value="transparent")
  #scale_y_continuous(breaks = seq(0, 20, 5))+
  # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20))+
  ggplot2::xlim(0,40)+
  ggplot2::ylim(0,41)+
  ggplot2::xlab("Distance along transect (m)")+
  ggplot2::ylab("Height above ground (m)")+
  #ggplot2::ggtitle(filename)+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

  
  
  
##### TRYING WITH A SPLINE
  
  head(m6)

  
  total.vai.16 <- sum(m6$vai.16)
  total.vai.17 <- sum(m6$vai.17)
  
  df.z.16 <- aggregate(vai.16 ~ zbin, data = m6, FUN = mean)
  df.z.17 <- aggregate(vai.17 ~ zbin, data = m6, FUN = mean)
  
  df.z.16$ratio.vai <- df.z$vai / total.vai
  
  sf <-  ceiling(sum(df.z.16$vai.16 > 0)/2)
  max(df.z$ratio.vai)
  
  df.z <- merge(df.z.16, df.z.17, by = c( "zbin"))
  
  x11()
  ggplot(df.z, aes(y = vai.16, x = zbin))+
    #geom_bar(stat = "identity", color = "light grey")+
    geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf), color = "green", show.legend = TRUE)+
    geom_smooth(method = "lm", se = FALSE, formula = df.z$vai.17 ~ splines::ns(x, sf), color = "blue", show.legend = TRUE)+
    theme_classic()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x= element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20))+
    coord_flip(xlim = NULL, ylim = c(0, max(df.z.16$vai.16) + 0.05), expand = FALSE)+
    ylab(vai.label)+
    xlab("Height Above Ground (m)")
  
  
  
  
#### BIG MEANS
grsm %>% group_by(plot, year) %>% summarize_each(funs(mean)) -> summary.df
  
df <- data.frame(summary.df)



df %>% select(plot, year, mean.height) %>%
  spread(year, mean.height) -> df.mean.ht2

colnames(df.mean.ht2)[2] <- "mh.16"
colnames(df.mean.ht2)[3] <- "mh.17"

x11(height = 3, width = 3)
ggplot(df.mean.ht2, aes(x =mh.16, y = mh.17))+
  geom_point(size = 3, color = "blue")+
  geom_abline(intercept = 0, slope = 1)+
  ylab("Mean Leaf Height (m) (2017)")+
  xlab("Mean Leaf Height (m) (2016)")+
  theme_classic()+
  ggtitle("Mean Leaf Height")+
  xlim(0,20)+
  ylim(0,20)

df %>% select(plot, year, mean.vai) %>%
  spread(year, mean.vai) -> df.vai

colnames(df.vai)[2] <- "mh.16"
colnames(df.vai)[3] <- "mh.17"

x11(height = 3, width = 3)
ggplot(df.vai, aes(x =mh.16, y = mh.17))+
  geom_point(size = 3, color = "orange")+
  geom_abline(intercept = 0, slope = 1)+
  ylab("VAI (2017)")+
  xlab("VAI (2016)")+
  theme_classic()+
  ggtitle("VAI")+
  xlim(0,8.5)+
  ylim(0,8.5)




df %>% select(plot, year, rugosity) %>%
  spread(year, rugosity) -> df.rc

colnames(df.rc)[2] <- "rc.16"
colnames(df.rc)[3] <- "rc.17"

x11(height = 3, width = 3)
ggplot(df.rc, aes(x =rc.16, y = rc.17))+
  geom_point(size = 3, color = "dark green")+
  geom_abline(intercept = 0, slope = 1)+
  ylab("Canopy Rugosity (2017)")+
  xlab("Canopy Rugosity (2016)")+
  theme_classic()+
  ggtitle("Canopy Rugosity")+
  xlim(0,40)+
  ylim(0,40)


df %>% select(plot, year, rumple) %>%
  spread(year, rumple) -> df.r

colnames(df.r)[2] <- "r.16"
colnames(df.r)[3] <- "r.17"

x11(height = 3, width = 3)
ggplot(df.r, aes(x =r.16, y = r.17))+
  geom_point(size = 3, color = "dark red")+
  geom_abline(intercept = 0, slope = 1)+
  ylab("Rumple (2017)")+
  xlab("Rumple (2016)")+
  theme_classic()+
  ggtitle("Rumple")+
  xlim(0,12)+
  ylim(0,12)

df %>% select(plot, year, sky.fraction) %>%
  spread(year, sky.fraction) -> df.gf

colnames(df.gf)[2] <- "gf.16"
colnames(df.gf)[3] <- "gf.17"

df.gf$gf.16 <-  df.gf$gf.16  / 100
df.gf$gf.17 <-  df.gf$gf.17  / 100

x11(height = 3, width = 3)
ggplot(df.gf, aes(x =gf.16, y = gf.17))+
  geom_point(size = 3, color = "purple")+
  geom_abline(intercept = 0, slope = 1)+
  ylab("Gap Fraction (2017)")+
  xlab("Gap Fraction (2016)")+
  theme_classic()+
  ggtitle("Gap Fraction")+
  xlim(0,0.25)+
  ylim(0,0.25)

#######
require(corrplot)
drops <- c("X", "plot_1", "filename", "scan.density", "transect.length")
df <- df[ , !(names(df) %in% drops)]


df.c <- cor(df$year, df[4:28])
p.c <- corrplot(df.c, method = "mixed")
