require(forestr)

data_dir <- ("./data/pcl/ise/")
process_multi_pcl(data_dir, marker.spacing = 10, pavd = TRUE)




require(ggplot2)
require(viridis)
#m <- read.csv("./output/FERN_03_13_1_output_hit_matrix.csv")
m <- read.csv("./output/FERN_W7_06_1_output_hit_matrix.csv")

max.vai <- 8
transect.length <- 20
max.ht <- 30
m$vai[m$vai == 0] <- NA

x11()
vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
p <- ggplot2::ggplot(m, ggplot2::aes(x = xbin, y = zbin))+
  ggplot2::geom_tile(ggplot2::aes(fill = vai))+
  scale_fill_viridis(option = "E",
                     na.value = "white",
                     limits=c(0, max.vai),
                     name=vai.label)+
  # ggplot2::scale_fill_gradient(low="purple", high="yellow",
  #                              na.value = "white",
  #                              limits=c(0, max.vai),
  #                              name=vai.label)+
  #scale_y_continuous(breaks = seq(0, 20, 5))+
  # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  theme(legend.position = "top")+
  ggplot2::xlim(0,transect.length)+
  ggplot2::ylim(0,max.ht)+
  ggplot2::xlab("Distance along transect (m)")+
  ggplot2::ylab("Height above ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 6, height = 5)
p
#ggsave(p, filename = "grsm_53_viridis_max_at_six.tiff", width = 8, height = 6, units = "in", dpi = 300, device='tiff')

###Export all files from SCENE in ptx format. ONLY the sky should be filtered.

library(data.table)
library(dplyr)

##### PAVD
total.vai <- sum(m$vai, na.rm = TRUE)
df.z <- stats::aggregate(vai ~ zbin, data = m, FUN = sum)

df.z$ratio.vai <- df.z$vai / transect.length

# Shape factor for spline

#plot_sum_vai<-aggregate(dxw ~ scan + z_bin, data = vai_w, mean)

####PAVD

require(ggplot2)

x11()
ggplot(df.z, aes(y= vai, x = zbin)) +
  geom_path(size = 1) +
  coord_flip() +
  theme_bw() +
  xlim(0,40)+
  ylab( expression(PAVD ~ (m^{2} ~ m^{-2}))) + xlab( expression(Height ~ (m))) +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  theme(axis.title.y = element_text(vjust=1.5)) +
  theme(plot.title = element_text(vjust=2.5)) +
  theme(legend.position= "none")

# Shape factor for spline
sf <- sum(df.z$ratio.vai > 0) - 1

pavd <- ggplot2::ggplot(df.z, ggplot2::aes(y = df.z$ratio.vai, x = df.z$zbin))+
  #geom_bar(stat = "identity", color = "light grey")+
  ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf))+
  #geom_path(size = 1) +
  ggplot2::theme_classic()+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x= ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  ggplot2::coord_flip(xlim = c(0,30), ylim = c(0, 1), expand = TRUE)+
  #ggplot2::ylab("Plant Area Volume Density (PAVD)")+
  ylab(vai.label)+
  ggplot2::xlab("Height Above Ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 3, height = 5)
pavd



################################
##### GRSM

# 2016
require(ggplot2)
require(viridis)
m <- read.csv("./output/GRSM_64C_output_hit_matrix.csv")
#m <- read.csv("./output/FERN_W7_06_1_output_hit_matrix.csv")

max.vai <- 8
transect.length <- 40
max.ht <- 40
m$vai[m$vai == 0] <- NA

x11()
vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
p <- ggplot2::ggplot(m, ggplot2::aes(x = xbin, y = zbin))+
  ggplot2::geom_tile(ggplot2::aes(fill = vai))+
  scale_fill_viridis(na.value = "white",
                     limits=c(0, max.vai),
                     name=vai.label)+
  # ggplot2::scale_fill_gradient(low="purple", high="yellow",
  #                              na.value = "white",
  #                              limits=c(0, max.vai),
  #                              name=vai.label)+
  #scale_y_continuous(breaks = seq(0, 20, 5))+
  # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  ggplot2::xlim(0,transect.length)+
  ggplot2::ylim(0,max.ht)+
  ggplot2::xlab("Distance along transect (m)")+
  ggplot2::ylab("Height above ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 6, height = 5)
p
#ggsave(p, filename = "grsm_53_viridis_max_at_six.tiff", width = 8, height = 6, units = "in", dpi = 300, device='tiff')

###Export all files from SCENE in ptx format. ONLY the sky should be filtered.

library(data.table)
library(dplyr)

##### PAVD
total.vai <- sum(m$vai, na.rm = TRUE)
df.z <- stats::aggregate(vai ~ zbin, data = m, FUN = sum)

df.z$ratio.vai <- df.z$vai / transect.length

# Shape factor for spline

#plot_sum_vai<-aggregate(dxw ~ scan + z_bin, data = vai_w, mean)

####PAVD

require(ggplot2)

x11()
ggplot(df.z, aes(y= vai, x = zbin)) +
  geom_path(size = 1) +
  coord_flip() +
  theme_bw() +
  xlim(0,40)+
  ylab( expression(PAVD ~ (m^{2} ~ m^{-2}))) + xlab( expression(Height ~ (m))) +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  theme(axis.title.y = element_text(vjust=1.5)) +
  theme(plot.title = element_text(vjust=2.5)) +
  theme(legend.position= "none")

# Shape factor for spline
sf <- sum(df.z$ratio.vai > 0) - 1

pavd <- ggplot2::ggplot(df.z, ggplot2::aes(y = df.z$ratio.vai, x = df.z$zbin))+
  #geom_bar(stat = "identity", color = "light grey")+
  ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf))+
  #geom_path(size = 1) +
  ggplot2::theme_classic()+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x= ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  ggplot2::coord_flip(xlim = c(0,40), ylim = c(0, 0.15), expand = TRUE)+
  #ggplot2::ylab("Plant Area Volume Density (PAVD)")+
  ylab(vai.label)+
  ggplot2::xlab("Height Above Ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 3, height = 5)
pavd


##########################
# 2017
require(ggplot2)
require(viridis)
m <- read.csv("./output/GRSM_064C_2017_output_hit_matrix.csv")
#m <- read.csv("./output/FERN_W7_06_1_output_hit_matrix.csv")

max.vai <- 8
transect.length <- 40
max.ht <- 40
m$vai[m$vai == 0] <- NA

x11()
vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
p <- ggplot2::ggplot(m, ggplot2::aes(x = xbin, y = zbin))+
  ggplot2::geom_tile(ggplot2::aes(fill = vai))+
  scale_fill_viridis(na.value = "white",
                     limits=c(0, max.vai),
                     name=vai.label)+
  # ggplot2::scale_fill_gradient(low="purple", high="yellow",
  #                              na.value = "white",
  #                              limits=c(0, max.vai),
  #                              name=vai.label)+
  #scale_y_continuous(breaks = seq(0, 20, 5))+
  # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  ggplot2::xlim(0,transect.length)+
  ggplot2::ylim(0,max.ht)+
  ggplot2::xlab("Distance along transect (m)")+
  ggplot2::ylab("Height above ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 6, height = 5)
p
#ggsave(p, filename = "grsm_53_viridis_max_at_six.tiff", width = 8, height = 6, units = "in", dpi = 300, device='tiff')

###Export all files from SCENE in ptx format. ONLY the sky should be filtered.

library(data.table)
library(dplyr)

##### PAVD
total.vai <- sum(m$vai, na.rm = TRUE)
df.z <- stats::aggregate(vai ~ zbin, data = m, FUN = sum)

df.z$ratio.vai <- df.z$vai / transect.length

# Shape factor for spline

#plot_sum_vai<-aggregate(dxw ~ scan + z_bin, data = vai_w, mean)

####PAVD

require(ggplot2)

x11()
ggplot(df.z, aes(y= vai, x = zbin)) +
  geom_path(size = 1) +
  coord_flip() +
  theme_bw() +
  xlim(0,40)+
  ylab( expression(PAVD ~ (m^{2} ~ m^{-2}))) + xlab( expression(Height ~ (m))) +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  theme(axis.title.y = element_text(vjust=1.5)) +
  theme(plot.title = element_text(vjust=2.5)) +
  theme(legend.position= "none")

# Shape factor for spline
sf <- sum(df.z$ratio.vai > 0) - 1

pavd <- ggplot2::ggplot(df.z, ggplot2::aes(y = df.z$ratio.vai, x = df.z$zbin))+
  #geom_bar(stat = "identity", color = "light grey")+
  ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf))+
  #geom_path(size = 1) +
  ggplot2::theme_classic()+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x= ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  ggplot2::coord_flip(xlim = c(0, 40), ylim = c(0, 0.15), expand = TRUE)+
  #ggplot2::ylab("Plant Area Volume Density (PAVD)")+
  ylab(vai.label)+
  ggplot2::xlab("Height Above Ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 3, height = 5)
pavd


#####################
# HBEF
## HEAVY TREATMENT
m <- read.csv("./output/hbefise_2017_p5_t3_output_hit_matrix.csv")
#m <- read.csv("./output/FERN_W7_06_1_output_hit_matrix.csv")

max.vai <- 8
transect.length <- 30
max.ht <- 30
m$vai[m$vai == 0] <- NA

x11()
vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
p <- ggplot2::ggplot(m, ggplot2::aes(x = xbin, y = zbin))+
  ggplot2::geom_tile(ggplot2::aes(fill = vai))+
  scale_fill_viridis(na.value = "white",
                     limits=c(0, max.vai),
                     name=vai.label)+
  # ggplot2::scale_fill_gradient(low="purple", high="yellow",
  #                              na.value = "white",
  #                              limits=c(0, max.vai),
  #                              name=vai.label)+
  #scale_y_continuous(breaks = seq(0, 20, 5))+
  # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  ggplot2::xlim(0,transect.length)+
  ggplot2::ylim(0,max.ht)+
  ggplot2::xlab("Distance along transect (m)")+
  ggplot2::ylab("Height above ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 6, height = 5)
p
#ggsave(p, filename = "grsm_53_viridis_max_at_six.tiff", width = 8, height = 6, units = "in", dpi = 300, device='tiff')

###Export all files from SCENE in ptx format. ONLY the sky should be filtered.

library(data.table)
library(dplyr)

##### PAVD
total.vai <- sum(m$vai, na.rm = TRUE)
df.z <- stats::aggregate(vai ~ zbin, data = m, FUN = sum)

df.z$ratio.vai <- df.z$vai / transect.length

# Shape factor for spline

#plot_sum_vai<-aggregate(dxw ~ scan + z_bin, data = vai_w, mean)

####PAVD

require(ggplot2)

x11()
ggplot(df.z, aes(y= vai, x = zbin)) +
  geom_path(size = 1) +
  coord_flip() +
  theme_bw() +
  xlim(0,40)+
  ylab( expression(PAVD ~ (m^{2} ~ m^{-2}))) + xlab( expression(Height ~ (m))) +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  theme(axis.title.y = element_text(vjust=1.5)) +
  theme(plot.title = element_text(vjust=2.5)) +
  theme(legend.position= "none")

# Shape factor for spline
sf <- sum(df.z$ratio.vai > 0) - 1

pavd <- ggplot2::ggplot(df.z, ggplot2::aes(y = df.z$ratio.vai, x = df.z$zbin))+
  #geom_bar(stat = "identity", color = "light grey")+
  ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf))+
  #geom_path(size = 1) +
  ggplot2::theme_classic()+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x= ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  ggplot2::coord_flip(xlim = c(0,30), ylim = c(0, 1), expand = TRUE)+
  #ggplot2::ylab("Plant Area Volume Density (PAVD)")+
  ylab(vai.label)+
  ggplot2::xlab("Height Above Ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 3, height = 5)
pavd


##########################
# CONTROL
require(ggplot2)
require(viridis)
m <- read.csv("./output/hbef_ise_2017_p7_t1_output_hit_matrix.csv")
#m <- read.csv("./output/FERN_W7_06_1_output_hit_matrix.csv")

max.vai <- 8
transect.length <- 30
max.ht <- 30
m$vai[m$vai == 0] <- NA

x11()
vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
p <- ggplot2::ggplot(m, ggplot2::aes(x = xbin, y = zbin))+
  ggplot2::geom_tile(ggplot2::aes(fill = vai))+
  scale_fill_viridis(na.value = "white",
                     limits=c(0, max.vai),
                     name=vai.label)+
  # ggplot2::scale_fill_gradient(low="purple", high="yellow",
  #                              na.value = "white",
  #                              limits=c(0, max.vai),
  #                              name=vai.label)+
  #scale_y_continuous(breaks = seq(0, 20, 5))+
  # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  ggplot2::xlim(0,transect.length)+
  ggplot2::ylim(0,max.ht)+
  ggplot2::xlab("Distance along transect (m)")+
  ggplot2::ylab("Height above ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 6, height = 5)
p
#ggsave(p, filename = "grsm_53_viridis_max_at_six.tiff", width = 8, height = 6, units = "in", dpi = 300, device='tiff')

###Export all files from SCENE in ptx format. ONLY the sky should be filtered.

library(data.table)
library(dplyr)

##### PAVD
total.vai <- sum(m$vai, na.rm = TRUE)
df.z <- stats::aggregate(vai ~ zbin, data = m, FUN = sum)

df.z$ratio.vai <- df.z$vai / transect.length

# Shape factor for spline

#plot_sum_vai<-aggregate(dxw ~ scan + z_bin, data = vai_w, mean)

####PAVD

require(ggplot2)

x11()
ggplot(df.z, aes(y= vai, x = zbin)) +
  geom_path(size = 1) +
  coord_flip() +
  theme_bw() +
  xlim(0,40)+
  ylab( expression(PAVD ~ (m^{2} ~ m^{-2}))) + xlab( expression(Height ~ (m))) +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  theme(axis.title.y = element_text(vjust=1.5)) +
  theme(plot.title = element_text(vjust=2.5)) +
  theme(legend.position= "none")

# Shape factor for spline
sf <- sum(df.z$ratio.vai > 0) - 1

pavd <- ggplot2::ggplot(df.z, ggplot2::aes(y = df.z$ratio.vai, x = df.z$zbin))+
  #geom_bar(stat = "identity", color = "light grey")+
  ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf))+
  #geom_path(size = 1) +
  ggplot2::theme_classic()+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x= ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank())+
  ggplot2::coord_flip(xlim = c(0, 30), ylim = c(0, 1), expand = TRUE)+
  #ggplot2::ylab("Plant Area Volume Density (PAVD)")+
  ylab(vai.label)+
  ggplot2::xlab("Height Above Ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 3, height = 5)
pavd