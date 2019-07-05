#bob data

library(plyr)
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(tidyverse)
library(ggplot2)

# labels
vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
# HBEF ISE DATA
# As a reminder the treatments for each plot are as follows:
#   
#   Plot        Treatment
# 
# 1              Double
# 2              Low
# 3              Moderate
# 4              Control
# 5              High
# 6              Low
# 7              Control
# 8              Double
# 9              High
# 10           Moderate 

#
data_dir <- "./output/ise"
# Importing the double treatment
files <- dir(data_dir, pattern="*2017_p1_t*")

data_frame(filename = files) %>% # create a data frame
  # holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_csv(file.path(data_dir, .))) # a new data column
  )  -> df

df.x <- unnest(df)

df.y <- data.frame(df.x)

# inporting double treatment part two
files <- dir(data_dir, pattern="*2017_p8_t*")

data_frame(filename = files) %>% # create a data frame
  # holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_csv(file.path(data_dir, .))) # a new data column
  )  -> df2

df.x2 <- unnest(df2)

df.y2 <- data.frame(df.x2)

# combine
df.double <-rbind(df.y, df.y2)

#### Importing control
# Importing the double treatment
files <- dir(data_dir, pattern="*2017_p4_t*")

data_frame(filename = files) %>% # create a data frame
  # holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_csv(file.path(data_dir, .))) # a new data column
  )  -> df3

df.x <- unnest(df)

df.y <- data.frame(df.x)

# inporting double treatment part two
files <- dir(data_dir, pattern="*2017_p7_t*")

data_frame(filename = files) %>% # create a data frame
  # holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_csv(file.path(data_dir, .))) # a new data column
  )  -> df2

df.x2 <- unnest(df2)

df.y2 <- data.frame(df.x2)

# combine
df.control <-rbind(df.y, df.y2)

# # Drop the columns of the dataframe
# df.z <- select(df.y,-c(X1))
# 
# x11()
# ggplot(df.z, aes(x= as.factor(zbin), y= vai)) + 
#   geom_boxplot()
# 
# ####
# 
# df.wide <- spread(df.z, filename, vai)
# 
# names(df.wide)[3] <- "t1"
# names(df.wide)[4] <- "t2"
# names(df.wide)[5] <- "t3"
# names(df.wide)[6] <- "t4"
# 
# 
# # make row mean
# df.wide <- transform(df.wide, vai.mean = rowMeans(df.wide[,c(3:6)], na.rm = TRUE))
# df.wide <- transform(df.wide, vai.sd = rowSds(df.wide[,c(3:6)], na.rm = TRUE))
# 
# 
# ##### workking with long data
# df.z.vai <- stats::aggregate(vai ~ zbin, data = df.y, FUN = mean)
# df.sd <- stats::aggregate(vai ~ zbin, data = df.y, FUN = sd)
# 
# df.z.vai$sd.vai <- df.sd[,2]
# #df.z$ratio.vai <- df.z$vai / transect.length

# Shape factor for spline
sf <- sum(df.z$ratio.vai > 0) - 1
sf <- 15

pavd <- ggplot2::ggplot(df.double, ggplot2::aes(y = vai, x = zbin))+
  #geom_bar(stat = "identity", color = "light grey")+
  # geom_ribbon(aes(ymin = df.z.vai$vai - df.z.vai$sd.vai, ymax = df.z.vai$vai + df.z.vai$sd.vai),
  #             alpha=0.2) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, formula = y ~ splines::ns(x, 10), aes(colour="Double"))+
  ggplot2::geom_smooth(data = df.control, method = "lm", se = TRUE, formula = y ~ splines::ns(x, 10), aes(colour="Control"))+
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
                 axis.ticks.x = element_blank(),
                 legend.title=element_blank())+
  ggplot2::coord_flip(xlim = c(0,30), ylim = c(0, 1), expand = TRUE)+
  scale_colour_manual(name="legend", values=c("blue", "red"))+
  #ggplot2::ylab("Plant Area Volume Density (PAVD)")+
  ylab(vai.label)+
  ggplot2::xlab("Height Above Ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

x11(width = 5, height = 8)
pavd
