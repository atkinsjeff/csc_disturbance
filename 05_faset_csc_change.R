#### FASET
# set your output directory
data_path<- "./output/faset_output/faset_summary_matrix/2017"

#
library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)



# files <- list.files(path = data_path, full.names = TRUE) 
# 
# df <- files %>%
#   map(~ read_csv(file.path(data_path, .))) %>% 
#   reduce(rbind)
# 
# 
# 
# df <- list.files(path = data_path, full.names = TRUE) %>%
#   lapply(read_csv) %>%
#   bind_rows
# 
# df <- data.frame(df)
# 
# write.csv(df, "faset_2017.CSV")

# Now let's try and make a polar map
df <- read.csv("faset_2017.CSV")

#make size of hole
df$xbin <- df$xbin + 5
df$sum.vai[df$sum.vai == 0] <- NA

# Labels and breaks need to be added with scale_y_discrete.

ggplot2::ggplot(df, ggplot2::aes(x = theta, y = xbin))+
  ggplot2::geom_tile(ggplot2::aes(fill = sum.vai))+
  ggplot2::scale_fill_gradient(low="gray88", high="dark green",
                               na.value = "white",
                               limits=c(0, 8))+

  ggplot2::coord_polar(theta = "x")+
  #scale_y_continuous(breaks = seq(0, 20, 5))+
  # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 14),
                 axis.title.x = ggplot2::element_text(size = 20),
                 axis.title.y = ggplot2::element_text(size = 20))
  
  +
  ggplot2::xlim(0,40)+
  ggplot2::ylim(0,41)+
  ggplot2::xlab("Distance along transect (m)")+
  ggplot2::ylab("Height above ground (m)")+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))
  


#### Looking at the outer plots

df <- read.csv("faset_output_file_outer.CSV")

df %>%  select(transect, year, rugosity)%>%
  spread( key = year,
         value = rugosity) -> df.rugosity
         
# rename them
df.rugosity <- rename(df.rugosity, c( "2012" = "y12", "2017" = "y17"))
         
ggplot(df.rugosity, aes(x = y12, y = y17))+
  geom_point(size = 4)+
  geom_abline(intercept = 0, slope = 1)+
  ylab("Rugosity (m) (2017)")+
  xlab("Rugosity (m) (2012)")+
  theme_classic()+
  xlim(0,20)+
  ylim(0,20)
  #####
  # Two correlation plots

df %>% filter(year == "2012") -> df.12
df %>% filter(year == "2017") -> df.17

require(corrplot)

m <- df.12

m <- subset(m, select = -c(plot, plot_1, transect.length, year, transect, deep.gaps, deep.gap.fraction))
m.12 <- cor(m)
corrplot(m.12, type = "lower")

##### Making summary stats
df$year <- as.factor(df$year)
df %>% group_by(year) %>%
  summarise_all(mean) -> df.means
  df.means <- data.frame(df.means)
  
# Let's look at differences  
qqnorm(df$rugosity)
shapiro.test(df$rugosity)
hist(df$rugosity)

summary(aov(rugosity ~ year, data = df))