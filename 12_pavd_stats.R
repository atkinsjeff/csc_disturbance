# vai profile script
#Working with WV drought data
# jeff atkins (jwatkins6@vcu.edu)

require(plyr)
require(dplyr)
require(ggplot2)
require(tidyverse)
require(ggridges)
library(PerformanceAnalytics)
library(segmented)

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
files <- dir(data_dir, pattern="*ISEp10t*")
plot.number <- 10
treatment <- "moderate"
filename <-"plot_ten_2015.csv"
year = 2015

data_frame(filename = files) %>% # create a data frame
  # holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_csv(file.path(data_dir, .))) # a new data column
  )  -> df

df.x <- unnest(df)

m <- data.frame(df.x)


# A new data fram of VAI distributed at each height level
df.z <- stats::aggregate(vai ~ zbin, data = m, FUN = mean)



# A check --- this number should be reasonable for each, like 5 to 8
total.vai <- sum(df.z$vai) 


### add cumulative column to the data frame
df.z[,"cum_vai"] <- cumsum(df.z$vai)


# the eruption durations
message("Maple Control")
per10 <- quantile(df.z$cum_vai, c(.10)) 
per25 <- quantile(df.z$cum_vai, c(.25))
per50 <- quantile(df.z$cum_vai, c(.50))
per75 <- quantile(df.z$cum_vai, c(.75))
per90 <- quantile(df.z$cum_vai, c(.90))

#make a model fit
df.z.fit <- loess(zbin ~ cum_vai, df.z)

#this is the loess prediction that gives the height at which these percentiles occur
p10 <- predict(df.z.fit, per10)
p25 <- predict(df.z.fit, per25)
p50 <- predict(df.z.fit, per50)
p75 <- predict(df.z.fit, per75)
p90 <- predict(df.z.fit, per90)

#distribtuion stats
skew.vai <- skewness(df.z$vai)
kur.vai <- kurtosis(df.z$vai)

#max and mins
max.vai <- df.z$zbin[which.max(df.z$vai)]
min.vai <- df.z$zbin[which.min(df.z$vai)]

# lcreating linear model
model <- lm(zbin ~  cum_vai, data = df.z)

x <- df.z$cum_vai
y <- df.z$zbin
plot(y ~ x)
# have to provide estimates for breakpoints.
# after looking a the data,   #my.lm <- lm(AltitudeMeters ~ DistanceMeters, data = df)
my.seg <- segmented(model, 
                    seg.Z = ~ cum_vai, 
                    psi = 5)

# summary
summary(my.seg)

#creating plots
plot(my.seg, conf.level=0.95, shade=TRUE)


breakpoint <- my.seg$psi[2]
breakpoint.se <- my.seg$psi[3]

#
variable.list <- list(plot = plot.number,
                      year = year,
                      treatment = treatment,
                      total.vai = total.vai,
                      h10 = p10,
                      h25 = p25,
                      h50 = p50,
                      h75 = p75,
                      h90 = p90,
                      skew.vai = skew.vai,
                      kur.vai = kur.vai,
                      max.vai = max.vai,
                      min.vai = min.vai,
                      breakpoint = breakpoint,
                      breakpoint.se = breakpoint.se)


#now to write to csv
variable.list <- data.frame(variable.list)

write.csv(variable.list, filename)



######
df <- read.csv("./summary/ise/ice_storm_summary.csv")

plot(df$breakpoint, df$year)

x11()
ggplot(df, aes(x = treatment, y = breakpoint)) + 
  geom_boxplot()+
  facet_wrap(. ~ year)

x11()
ggplot(df, aes(x = treatment, y = breakpoint, color = year ))+
  geom_point()
