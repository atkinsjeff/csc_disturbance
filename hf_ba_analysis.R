# making a harvard forest hemlock mortality assessment
require(plyr)
require(dplyr)
require(tidyverse)

hf.ba <- read.csv("./harvard_supplemental/MegaplotMortalityAssessment_2010_2016_subset.csv")

#make basal area
hf.ba$ba <- ((hf.ba$dbh/2)^2 * pi)

#make quad a factor
hf.ba$quadrat <- as.factor(hf.ba$quadrat)

#now group by quadrat
hf.ba %>%
  group_by(quadrat) %>%
  summarize(ba.sum = sum(ba)) -> hf.summary 

hf.ba %>%
  filter(status.2016 == "D" & sp == "tsugca") %>%
  group_by(quadrat) %>%
  summarize(dead.hem.ba.sum = sum(ba)) -> hf.dead.hem.ba

hf.mortality <- merge(hf.summary, hf.dead.hem.ba)

hf.mortality$dead.per <- hf.mortality$dead.hem.ba.sum / hf.mortality$ba.sum
hist(hf.mortality$dead.per)

write.csv(hf.mortality, "hf_mortality.csv")


#### logistic