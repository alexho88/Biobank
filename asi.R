library(tidyverse)
library(ggplot2)
options(max.print=1000000)
asi <- read.csv("//ad.ucl.ac.uk/homex/rmheoxx/Documents/Biobank/ASIforR.csv")
View(asi)
asi <- asi %>% 
  select (-(X:X.39))
asi <- asi %>% 
  select (-(X.40:X.59))

cancercount <- asi %>%
  group_by (gi_cancer_type) %>% 
  tally()
asi %>%
  group_by (gi_cancer_type) %>% 
  summarise(ASImean=mean(Mean_ASI, na.rm=TRUE)) -> asi2
asi %>%
  group_by (gi_cancer_type) %>% 
  summarise(PPMean=mean(Pulse_Pressure, na.rm=TRUE)) -> asi3
asi %>% 
  group_by (gi_cancer_type) %>% 
  summarise (ASImedian = median(Mean_ASI, na.rm=TRUE)) -> asi4
asi %>% 
  group_by (gi_cancer_type) %>% 
  summarise (PPmedian = median(Mean_ASI, na.rm=TRUE)) -> asi5

str(asi)

ggplot(asi) + geom_boxplot(mapping=aes(x=gi_cancer_type, y=Mean_ASI)) + coord_cartesian(ylim=c(0,50))
ggplot(asi) + geom_boxplot(mapping=aes(x=gi_cancer_type, y=Pulse_Pressure))


asi.out = aov(Mean_ASI ~ gi_cancer_type, data=asi)
summary(asi.out)
TukeyHSD(asi.out) -> tukeyasi
tukeyasi

pp.out = aov(Pulse_Pressure ~ gi_cancer_type, data=asi)
summary(pp.out)
TukeyHSD(pp.out) ->tukeypp

