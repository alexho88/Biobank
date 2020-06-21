library(tidyverse)
dataset <- read.csv("//ad.ucl.ac.uk/homex/rmheoxx/Documents/Biobank/ASIforR.csv")
options(max.print=1000000)

res <- dataset %>%
  group_by(gi_cancer_type) %>% 
  summarise (median = median(MAP))

res <- dataset %>%
  filter (!is.na(MAP)) %>% 
  group_by(gi_cancer_type) %>% 
  summarise (mean = mean(MAP))


res2 <- dataset %>%
  group_by(gi_cancer_type) %>% 
  tally()


View (res2)

summary

res3 <- aov(MAP ~ gi_cancer_type, data = dataset)
summary (res3)
TukeyHSD (res3)
res4 <- TukeyHSD (res3)
View (res4)
