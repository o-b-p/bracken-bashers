library(tidyverse)
library(gridExtra)
# Import data ----
proj10_spectra <- read.csv("data/project_10_spectra.csv")

# Remove first row ----
proj10_spectra <- proj10_spectra[-1,]
proj10_spectra <- select(proj10_spectra, -c(2, 3))

# Data wrangling ----
proj10_long <- proj10_spectra %>%
  gather(sample, reflectance, c(2:131), na.rm = F) %>%
  mutate(reflectance = as.numeric(reflectance),  # change the type of data
         sample = as.factor(sample),
         wavelength = as.numeric(as.character(X))) %>%
  select(-X)
write.csv(proj10_long, file = "data/Long_format.csv")

rededge_3 <- proj10_long %>% 
  filter(wavelength == 783)

rededge_4 <- proj10_long %>%
  filter(wavelength == 705)

index <- right_join(rededge_3, rededge_4, by = "sample")

index <- index %>%
  mutate(ratio = reflectance.x / reflectance.y) %>%
  mutate(veg_index = ratio-1)

write.csv(index, file = "data/index.csv")
  