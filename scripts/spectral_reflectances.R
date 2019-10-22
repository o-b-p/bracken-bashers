# spectral_reflectances.R
# 22 October 2019
# Erika Hodgson

# The purpose of this script is to compare the spectral reflectance of bracken
# with other plant species. We need to ensure that bracken has a significantly
# distinguishable spectral signature in order to map it around Loch Tay.

# Load libraries ----

library(tidyverse)

# Import data ----

proj10_spectra <- read.csv("~/bracken-bashers/data/project_10_spectra.csv")

# Remove first row ----

proj10_spectra <- proj10_spectra[-1,]

# Convert data to long format ----

proj10_long <- proj10_spectra %>%
  gather(sample, reflectance, 2:133, na.rm = F)

# Rename column ----

proj10_long$wavelength <- proj10_long$X
