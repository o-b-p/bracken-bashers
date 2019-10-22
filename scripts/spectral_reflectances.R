# spectral_reflectances.R
# 22 October 2019
# Erika Hodgson

# The purpose of this script is to compare the spectral reflectance of bracken
# with other plant species. We need to ensure that bracken has a significantly
# distinguishable spectral signature in order to map it around Loch Tay.

# Load libraries ----

library(tidyverse)

# Import data ----

project10_spectra <- read.csv("~/bracken-bashers/data/project_10_spectra.csv")

