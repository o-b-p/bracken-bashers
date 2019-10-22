# spectral_reflectances.R
# 22 October 2019
# Erika Hodgson

# The purpose of this script is to compare the spectral reflectance of bracken
# with other plant species. We need to ensure that bracken has a significantly
# distinguishable spectral signature in order to map it around Loch Tay.

# Load libraries ----
library(tidyverse)
library(ggspectra)

# Import data ----
proj10_spectra <- read.csv("~/bracken-bashers/data/project_10_spectra.csv")
class_spectra <- read.csv("~/bracken-bashers/data/class_spectra.csv")


# Remove first row ----
proj10_spectra <- proj10_spectra[-1,]


# Convert data to long format ----
proj10_long <- proj10_spectra %>%
  gather(sample, reflectance, 2:133, na.rm = F)


# Rename columns ----
proj10_long$wavelength <- proj10_long$X
class_spectra$wavelength <- class_spectra$Wavelength..nm.


# Subset only bracken observations by string-matching ----
bracken <- proj10_long[grep("Br", proj10_long$sample), ]


# Create an ID column for bracken samples ----

# View vector types
str(bracken)
# Convert character to factor (for column "sample")
bracken$sample <- as.factor(bracken$sample)
# Create new column "id", with unique sample number.
bracken$id <- as.numeric(bracken$sample)


# Convert reflectance from character to numeric ----

bracken$reflectance <- as.factor(bracken$reflectance)
bracken$reflectance <- as.numeric(bracken$reflectance)
bracken$wavelength <- as.numeric(bracken$wavelength)

# Group by id

bracken_mean <- bracken %>%
  group_by(wavelength) %>%
  dplyr::summarise(Mean = mean(reflectance)) %>%
  ungroup()

# Plot average bracken spectrogram (hyperspectral)----

(ggplot(bracken_mean, aes(x = wavelength, y = Mean, group = 1)) +
  geom_line() +
   #geom_point() +
    xlab("\nWavelength") + ylab("Mean reflectance (nm)\n"))

(ggplot(bracken, aes(x = wavelength, y = reflectance, group = 1)) +
    #geom_line() +
    geom_point(colour="blue", size = 0.5) +
    #geom_smooth(method=lm) +
    xlab("\nWavelength (nm)") + ylab("Reflectance"))

# Select red-edge only ----


bracken_red <- bracken[ which(bracken$wavelength > 679 
                              & bracken$wavelength < 781), ]

# Plot bracken red-edge with linear regression ----

(ggplot(bracken_red, aes(x = wavelength, y = reflectance, group = 1)) +
    geom_point(colour="red", size = 0.5) +
    geom_smooth(method=lm) +
    theme_bw() +
    xlab("\nWavelength (nm)") + ylab("Reflectance\n"))

# Build linear model for bracken red-edge ----

bracken_lm <- lm(reflectance ~ wavelength, data = bracken)
# lm(response ~ predictor). So reflectance is response, wavelength is predictor.
print(bracken_lm)

# Therefore, our linear model is:
# reflectance = 21233.834 + (-4.797 * wavelength)

# The gradient of the line is -4.797.


# Check for statistical significance ----

summary(bracken_lm)
# p-value < 2.2e-16
# Therefore our model is statistically significant, as it is well below the 
# theshold of p < 0.05.
