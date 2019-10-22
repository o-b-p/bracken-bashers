

# Load libraries ----
library(tidyverse)
library(ggspectra)
library(dplyr)

# Import data ----
class_spectra <- read.csv("~/bracken-bashers/data/class_spectra.csv")

# Subset data ----

grass <- class_spectra %>%
  dplyr::select(Wavelength..nm., Grass) %>%
  # Select only Grass and wavelength
  dplyr::transmute(wavelength = Wavelength..nm., reflectance = Grass)
  # Rename variables



# Importing our own data -------------------------------------------------------

proj10_spectra <- read.csv("~/bracken-bashers/data/project_10_spectra.csv")

# Remove first row ----
proj10_spectra <- proj10_spectra[-1,]

# Convert data to long format ----
proj10_long <- proj10_spectra %>%
  gather(sample, reflectance, 2:133, na.rm = F)

# Subset only grass observations by string-matching ----
proj_10_grass <- proj10_long[grep("grass", proj10_long$sample), ]

# Convert character to factor (for column "sample")
proj_10_grass$sample <- as.factor(proj_10_grass$sample)

# Convert reflectance & wavelength from character to numeric ----
proj_10_grass$reflectance <- as.numeric(proj_10_grass$reflectance)
proj_10_grass$wavelength <- as.numeric(proj_10_grass$wavelength)


# Rename variables
proj_10_grass <- proj_10_grass %>%
  dplyr::mutate(wavelength = X) %>%
  dplyr::select(wavelength, reflectance)


# Plots ----

# Grass plot (project 10)

(ggplot(proj_10_grass, aes(x = wavelength, y = reflectance, group = 1)) +
   #geom_line() +
   geom_point(colour="blue", size = 0.5) +
   #geom_smooth(method=lm) +
   xlab("\nWavelength (nm)") + ylab("Reflectance"))

# Grass plot (class)

(ggplot(grass, aes(x = wavelength, y = reflectance, group = 1)) +
    #geom_line() +
    geom_point(colour="blue", size = 0.5) +
    #geom_smooth(method=lm) +
    xlab("\nWavelength (nm)") + ylab("Reflectance"))

# Select red-edge only ----

# Project 10 grass data
proj10_grass_red <- proj_10_grass[ which(proj_10_grass$wavelength > 679 
                              & proj_10_grass$wavelength < 781), ]

# Class grass data
class_grass_red <- grass[ which(grass$wavelength > 679 
                                & grass$wavelength < 781), ]

# Plot grass red-edge with linear regression

(ggplot(proj10_grass_red, aes(x = wavelength, y = reflectance, group = 1)) +
   geom_point(colour="red", size = 0.5) +
   geom_smooth(method=lm) +
   theme_bw() +
   ggtitle("Red-edge reflectance of grass samples (680-780 nm)\n") +
   theme(plot.title = element_text(face = "bold")) +
   xlab("\nWavelength (nm)") + ylab("Reflectance\n"))

(ggplot(class_grass_red, aes(x = wavelength, y = reflectance, group = 1)) +
    geom_point(colour="red", size = 0.5) +
    geom_smooth(method=lm) +
    theme_bw() +
    ggtitle("Red-edge reflectance of grass samples (680-780 nm)\n") +
    theme(plot.title = element_text(face = "bold")) +
    xlab("\nWavelength (nm)") + ylab("Reflectance\n"))



# Build linear model

grass_lm <- lm(reflectance ~ wavelength, data = proj10_grass_red)
# lm(response ~ predictor). So reflectance is response, wavelength is predictor.
print(grass_lm)

summary(grass_lm)
