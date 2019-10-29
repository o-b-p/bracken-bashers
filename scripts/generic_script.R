library(tidyverse)
library(gridExtra)
# Import data ----
proj10_spectra <- read.csv("data/project_10_spectra.csv")


# Remove first row ----
proj10_spectra <- proj10_spectra[-1,]


# Data wrangling ----
proj10_long <- proj10_spectra %>%
  gather(sample, reflectance, 2:133, na.rm = F) %>%
  dplyr::mutate(wavelength = as.numeric(X), 
                reflectance = as.numeric(reflectance),
                sample = as.factor(sample)) %>%
  dplyr::select(-c("X"))

# Select red-edge only ----

rededge <- proj10_long[ which(proj10_long$wavelength > 679 
                              & proj10_long$wavelength < 781), ]



# Create species-specific rededge dataframes ----

# Non-broadleaf
re_sitka <- rededge[grep("Sika", rededge$sample), ]
re_pasture <- rededge[grep("pasture", rededge$sample), ]
re_grass <- rededge[grep("grass", rededge$sample), ]
re_rho <- rededge[grep("Rho", rededge$sample), ]
re_moss <- rededge[grep("moss", rededge$sample), ]
re_br <- rededge[grep("Br", rededge$sample), ]

# Broadleaf
re_alder <- rededge[grep("Alder", rededge$sample), ]
re_ash <- rededge[grep("Ash", rededge$sample), ]
re_syc <- rededge[grep("Sycamore", rededge$sample), ]
re_lime <- rededge[grep("Lime", rededge$sample), ]
re_hc <- rededge[grep("Horse.chestnut", rededge$sample), ]
re_haz <- rededge[grep("Hazel", rededge$sample), ]
re_beech <- rededge[grep("Beech", rededge$sample), ]
re_rowan <- rededge[grep("Rowan", rededge$sample), ]
re_birch <- rededge[grep("Birch", rededge$sample), ]

# Template: re_ <- rededge[grep("", rededge$sample), ] 



# Build linear models----

# Template: lm_ <- lm(reflectance ~ wavelength, data = re_)
# lm(response ~ predictor). So reflectance is response, wavelength is predictor.
lm_sitka <- lm(reflectance ~ wavelength, data = re_sitka)
lm_pasture <- lm(reflectance ~ wavelength, data = re_pasture)
lm_grass <- lm(reflectance ~ wavelength, data = re_grass)
lm_rho <- lm(reflectance ~ wavelength, data = re_rho)
lm_br <- lm(reflectance ~ wavelength, data = re_br)
lm_moss <- lm(reflectance ~ wavelength, data = re_moss)



# Print linear models
print(lm_sitka)
print(lm_pasture)
print(lm_grass)
print(lm_rho)
print(lm_br)
print(lm_moss)


# Check for statistical significance ----

summary(lm_sitka)
summary(lm_pasture)
summary(lm_grass)
summary(lm_rho)
summary(lm_br)
summary(lm_moss)

# Make plots ----

# Sitka spruce
p_sitka <- ggplot(re_sitka, aes(x = wavelength, y = reflectance, group = 1)) +
   geom_point(colour="red", size = 0.5) +
   geom_smooth(method=lm) +
   theme_bw() +
   ggtitle("Sitka spruce\n") +
   theme(plot.title = element_text(face = "bold")) +
   xlab("\nWavelength (nm)") + ylab("Reflectance\n")

# Pasture grass
p_pasture <- ggplot(re_pasture, aes(x = wavelength, y = reflectance, group = 1)) +
    geom_point(colour="red", size = 0.5) +
    geom_smooth(method=lm) +
    theme_bw() +
    ggtitle("Pasture grass\n") +
    theme(plot.title = element_text(face = "bold")) +
    xlab("\nWavelength (nm)") + ylab("Reflectance\n")

# Grassland
p_grass <- ggplot(re_grass, aes(x = wavelength, y = reflectance, group = 1)) +
    geom_point(colour="red", size = 0.5) +
    geom_smooth(method=lm) +
    theme_bw() +
    ggtitle("Grassland\n") +
    theme(plot.title = element_text(face = "bold")) +
    xlab("\nWavelength (nm)") + ylab("Reflectance\n")

# Rhodedendron
p_rho <- ggplot(re_rho, aes(x = wavelength, y = reflectance, group = 1)) +
    geom_point(colour="red", size = 0.5) +
    geom_smooth(method=lm) +
    theme_bw() +
    ggtitle("Rhodedendron\n") +
    theme(plot.title = element_text(face = "bold")) +
    xlab("\nWavelength (nm)") + ylab("Reflectance\n")

# Bracken
p_br <- ggplot(re_br, aes(x = wavelength, y = reflectance, group = 1)) +
    geom_point(colour="red", size = 0.5) +
    geom_smooth(method=lm) +
    theme_bw() +
    ggtitle("Bracken\n") +
    theme(plot.title = element_text(face = "bold")) +
    xlab("\nWavelength (nm)") + ylab("Reflectance\n")

# Moss
p_moss <- ggplot(re_moss, aes(x = wavelength, y = reflectance, group = 1)) +
   geom_point(colour="red", size = 0.5) +
   geom_smooth(method=lm) +
   theme_bw() +
   ggtitle("Moss\n") +
   theme(plot.title = element_text(face = "bold")) +
   xlab("\nWavelength (nm)") + ylab("Reflectance\n")



# Combine plots into one graphic ----

re_combined <- grid.arrange(p_sitka, p_grass, p_pasture, p_rho, p_br, p_moss, nrow = 2)
