# This script deals with the project 10 data samples that we collected in the field then scanned their spectral reflectances across the wavelengths 

library(tidyverse)
library(gridExtra)
# Import data ----
proj10_spectra <- read.csv("~bracken-bashers/data/project_10_spectra.csv")

# Remove first row ----
proj10_spectra <- proj10_spectra[-1,]



# Data wrangling ---------------------------------------------------------------
proj10_long <- proj10_spectra %>%
  gather(sample, reflectance, 2:133, na.rm = F) %>%
  dplyr::mutate(wavelength = as.numeric(X), 
                reflectance = as.numeric(reflectance),
                sample = as.factor(sample)) %>%
  dplyr::select(-c("X"))



# Create species-specific dataframes -------------------------------------------

# Non-broadleaf
sitka <- proj10_long[grep("Sika", proj10_long$sample), ] %>% 
  group_by(wavelength) %>%
  dplyr::summarise(sitka_mean_reflectance = mean(reflectance)) %>%
  ungroup() 
  
pasture <- proj10_long[grep("pasture", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(pasture_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

grass <- proj10_long[grep("grass", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(grass_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

rho <- proj10_long[grep("Rho", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(rho_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

moss <- proj10_long[grep("moss", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(moss_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

br <- proj10_long[grep("Br", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(br_mean_reflectance = mean(reflectance)) %>%
  ungroup() 



# Broadleaf 
alder <- proj10_long[grep("Alder", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(alder_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

ash <- proj10_long[grep("Ash", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(ash_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

syc <- proj10_long[grep("Sycamore", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(syc_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

lime <- proj10_long[grep("Lime", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(lime_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

hc <- proj10_long[grep("Horse.chestnut", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(hc_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

haz <- proj10_long[grep("Hazel", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(haz_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

beech <- proj10_long[grep("Beech", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(beech_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

rowan <- proj10_long[grep("Rowan", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(rowan_mean_reflectance = mean(reflectance)) %>%
  ungroup() 

birch <- proj10_long[grep("Birch", proj10_long$sample), ] %>%
  group_by(wavelength) %>%
  dplyr::summarise(birch_mean_reflectance = mean(reflectance)) %>%
  ungroup() 


# Template: re_ <- rededge[grep("", rededge$sample), ] 

# Save average dataframes as .csv files ----------------------------------------

write.csv(sitka, file = "~/bracken-bashers/other_outputs/sitka.csv")
write.csv(pasture, file = "~/bracken-bashers/other_outputs/pasture.csv")
write.csv(grass, file = "~/bracken-bashers/other_outputs/grass.csv")
write.csv(rho, file = "~/bracken-bashers/other_outputs/rho.csv")
write.csv(br, file = "~/bracken-bashers/other_outputs/br.csv")
write.csv(moss, file = "~/bracken-bashers/other_outputs/.csv")

write.csv(alder, file = "~/bracken-bashers/other_outputs/alder.csv")
write.csv(ash, file = "~/bracken-bashers/other_outputs/ash.csv")
write.csv(beech, file = "~/bracken-bashers/other_outputs/beech.csv")
write.csv(birch, file = "~/bracken-bashers/other_outputs/birch.csv")
write.csv(hc, file = "~/bracken-bashers/other_outputs/hc.csv")
write.csv(rowan, file = "~/bracken-bashers/other_outputs/rowan.csv")
write.csv(haz, file = "~/bracken-bashers/other_outputs/haz.csv")
write.csv(lime, file = "~/bracken-bashers/other_outputs/lime.csv")
write.csv(syc, file = "~/bracken-bashers/other_outputs/syc.csv")
# Template: write.csv(, file = "~/bracken-bashers/other_outputs/.csv")



# Join all averages into one dataframe

merged <- merge(sitka, pasture, by = "wavelength") %>%
  merge(moss, by = "wavelength") %>%
  merge(rho, by = "wavelength") %>%
  merge(grass, by = "wavelength") %>%
  merge(br, by = "wavelength")

decid <- merge(ash, alder, by = "wavelength") %>%
  merge(birch, by = "wavelength") %>%
  merge(beech, by = "wavelength") %>%
  merge(haz, by = "wavelength") %>%
  merge(hc, by = "wavelength") %>%
  merge(lime, by = "wavelength") %>%
  merge(rowan, by = "wavelength") %>%
  merge(syc, by = "wavelength")

merged <- merge(merged, decid, by = "wavelength")

# Find mean decidious
mean_decid <- decid %>%
  gather(species, mean_reflectance, 2:10) %>%
  group_by(wavelength) %>%
  dplyr::summarise(mean_deciduous_reflectance = mean(mean_reflectance)) %>%
  ungroup()

# Add mean deciduous to master df
merged <- merge(mean_decid, merged, by = "wavelength")

# Save master df
write.csv(merged, file = "~/bracken-bashers/other_outputs/spectral_library.csv")















# Make plots 

# Sitka spruce
(p_sitka <- ggplot(sitka, aes(x = wavelength, y = sitka_mean_reflectance, group = 1)) +
  geom_point(colour="red", size = 0.5) +
  #geom_smooth(method=lm) +
  theme_bw() +
  ggtitle("Sitka spruce\n") +
  theme(plot.title = element_text(face = "bold")) +
  xlab("\nWavelength (nm)") + ylab("Reflectance\n"))

# Pasture grass
(p_pasture <- ggplot(pasture, aes(x = wavelength, y = pasture_mean_reflectance, group = 1)) +
  geom_point(colour="red", size = 0.5) +
  #geom_smooth(method=lm) +
  theme_bw() +
  ggtitle("Pasture grass\n") +
  theme(plot.title = element_text(face = "bold")) +
  xlab("\nWavelength (nm)") + ylab("Reflectance\n"))

# Grassland
p_grass <- ggplot(grass, aes(x = wavelength, y = reflectance, group = 1)) +
  geom_point(colour="red", size = 0.5) +
  geom_smooth(method=lm) +
  theme_bw() +
  ggtitle("Grassland\n") +
  theme(plot.title = element_text(face = "bold")) +
  xlab("\nWavelength (nm)") + ylab("Reflectance\n")

# Rhodedendron
p_rho <- ggplot(rho, aes(x = wavelength, y = reflectance, group = 1)) +
  geom_point(colour="red", size = 0.5) +
  geom_smooth(method=lm) +
  theme_bw() +
  ggtitle("Rhodedendron\n") +
  theme(plot.title = element_text(face = "bold")) +
  xlab("\nWavelength (nm)") + ylab("Reflectance\n")

# Bracken
p_br <- ggplot(br, aes(x = wavelength, y = reflectance, group = 1)) +
  geom_point(colour="red", size = 0.5) +
  geom_smooth(method=lm) +
  theme_bw() +
  ggtitle("Bracken\n") +
  theme(plot.title = element_text(face = "bold")) +
  xlab("\nWavelength (nm)") + ylab("Reflectance\n")

# Moss
p_moss <- ggplot(moss, aes(x = wavelength, y = reflectance, group = 1)) +
  geom_point(colour="red", size = 0.5) +
  geom_smooth(method=lm) +
  theme_bw() +
  ggtitle("Moss\n") +
  theme(plot.title = element_text(face = "bold")) +
  xlab("\nWavelength (nm)") + ylab("Reflectance\n")



# Combine plots into one graphic ----

re_combined <- grid.arrange(p_sitka, p_grass, p_pasture, p_rho, p_br, p_moss, nrow = 2)
