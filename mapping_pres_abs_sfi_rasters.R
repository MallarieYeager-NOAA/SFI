library(terra)
library(sf)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(akgfmaps)
library(purrr)
library(tidyterra)
library(patchwork)
library(tmaptools)
library(magick)
library(viridis)

goa <- akgfmaps::get_base_layers(select.region = "goa",
                                 set.crs = "EPSG:4326")
ebs <- akgfmaps::get_base_layers(select.region = "ebs",
                                 set.crs = "EPSG:4326")
ext(ebs$akland)
ext(goa$akland)
lonlat <- cbind(c(-179.9995,-135), c(48.3076782, 62.2653277))

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

#EEZ shapefile to crop by
EEZ <- read_sf("/Users/mallarie.yeager/Documents/R_projects/IBM_data/ModelRunsFinal/GOA EEZ to Clip Original ROMS Extent Shapefiles/NMFS_Areas_GOA_All_Dissolve.shp")

Pres_demospongia <- rast("2025 model taxa - 1 km trimmed/2025 model taxa - 1 km trimmed/pa.Demospongiae.tif")
Pres_demospongia <- project(Pres_demospongia, "EPSG:4326")
ggplot()+  
  geom_spatraster(data = Pres_demospongia, aes(fill = pa.Demospongiae))+
  scale_fill_viridis( option = "D", discrete = F, limits = c(0,1), na.value = t_col("white",100)) +
  geom_sf(data = goa$akland, fill = "grey60", color = "grey40") +
  geom_sf(data = goa$bathymetry, color = "grey75", lwd = 0.1)+
  ggtitle("Pres_abs model Demospongia")+  
  scale_x_continuous(limits = lonlat[,1])+
  scale_y_continuous(limits = lonlat[,2]) +
  theme_bw()
ggsave("Presence_Absence_demospongia.png", dpi = 500)

Pres_Hexactinellida <- rast("2025 model taxa - 1 km trimmed/2025 model taxa - 1 km trimmed/pa.Hexactinellida.tif")
Pres_Hexactinellida <- project(Pres_Hexactinellida, "EPSG:4326")
ggplot()+  
  geom_spatraster(data = Pres_Hexactinellida, aes(fill = pa.Hexactinellida))+
  scale_fill_viridis( option = "D", discrete = F, limits = c(0,1), na.value = t_col("white",100)) +
  geom_sf(data = goa$akland, fill = "grey60", color = "grey40") +
  geom_sf(data = goa$bathymetry, color = "grey75", lwd = 0.1)+
  ggtitle("Pres_abs model Hexactinellida")+
  scale_x_continuous(limits = lonlat[,1])+
  scale_y_continuous(limits = lonlat[,2]) +
  theme_bw()
ggsave("Presence_Absence_hexactinellida.png", dpi = 500)

Pres_Pennatuloidea <- rast("2025 model taxa - 1 km trimmed/2025 model taxa - 1 km trimmed/pa.Pennatuloidea.tif")
Pres_Pennatuloidea <- project(Pres_Pennatuloidea, "EPSG:4326")
ggplot()+  
  geom_spatraster(data = Pres_Pennatuloidea, aes(fill = pa.Pennatuloidea))+
  scale_fill_viridis( option = "D", discrete = F, limits = c(0,1), na.value = t_col("white",100)) +
  geom_sf(data = goa$akland, fill = "grey60", color = "grey40") +
  geom_sf(data = goa$bathymetry, color = "grey75", lwd = 0.1)+
  ggtitle("Pres_abs model Pennatuloidea")+
  scale_x_continuous(limits = lonlat[,1])+
  scale_y_continuous(limits = lonlat[,2]) +
  theme_bw()
ggsave("Presence_Absence_pennatuloidea.png", dpi = 500)

Pres_Primnoidae <- rast("2025 model taxa - 1 km trimmed/2025 model taxa - 1 km trimmed/pa.Primnoidae.tif")
Pres_Primnoidae <- project(Pres_Primnoidae, "EPSG:4326")
ggplot()+  
  geom_spatraster(data = Pres_Primnoidae, aes(fill = pa.Primnoidae))+
  scale_fill_viridis( option = "D", discrete = F, limits = c(0,1), na.value = t_col("white",100)) +
  geom_sf(data = goa$akland, fill = "grey60", color = "grey40") +
  geom_sf(data = goa$bathymetry, color = "grey75", lwd = 0.1)+
  ggtitle("Pres_abs model Primnoidae")+
  scale_x_continuous(limits = lonlat[,1])+
  scale_y_continuous(limits = lonlat[,2]) +
  theme_bw()
ggsave("Presence_Absence_primnoidae.png", dpi = 500)


Pres_Ptilosarcus <- rast("2025 model taxa - 1 km trimmed/2025 model taxa - 1 km trimmed/pa.Ptilosarcus.tif")
Pres_Ptilosarcus <- project(Pres_Ptilosarcus, "EPSG:4326")
ggplot()+  
  geom_spatraster(data = Pres_Ptilosarcus, aes(fill = pa.Ptilosarcus))+
  scale_fill_viridis( option = "D", discrete = F, limits = c(0,1), na.value = t_col("white",100)) +
  geom_sf(data = goa$akland, fill = "grey60", color = "grey40") +
  geom_sf(data = goa$bathymetry, color = "grey75", lwd = 0.1)+
  ggtitle("Pres_abs model Ptilosarcus")+
  scale_x_continuous(limits = lonlat[,1])+
  scale_y_continuous(limits = lonlat[,2]) +
  theme_bw()
ggsave("Presence_Absence_ptilosarcus.png", dpi = 500)

Pres_Stylasteridae <- rast("2025 model taxa - 1 km trimmed/2025 model taxa - 1 km trimmed/pa.Stylasteridae.tif")
Pres_Stylasteridae <- project(Pres_Stylasteridae, "EPSG:4326")
ggplot()+  
  geom_spatraster(data = Pres_Stylasteridae, aes(fill = pa.Stylasteridae))+
  scale_fill_viridis( option = "D", discrete = F, limits = c(0,1), na.value = t_col("white",100)) +
  geom_sf(data = goa$akland, fill = "grey60", color = "grey40") +
  geom_sf(data = goa$bathymetry, color = "grey75", lwd = 0.1)+
  ggtitle("Pres_abs model Stylasteridae")+
  scale_x_continuous(limits = lonlat[,1])+
  scale_y_continuous(limits = lonlat[,2]) +
  theme_bw()
ggsave("Presence_Absence_stylasteridae.png", dpi = 500)















