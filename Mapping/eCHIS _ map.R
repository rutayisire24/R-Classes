

## Load library and Shape files 
library(sf)
library(here)
library(ggplot2)
library(dplyr)

uganda_sp <- st_read(here("Shape Files/Districts/uganda_districts.shp"))

water_sp <- st_read(here("Shape Files/Water Bodies/UGA_water_areas_dcw.shp"))

highlight_districts <- c("AMURU","LIRA", "KAMULI","BUIKWE",
                         "MUKONO","BUGIRI","NTUNGAMO","KAMWENGE","NEBBI",
                         "MARACHA","KOBOKO","MOYO","OMORO","OYAM", "LAMWO",
                         "MPIGI","BUKOMANSIMBI")

uganda_sp <- uganda_sp %>%
  mutate(highlight = ifelse(District %in% highlight_districts, 
                            "highlight", "normal"))

ggplot()+
  geom_sf(data = uganda_sp, aes(fill = highlight), color = "black", size = 0.5) +
  geom_sf(data = water_sp, fill = "#8ecae6")+
  geom_sf_label(fill = "white",  # override the fill from aes()
                fun.geometry = sf::st_centroid)+
  theme_void()+
  scale_fill_manual(values = c("highlight" = "#ffbe0b", 
                               "normal" = "#edf6f9"), labels = c("eCHIS","Non eCHIS")) +
  labs(title = " ", fill = "")+
  theme(legend.position = "bottom")


