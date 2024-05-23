library(tidyverse)
library(sf)
library(here)
library(readxl) 

## import shape files 
water <- st_read(here('UGA_water Bodies/UGA_water_areas_dcw.shp'))
districts <- st_read(here('UBOS_Districts 146_2021/uganda_districts.shp'))
regions <- st_read(here("UDHS Regions 2019/UDHS Regions 2019.shp"))


# Moses Map  _ VLC


VL_data <- read_excel("VL_data.xlsx")
VL_data$District <- toupper(VL_data$District)

moses_data <- merge(x = districts, y = VL_data, 
                   by.x = "District", by.y = "District")

moses_data$VLC <- round(moses_data$VLC * 100,0)

moses_data$bins <- cut(moses_data$VLC ,
                     breaks = c(0,90,94.4, 120),
                     labels = c("( < 90%)", "( 90 - 95)", "(> 95 %)"))


moses_map_vlc <- ggplot() +
  geom_sf(data = moses_data, aes(fill = bins), colour = "gray70") +
  geom_sf(data = water, fill = "lightblue") +
  scale_fill_manual(values = c("darkred", "yellow", "darkgreen", "purple"),
                    labels = c("( < 90%)", "( 90 - 95)", "(> 95 %)")) +
  theme_void()+
  labs(fill = 'Viral Load Coverage(%)', caption = 'Data source : DHIS2')+
  theme(legend.position = 'bottom')

ggsave(filename = 'moses_map_vlc.png',
       plot = moses_map_vlc,
       width = 9 , height =  6, dpi = 600)


## -- VLS 

# Moses Map  _ VLC

moses_data$VLS <- round(moses_data$VLS * 100,0)

moses_data$bins <- cut(moses_data$VLC ,
                       breaks = c(0,90,94.4, 120),
                       labels = c("( < 90%)", "( 90 - 95)", "(> 95 %)"))

moses_map_vls <- ggplot() +
  geom_sf(data = moses_data, aes(fill = bins), colour = "gray70") +
  geom_sf(data = water, fill = "lightblue") +
  scale_fill_manual(values = c("darkred", "yellow", "darkgreen", "purple"),
                    labels = c("( < 90%)", "( 90 - 95)", "(> 95 %)")) +
  theme_void()+
  labs(fill = 'Viral Load Supression (%)', caption = 'Data source : DHIS2')+
  theme(legend.position = 'bottom')

ggsave(filename = 'moses_map_vls.png',
       plot = moses_map_vlc,
       width = 9 , height =  6, dpi = 600)


#---- community data 

community_data <- read_excel('community reporting.xlsx')

community_data$organisationunitname <- gsub("MADI-OKOLLO", "MADI OKOLLO", community_data$organisationunitname)
community_data$organisationunitname <- gsub("NAMUTUMBA", "NAMUTUNMBA", community_data$organisationunitname)
community_data$organisationunitname <- gsub("SEMBABULE", "SSEMBABULE", community_data$organisationunitname)
community_data$organisationunitname <- gsub("KASSANDA", "KASSNDA", community_data$organisationunitname)

## merge the data 

plot_data <- merge(x = districts, y = community_data, 
                   by.x = "District", by.y = "organisationunitname")

plot_data <- plot_data %>%
  rename(
    rate = `HMIS 097b - VHT/ICCM Quarterly Report - Reporting rate 1. National`
  ) %>%
  mutate(
    rate = as.integer(rate)
  )

plot_data$bins <- cut(plot_data$rate ,
                       breaks = c(0,50,80, 100),
                       labels = c("( < 50% )", "( 50  - 80%)", "(> 80 %)"))

plot_data$periodname <- factor(plot_data$periodname,levels = c('Apr to Jun 2023','Jul to Sep 2023','Oct to Dec 2023','Jan to Mar 2024'))


community_map <- ggplot() +
  geom_sf(data = plot_data, aes(fill = bins), colour = "gray70") +
  geom_sf(data = water, fill = "lightblue") +
  scale_fill_manual(values = c("darkred", "yellow", "darkgreen", "purple"),
                    labels = c("( < 50% )", "( 50  - 80%)", "(> 80 %)")) +
  facet_wrap(~periodname, nrow = 1)+
  theme_void()+
  labs(fill = 'Reporting Rates', caption = 'Data source : DHIS2')+
  theme(legend.position = 'bottom')

ggsave(filename = 'community_reporting.png',
       plot = community_map,
       width = 9 , height =  6, dpi = 600)


