

rm(list = ls())

## load libraries 
pacman::p_load(here,ggplot2,ggthemes,
               patchwork, dplyr,readr,sf)

## Regions 
region_sp <- st_read(here("Regions/UDHS Regions 2019.shp"))

## basic plot 

ggplot()+
  geom_sf(data = region_sp)

## load data 

malaria_data <- readxl::read_xlsx(here("malaria_data.xlsx"))

region_malaria <- malaria_data |> 
  group_by(orgunitlevel2) |> 
  summarise(
    malaria_cases = sum(Malaria_total, na.rm = T)
  )

plot_data <- region_sp |> 
  rename(
    orgunitlevel2 = Name
  ) |> 
  left_join(
    region_malaria
  )

## plot regional data 

basic_map <- ggplot()+
  geom_sf(data = plot_data, 
          aes(fill = malaria_cases))+
  scale_fill_fermenter(
    palette = "YlOrRd", 
    direction = 2, 
    breaks = c(0,400000,100000)
  )+
  labs(
    title = "Malaria Cases seen at OPD", 
    caption = "Source: DHIS2"
  )+
  theme_map()+
  theme(legend.position = "top")



## water 
water_sp <- st_read(here("Water Bodies/UGA_water_areas_dcw.shp"))

## Add water 

basic_map +
  geom_sf(data = water_sp, fill = "#00bbf9")



## add water and borders 
water_sp <- st_read(here("Water Bodies/UGA_water_areas_dcw.shp"))
uganda_sp <- st_read(here("Uganda/ug.shp"))


## Sub county Data 
sub_counties <- read_sf("sub_counties.geojson") |> 
  rename(sub_county = name) |> 
  select(c(sub_county,geometry))

## Data on Malaria 
data_malaria <- readxl::read_excel(here("malaria_data.xlsx")) |> 
  rowwise() |> 
  mutate(
    opd = sum(new, re_attendancies,na.rm = T) ) |> 
  rename(
    sub_county = orgunitlevel4,
    district = orgunitlevel3,
    malaria_index = Malaria_total
  ) |> 
  select(c(district,sub_county,malaria_index))

## Combine the data 
combined_data <- sub_counties |> 
  left_join(data_malaria)

## Basic Plot 
basic_map <- combined_data |> 
  ggplot()+
  geom_sf(aes(fill = malaria_index), lwd = 0.05, color = "white")





## Style and complete the report 
sub_country_map <- ggplot()+ 
  geom_sf(data = uganda_sp,color = "black",lwd = 0.05)+
  geom_sf(data = combined_data ,aes( fill = malaria_index), lwd = 0.05, color = "white")+
  geom_sf(data = water_sp, fill = "#0077b6")+
  scale_fill_fermenter(palette = "YlOrRd", 
                       breaks = seq(0, 20000, 5000),
                       name = "Malaria cases", 
                       direction = 2) +
  theme_map()+
  labs(
    title = "Malaria Cases - Sub county level",
    subtitle = "Total Number of Malaria cases seen in  January to March 2024",
    caption = "Source: DHIS2  | Visual : Rutayisire"
  ) +
  theme(
    # Legend
    legend.position = "top",
    legend.justification = 0.5,
    legend.key.size = unit(1.25, "cm"),
    legend.key.width = unit(1.75, "cm"),
    legend.text = element_text(size = 20),
    legend.margin = margin(),
    # Increase size and horizontal alignment of the both the title and subtitle
    plot.title = element_text(size = 48, hjust = 0.5),
    plot.subtitle = element_text(size = 30, hjust = 0.5), 
    plot.caption =  element_text(size = 24)
  )

## Bar chart

# Calculate population share in each HDI group

group_malaria <- data_malaria |> 
  st_drop_geometry() |> 
  mutate(
    group = findInterval(malaria_index, seq(0, 20000, 5000), left.open = FALSE),
    group = factor(group)
  ) |> 
  group_by(group) |> 
  summarise(score = sum(malaria_index, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(share = score / sum(score) * 100) |> 
  na.omit()


## Basic plots 

ggplot(data = group_malaria)+
  geom_col(aes(x = group, y = share, fill = group))


# Create a variable to store the position of the text label
group_malaria <- group_malaria |> 
  mutate(
    y_text = if_else(group %in% c(0, 5), share + 3, share - 40),
    label = paste0(round(share, 1), "%")
  )

ggplot(group_malaria, aes(group, share, fill = group)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  # Text labels
  geom_text(
    aes(y = y_text, label = label, color = group),
    size = 3
  ) +
  coord_flip() +
  # Use the same color palette as the map
  scale_fill_brewer(palette = "YlOrRd", direction = 2) +
  # Swap between black and white text
  scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
  # Hide color legend
  guides(fill = "none", color = "none")


## Final Bar plot 
# Labels for the color legend
x_labels <- c(
  "5000 or less", "5001 to 10000", "10001 to 15000", 
  "15001 to 20000",
  "Above 20000")

pcol <- ggplot(group_malaria, aes(group, share, fill = group)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  geom_text(
    aes(y = 10, label = label,
        color = group , name = "Malaria Cases"),
    size = 6
  ) +
  coord_flip() +
  scale_x_discrete(labels = x_labels) +
  scale_fill_brewer(palette = "YlOrRd" , direction = 2) +
  scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
  guides(fill = "none", color = "none") +
  labs(
    title = " ",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_blank()
  )

pcol


## Combine Plots 
malaria_visual <-
  sub_country_map + 
  inset_element(pcol, 0.79, 0.1, 1, 0.3, align_to = 'full')


#malaria_visual 


ggsave("malaria_visual .jpg", plot = malaria_visual, 
       width = 12, height = 11, dpi = 300)



