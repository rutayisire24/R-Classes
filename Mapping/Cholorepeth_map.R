
rm(list = ls())

## Class --  is to produce a Cholorepath Map 

# A choropleth map is a thematic map that visualizes geographical areas or
# regions clearly divided with colors, shades or patterns 
# in relation to a given variable. 

## Inspiration 
## https://r-graph-gallery.com/web-choropleth-barchart-map.html

## Load libraries with pacman 

pacman::p_load(ggplot2,ggthemes,patchwork, dplyr,readr,sf)

## Load data 
link <- "https://github.com/viniciusoike/restateinsight/raw/main/static/data/atlas_sp_hdi.rds?download="

atlas <- readr::read_rds(link)

## Explore the Data 

names(atlas)
glimpse(atlas)

## Basic Map

ggplot(data = atlas)+
  geom_sf()

# Color by HDI - Human Development Index 

ggplot(data = atlas)+
  geom_sf(aes(fill = HDI), color = "red")

ggplot(data = atlas)+
  geom_sf(aes(fill = pop))

## change borders to white 

ggplot(data = atlas)+
  geom_sf(aes(fill = HDI), lwd = 0.05, color = "white")


## Add a legend 

## assign plot 

basic_plot <- ggplot(data = atlas)+
  geom_sf(aes(fill = HDI), lwd = 0.05, color = "white")

print(basic_plot)
## custom palette 

basic_plot +
  scale_fill_fermenter(palette = "YiGnBu")

## build up change name of the legend , bins , direction and palette 
## explore palletes 

ggplot(atlas) +
  geom_sf(aes(fill = HDI), lwd = 0.05, color = "white") +
  scale_fill_fermenter(
    name = "",
    breaks = seq(0.65, 0.95, 0.05),
    direction = 2,
    palette = "Spectral"
  )

## asign to better plot 

slightly_better_map <- ggplot(atlas) +
  geom_sf(aes(fill = HDI), lwd = 0.05, color = "white") +
  scale_fill_fermenter(
    name = "",
    breaks = seq(0.65, 0.95, 0.05),
    direction = 2,
    palette = "Spectral"
  )

slightly_better_map

## add labels 

labelled_map <- slightly_better_map +
  labs(
    title = "HDI in Sao Paulo, BR (2010)",
    subtitle = "Microregion HDI in Sao Paulo",
    caption = "Source: Atlas Brasil | Visual : Rutayisire"
  ) +
  theme_map()

## themed map 

labelled_map+
  theme(legend.position = "right")


themed_map <- labelled_map +
  theme(
    # Legend
    legend.position = "left",
    legend.justification = 0.8,
    legend.key.size = unit(1.25, "cm"),
    legend.key.width = unit(1.75, "cm"),
    legend.text = element_text(size = 12),
    legend.margin = margin(),
    # Increase size and horizontal alignment of the both the title and subtitle
    plot.title = element_text(size = 32, hjust = 0.5),
    plot.subtitle = element_text(size = 24, hjust = 0.5), 
    plot.caption =  element_text(size = 20)
  )
  
## Bar chart

# Calculate population share in each HDI group

pop_hdi <- atlas |> 
  st_drop_geometry() |> 
  mutate(
    group_hdi = findInterval(HDI, seq(0.65, 0.95, 0.05), left.open = FALSE),
    group_hdi = factor(group_hdi)
  ) |> 
  group_by(group_hdi) |> 
  summarise(score = sum(pop, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(share = score / sum(score) * 100) |> 
  na.omit()


## Basic plots 

ggplot(data = pop_hdi)+
  geom_col(aes(x = group_hdi, y = share, fill = group_hdi))


# Create a variable to store the position of the text label
pop_hdi <- pop_hdi |> 
  mutate(
    y_text = if_else(group_hdi %in% c(0, 7), share + 3, share - 3),
    label = paste0(round(share, 1), "%")
  )



ggplot(pop_hdi, aes(group_hdi, share, fill = group_hdi)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  # Text labels
  geom_text(
    aes(y = y_text, label = label, color = group_hdi),
    size = 3
  ) +
  coord_flip() +
  # Use the same color palette as the map
  scale_fill_brewer(palette = "Spectral") +
  # Swap between black and white text
  scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
  # Hide color legend
  guides(fill = "none", color = "none")


## Final Bar plot 
# Labels for the color legend
x_labels <- c(
  "0.650 or less", "0.650 to 0.699", "0.700 to 0.749", "0.750 to 0.799",
  "0.800 to 0.849", "0.850 to 0.899", "0.900 to 0.949", "0.950 or more"
)

pcol <- ggplot(pop_hdi, aes(group_hdi, share, fill = group_hdi)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  geom_text(
    aes(y = y_text, label = label, color = group_hdi),
    size = 10
  ) +
  coord_flip() +
  scale_x_discrete(labels = x_labels) +
  scale_fill_brewer(palette = "Spectral") +
  scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Population share by HDI group",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 28),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank()
  )

pcol

bar_plot <- 
  ggplot(pop_hdi, aes(group_hdi, share, fill = group_hdi)) + 
  geom_col(width = 0.5) + 
  labs(x = "HDI Group", y = "Population Share (%)", title = "Population Share by HDI Group") + 
  geom_col() +
  geom_hline(yintercept = 0) +
  # Text labels
  geom_text(
    aes(y = y_text, label = label, color = group_hdi),
    size = 3
  ) +
  coord_flip() +
  # Use the same color palette as the map
  scale_fill_brewer(palette = "YlGnBu") +
  # Swap between black and white text
  scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
  # Hide color legend
  guides(fill = "none", color = "none")

combined <- p_hdi_atlas +  inset_element(pcol, left = 0.50, bottom = 0.05, 
                                          right = 1, top = 0.5)

combined
  


## Combine Plots 
combined <- 
  themed_map + inset_element(pcol, left = 0.50, bottom = 0.05, 
                             right = 1, top = 0.5)

combined

## save the plot 

ggsave("combined_plot.jpg", plot = combined, width = 15, height = 14, dpi = 150)

