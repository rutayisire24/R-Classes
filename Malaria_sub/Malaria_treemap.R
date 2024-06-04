
rm(list = ls(all = T))

pacman::p_load(
  tidyverse,      # grammar of data and graphics
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  colorspace,     # fancy stuff with colors 
  janitor,        # some efficient data cleaning stuff
  camcorder,      # record the making of the plot into a gif
  tidytuesdayR,   # download Tidy Tuesday Data
  paletteer,      # color palettes
  treemapify,     # Treemap
  glue            # glue together formatted text
)

malaria_data <- readxl::read_excel(here("malaria_data.xlsx"))

malaria_data <- malaria_data |> 
  rename(
    region = orgunitlevel2,
    district = orgunitlevel3
  ) |> 
  group_by(region,district) |> 
  summarise(cases = sum(Malaria_total,na.rm = T))

top_regions <- malaria_data |> 
  group_by(region) |> 
  summarise(cases = sum(cases,na.rm = T)) |> 
  arrange(desc(cases)) |> 
  slice_max(order_by = cases, n = 6)


data_malaria <- malaria_data |> 
  group_by(region,district) |> 
  filter(region %in% top_regions$region) |> 
  summarise(cases = sum(cases,na.rm = T)) |> 
  group_by(region) |> 
  slice_max(order_by = cases, n = 3) |> 
  arrange(region,cases)

## colors 
# myPal <- c("#c9184a", "#ff4d6d",
#            "#ff758f", "#ff8fa3", 
#            "#ffb3c1", "#fff0f3" )

myPal <- c("#b08968","#b08968", "#b08968", "#b08968", "#b08968", "#b08968", "#b08968" )


back_colour = "#EFEFEF"
strong_text = darken("darkgreen",0.7)
weak_text = lighten(strong_text, 0.1)

## Fonts 

## Main fonts 

font_add(
  family = "Roboto",
  regular = here("fonts/Roboto.ttf"),
  bold  = here("fonts/RobotoBold.ttf")
)

showtext_auto()

main_font = "Roboto"


# Assuming data_malaria is your dataframe
data_malaria <- data_malaria %>%
  mutate(cases_label = paste(district, ":", round(cases / 1000), "K"))

my_caption <- glue("<b>Data: </b> DHIS2 |  </b>Visual: </b> Rutayisire")
my_subtitle <- paste("A description of the plot")

plot_malaria <- ggplot(data_malaria, aes(area = cases, fill = region, subgroup = region)) +
  geom_treemap(aes(alpha = cases), colour = "white", linetype = 2) +
  geom_treemap_subgroup_border(color = "#EFEFEF") +
  geom_treemap_subgroup_text(
    place = "topleft", grow = F, alpha = 0.5, colour = "black",
    min.size = 0, family = main_font
  ) +
  geom_treemap_text(
    aes(label = cases_label), colour = "black", alpha = 0.7,
    place = "bottomleft", min.size = 2, family = main_font
  ) +
  scale_fill_manual(values = myPal) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  labs(
    title = "Regions With the Most Malaria Cases - Jan to March 2024",
    subtitle = "This type of visualization is called a tree map. Here, the area of each rectangle corresponds to the number of Malaria cases per region and sub-category 
    represent 3 Districts with the highest cases in that region in '000s(K).
    The bigger and darker the box the more the Cases",
    caption = my_caption
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = back_colour, color = back_colour),
    plot.background = element_rect(fill = back_colour, colour = back_colour),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = rel(2.1), family = main_font, face = "bold",
      color = strong_text, margin = margin(8, 0, 8, 0)
    ),
    plot.subtitle = element_textbox_simple(
      size = rel(1.1), family = main_font, colour = weak_text,
      margin = margin(0, 0, 10, 0)
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    plot.caption = element_markdown(
      size = rel(0.8), colour = weak_text, family = main_font,
      hjust = c(0), margin = margin(10, 0, 0, 0)
    )
  )

# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(plot = plot_malaria, filename = here("Malaria_visual.png"), height = 6, width = 8)
