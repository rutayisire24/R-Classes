
rm(list = ls(all=T))

if(!require("pacman")) install.packages("pacman")

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


## load the data 
tuesdata <- tidytuesdayR::tt_load(2024,week = 22)


data_2021 <- tuesdata$planting_2021 |> 
  mutate(
    vegetable = str_to_title(vegetable),
    variety = str_to_title(variety)
  )

top_veg <- data_2021 |> 
  group_by(vegetable) |> 
  summarise(seeds = sum(number_seeds_planted)) |> 
  arrange(
    desc(seeds)) |> 
  slice_max(
    order_by = seeds , n = 6
  )


data_2021 <- data_2021 |> 
  filter(vegetable %in% top_veg$vegetable) |> 
  group_by(vegetable, variety) |> 
  summarise(seeds = sum(number_seeds_planted)) |> 
  group_by(vegetable) |> 
  arrange(vegetable, seeds)


## colors 
myPal <- c("purple4", "orange", "yellow", "green3", "lightyellow", "darkgreen" )

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

github_icon <- "&#xf09b"
github_username <- "GregoryVdvinne  "

twitter_icon <- "\uf099"
twitter_username <- "@GregoryVdvinne  "

linkedin_icon <- "\uf08c"
linkedin_username <- "Gregory Vander Vinne"

## caption 

my_caption <- glue("<b>Data: </b> DHIS2",)

my_subtitle <- paste(substitle = "A description of the plot")


## basic 
# The Actual Plot --------------------------------------------------------------
ggplot(data = data_2021, aes(area = seeds, fill = vegetable, 
                          subgroup = vegetable)) + # Very weird to me that subgroup is not variety
  geom_treemap(aes(alpha = seeds)) + 
  geom_treemap_subgroup_border() + 
  geom_treemap_subgroup_text(place = "middle", grow = F, alpha = 0.7, colour =
                               "black", min.size = 0, family = main_font) +
  geom_treemap_text(aes(label = paste(variety, ":", seeds, "seeds")), 
                    colour = "black", alpha = 0.4, place = "bottomleft", 
                    min.size = 2, family = main_font) + 
  scale_fill_manual(values = myPal) + 
  scale_alpha_continuous(range = c(0.5,1)) + 
  labs(title = "Regions With the Most Malaria Cases - Q1 2024", 
       subtitle = "This type of visualization is called a tree map. Here, the 
       area of each rectangle corresponds to the number of Malaria cases  per 
       region and sub-category represent the districts with the highest cases.", 
       caption = my_caption) + 
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = back_colour,
                                    color = back_colour),
    plot.background = element_rect(fill = back_colour, 
                                   colour = back_colour),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = rel(2.1),
                                        family = main_font,
                                        face = "bold",
                                        color = strong_text,
                                        margin = margin(8, 0, 8, 0)),
    plot.subtitle = element_textbox_simple(size = rel(1.1),
                                           family = main_font,
                                           colour = weak_text,
                                           margin = margin(0, 0, 10, 0)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    plot.caption = element_markdown(size = rel(0.8),
                                    colour = weak_text,
                                    family = main_font,
                                    hjust = c(0), 
                                    margin = margin(10,0,0,0))
  )

# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("visual_garden.png"), height = 6, width = 8)


