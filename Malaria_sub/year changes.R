
## load data 

rm(list = ls(all = T))

malaria_years <- readxl::read_excel("malaria_years.xls")


wide <- malaria_years |> 
  pivot_wider(names_from = periodname,
              values_from = x105_ep01b_malaria_total)

malaria_years <- janitor::clean_names(malaria_years)

## summary 

# Summarize cases for each year
national_summary <- malaria_years |> 
  group_by(periodname) |> 
  summarise(organisationunitname = "National", 
            x105_ep01b_malaria_total = sum(x105_ep01b_malaria_total))

# Combine the original data with the national summary
malaria_years  <- bind_rows(malaria_years, national_summary)


malaria_summary <- malaria_years |> 
  summarise(
    total_cases = sum(x105_ep01b_malaria_total,na.rm = T),
    .by = c(organisationunitname,periodname)
  ) |> 
  group_by(organisationunitname) |> 
  mutate(
    yoy_change = (total_cases - lag(total_cases)) / lag(total_cases) * 100   # YoY percentage change
  ) |> 
  filter(!is.na(yoy_change))|>
  ungroup() |>
  mutate(
    organisationunitname = str_to_title(organisationunitname),
    organisationunitname = fct_reorder(organisationunitname, yoy_change),
    direction = ifelse(yoy_change > 0, "increase", "decrease")
  )

# 5. VISUALIZATION ---- 
### |- plot aesthetics ---- 
bkg_col      <- "#323238" 
title_col    <- "#d7d7d8"             
subtitle_col <- "#d7d7d8"   
caption_col  <- "#d7d7d8"   
text_col     <- "#d7d7d8"  
col_palette  <- c("increase" = "#F44336", "decrease" = "#4CAF50")

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 22 } &bull; Source: {{gardenR}}<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")

mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Malaria Cases - Uganda's Regions") 

subtitle_text <- "Year-over-Year Percentage Change in Total 
                          Malaria Cases 2022 - 2023\nThe National Cases reduced by 20% and in most regions 
                          except Kampala. "

caption_text  <- "Data: DHIS2(105) | <br>Visualisation: Rutayisire"



### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = 'plot',
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
  axis.text             = element_text(size = rel(1), color = text_col, family = 'text'),
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray40'),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_blank(),
  axis.line.x           = element_line(color = "#d7d7d8", linewidth = .2),
)

plot_malaria <- malaria_summary |> 
  ggplot(aes(x = organisationunitname, y = yoy_change, color = direction)) +
  
  # Geoms
  geom_hline(
    yintercept = 0, linetype = 1, linewidth = 0.1, color = "#d7d7d8"
  ) +
  geom_segment(
    aes(x = organisationunitname, xend = organisationunitname, y = 0, yend = yoy_change),
    size = 1.2
  ) +
  geom_point(
    aes(size = abs(yoy_change)),
    shape = 21,
    fill = "#d7d7d8"
  ) +
  geom_text(
    aes(label = paste0(scales::percent(yoy_change / 100, accuracy = 1))),
    hjust = ifelse(malaria_summary$yoy_change > 0, -0.55, 1.5),
    nudge_y = 0, size = 3.5, family = "text", color = "#d7d7d8"
  ) +
  
  # Scales
  scale_x_discrete(expand = expansion(add = c(.8, .6)))+
  scale_y_continuous(
    breaks = seq(-50, 20, by = 10),
    limits = c(-50, 20),
    expand = c(0, 0),
    labels = number_format()
  ) +
  scale_color_manual(values = col_palette) +
  scale_size_continuous(range = c(1, 6), guide = "none") +
  coord_flip(clip = "off") +
  
  # Labs
  labs(
    x = NULL,
    y = "YoY Change (%)",
    title    = title_text,
    subtitle = "The National Cases reduced by 20% . In most Regions cases reduced except Kampala.",
    caption  = caption_text
  ) +
  
  # Theme
  theme( 
    plot.title      = element_text(
      size        = rel(1.8), 
      family      = 'title',
      color       = title_col,
      face        = 'bold',
      lineheight  = 0.85,
      margin      = margin(t = 5, b = 5)),
    
    plot.subtitle   = element_markdown(
      size        = rel(0.90), 
      family      = 'subtitle',
      color       = title_col,
      lineheight  = 1, 
      margin      = margin(t = 10, b = 20)),
    
    plot.caption    = element_markdown(
      size        = rel(.65), 
      family      = 'caption',
      color       = caption_col,
      lineheight  = 0.6,
      hjust       = 0,
      halign      = 0,
      margin      = margin(t = 10, b = 10)),
  )



ggsave(
       "plot_malaria.png",
       width = 8,
       height = 10.5,
       dpi = 300)


  