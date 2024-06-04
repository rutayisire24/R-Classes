
rm(list = ls(all= T))

pacman::p_load(
  tidyverse,
  here,
  ggrepel
)

malaria_district <- readxl::read_xlsx(here("malaria_district.xlsx"))

Busoga_districts <- malaria_district |> 
  filter(region  == "Busoga") |> 
  distinct(District)

view <- malaria_district |> 
  mutate(period = my(periodname),
         is_busoga = ifelse(District %in% Busoga_districts$District, "Busoga", "Other")) %>%
  filter(cases < 50000)

# Create a data frame for labeling (random point of each district in Busoga region)
set.seed(42)  # for reproducibility
label_data <- view %>%
  filter(is_busoga == "Busoga") %>%
  group_by(District) %>%
  sample_n(1) %>%
  ungroup()

# Plotting
ggplot(view, aes(x = period, y = cases, group = District)) +
  geom_line(aes(color = is_busoga, alpha = is_busoga)) +
  scale_color_manual(values = c("Busoga" = "red", "Other" = "grey")) +
  scale_alpha_manual(values = c("Busoga" = 1, "Other" = 0.2)) +
  guides(alpha = "none") +  # to remove alpha legend
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text_repel(data = label_data,
                  aes(label = District),
                  nudge_y = 0.05, # adjust the nudging as needed
                  direction = "y",
                  box.padding = 0.5,
                  point.padding = 0.5,
                  max.overlaps = Inf)
