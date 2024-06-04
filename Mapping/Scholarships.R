
rm(list = ls())

## Load libraries
library(tidyverse)
library(readr)
library(patchwork)
library(extrafont)
loadfonts()


## Load data 
data_url <- "https://raw.githubusercontent.com/rutayisire24/Open-Data-Uganda/main/Education/Government%20Scholarships%20Public%20Universities.csv"
data <- read_csv(data_url) |> 
  mutate(School = str_to_title(School))

## Explore the data 
head(data)
glimpse(data)

## categorise the programs 
# Classify programs into broad themes
program_classification <- data %>%
  mutate(
    Theme = case_when(
      str_detect(Program, regex("Business|Commerce|Economics|Entrepreneurship|Management", ignore_case = TRUE)) ~ "Business",
      str_detect(Program, regex("Engineering|Technology|Engineer|Architecture|Land Surveying", ignore_case = TRUE)) ~ "Engineering and Design",
      str_detect(Program, regex("Agricultur|Agribusiness|Fisheries|Forestry", ignore_case = TRUE)) ~ "Agriculture",
      str_detect(Program, regex("Medicine|Medical|Nursing|Pharmacy|Biomedical|Surgery|Health|Optometry|Speech", ignore_case = TRUE)) ~ "Health Sciences",
      str_detect(Program, regex("Arts|Music|Fine Art|Design|Communication|Library|Law", ignore_case = TRUE)) ~ "Arts",
      str_detect(Program, regex("Education|Teaching", ignore_case = TRUE)) ~ "Education",
      TRUE ~ "Others"
    )
  )

# Summarize the number of scholarships per school and theme
scholarships_summary <- program_classification %>%
  group_by(Theme, School) %>%
  summarise(Total_Scholarships = n()) %>%
  arrange(Theme, desc(Total_Scholarships))

# Identify the top 5 schools for each theme
top_schools <- scholarships_summary %>%
  group_by(Theme) %>%
  slice_max(Total_Scholarships, n = 5)

# Summarize the total number of scholarships per theme
theme_totals <- scholarships_summary %>%
  group_by(Theme) %>%
  summarise(Total_Scholarships_Theme = sum(Total_Scholarships))

# Calculate the percentage of scholarships for top schools within each theme
top_schools_percentage <- top_schools %>%
  left_join(theme_totals, by = "Theme") %>%
  mutate(Percentage = round((Total_Scholarships / Total_Scholarships_Theme) * 100))
 
## Plots 
sysfonts::font_add_google(name = "Amatic SC", family = "amatic-sc")
showtext::showtext_auto()

Health <- top_schools_percentage |>
  filter(Theme == "Health Sciences") |> 
  mutate(School = fct_reorder(School,Total_Scholarships)) |> 
  ggplot(aes(School,Total_Scholarships, label = paste0(Percentage,"%")))+
  geom_col(fill = "#006d77", color = "white")+
  geom_text(aes(y = 2), vjust = 0, size = 5, color = "white") +  # Use Cambria font for text
  coord_flip()+
  theme_minimal()+
  labs(x = "", y = "", 
       title = "Leading Health Science Scholarship Recipients -  Makerere Univerity|2023",
       subtitle = "These top schools take 40% of the Total scholarships awarded in Health Sciences assessed (180)" )+
  theme(axis.text.x = element_blank(),  # Remove y-axis tick labels
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20), 
        plot.title = element_text(hjust = 0, size = 28, face = "bold"),
        plot.subtitle =  element_text(hjust = 0, size = 24))



ggsave("Health.jpg", plot = Health, width = 10, height = 7, units = "in", dpi = 200)


Engineering <- top_schools_percentage |>
  filter(Theme == "Engineering and Design") |> 
  mutate(School = fct_reorder(School,Total_Scholarships)) |> 
  ggplot(aes(School,Total_Scholarships, label = paste0(Percentage,"%")))+
  geom_col(fill = "#5e548e", color = "white")+
  geom_text(aes(y = 2), vjust = 0, size = 5, color = "white") +  # Use Cambria font for text
  coord_flip()+
  theme_minimal()+
  labs(x = "", y = "", 
       title = "Leading Engneering or Design  Scholarship Recipients -  Makerere Univerity | 2023",
       subtitle = "These top schools take 37% of the Total scholarships awarded in Engineering or Design  assessed (291)" )+
  theme(axis.text.x = element_blank(),  # Remove y-axis tick labels
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20), 
        plot.title = element_text(hjust = 0, size = 28, face = "bold"),
        plot.subtitle =  element_text(hjust = 0, size = 24))

ggsave("engneer.jpg", plot =  Engineering, width = 10, height = 6, units = "in", dpi = 200)

 
