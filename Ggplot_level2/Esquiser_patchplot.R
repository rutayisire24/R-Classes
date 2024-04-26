

## install pacman 

install.packages('pacman')
install.packages('esquisse')

## load the libraries 
library(tidyverse)
library(pacman)
library(esquisse)

## using pacman 
p_load(patchwork)

## load the data 

#data_facility  <- MFL.Updated...21.feb

## facilities per region 

data_region <- readr::read_csv("https://raw.githubusercontent.com/rutayisire24/R-Classes/main/Ggplot_level2/data_region.csv")

## facilities by level

data_level <- readr::read_csv("https://raw.githubusercontent.com/rutayisire24/R-Classes/main/Ggplot_level2/data_level.csv")

## by authority

data_authority <- readr::read_csv("https://raw.githubusercontent.com/rutayisire24/R-Classes/main/Ggplot_level2/data_authority.csv")


## Esquiser 


library(ggplot2)

## plot by authority 
authority_plot <- ggplot(data_authority) +
 aes(x = reorder(authority,n), y = n) +
 geom_col(fill = "#112446") +
  coord_flip() +
 labs(x = " ", y = "",
 title = "Facilities by Authority") +
 ggthemes::theme_economist() +
 theme(plot.title = element_text(size = 15L, 
 face = "bold"), axis.title.x = element_text(size = 13L, face = "bold")) +
 ylim(0, 5000)

##  plot by level 


library(ggplot2)

plot_by_level <- ggplot(data_level) +
 aes(x = reorder(hflevel,n), y = n) +
 geom_col(fill = "#112446") +
 labs(x = " ", y = "Counts of Facilities", 
 title = "Facilities by Level",
 caption = "Visualisation by Meddy | Data Source MFL") +
 coord_flip() +
 ggthemes::theme_economist() +
 theme(plot.title = element_text(size = 15L, face = "bold"))

## region 

region <- ggplot(data_region) +
  aes(x = reorder(region,n), y = n) +
  geom_col(fill = "#112446") +
  labs(
    x = " ",
    y = " ",
    title = "Facilities by Region"
  ) +
  coord_flip() +
  ggthemes::theme_economist() +
  theme(plot.title = element_text(size = 15L, face = "bold"))

## patchwork 

combined_plot <-   plot_by_level  + region / authority_plot

ggsave("combined_plot.jpg" , width =  10 , height =  8)


