

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

esquisser()
