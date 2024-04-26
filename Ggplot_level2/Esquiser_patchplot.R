

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

data_facility  <- MFL.Updated...21.feb

## facilities per region 

data_region <- data_facility %>%
  group_by(region) %>%
  count()

saveRDS(data_region,"data_region.rds")

## facilities by level

data_level <- data_facility %>%
  group_by(hflevel) %>%
  count()  %>% 
  filter ( n != 18)

saveRDS(data_level,"data_level.rds")


## by authority

data_authority <- data_facility %>%
  group_by(authority) %>%
  count() %>% 
  filter ( n > 50)

saveRDS(data_authority,"data_authority.rds")

