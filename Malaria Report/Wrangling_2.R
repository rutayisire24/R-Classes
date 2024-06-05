
## load libraries 
pacman::p_load(
  readxl, ## read xlsx
  tidyverse, ## several packages for wrangling 
  here ## locate files 
)

## load data 

data_malaria <- read_excel(here("District_data.xlsx"))

##  Preview data 

names(data_malaria)

glimpse(data_malaria)


## explore data elements 

unique(data_malaria$dataelement)
unique(data_malaria$district)
unique(data_malaria$period_date)

## rename ------------------

## rename , period_date and dataelement 

malaria_new <- data_malaria |> 
  rename(
    month = period_date, 
    blank = Column1
  ) |> 
  select(-blank)

## assign 

## option one use names()



## option two use rename()

##------------ pivot 

## pivot wider 

wider_data <- data_malaria |> 
  pivot_wider(
    names_from = dataelement,
    values_from = value
  )

## pivot longer 

longer_data <- wider_data |> 
  select(c(1,2,10,11,12)) |> 
  pivot_longer(
    cols = c(3,4,5), 
    names_to = "Variable",
    values_to = "values"
  )

## select 

selected_data <- wider_data |> 
  select(c(
    district,period_date,
    `105-AN010. Third dose IPT (IPT3)`, 
    `105-DT01. Deaths in OPD`
  ))


## pivot longer 


## ----- Use of select helpers with filter 


## select  wide data 


## filter all with ANC 

data_anc <- data_malaria |> 
  filter(grepl("ANC", dataelement))

## use gsub to replace District 

data_anc$district <- gsub(" District","",data_anc$district)


## arrange top districts with deaths 



## slice districts with most deaths _ total 

top_districts_deaths <- data_malaria |> 
  filter(grepl("No. of deaths", dataelement)) |> 
  group_by(district) |> 
  summarise(deaths = sum(value,na.rm = T)) |> 
  slice_max(order_by = deaths , n = 10)




