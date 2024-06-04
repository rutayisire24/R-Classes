
## load libraries 
pacman::p_load(
  readxl,tidyverse,here
)

## load data 

data_malaria <- read_excel(here("District_data.xlsx"))

##  Preview data 

names(data_malaria)
glimpse(data_malaria)


## explore data elements 

unique(data_malaria$dataelement)

## rename ------------------

## rename , period_date and dataelement 

## assign 

## option one use names()

## option two use rename()

##------------ pivot 

## pivot wider 



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


  



