library("tmap")
library("tmaptools")
library("sp")
# library("rgdal")
library("here")
library("tidyverse")
library("sf")   # convert spreadsheets to shape files
library(yarrr)  # palettes
library(readxl)


# import data from old dhis2 ----
old16 <- read_excel("data/old_16.xls")
old17 <- read_excel("data/old_17.xls")
old18 <- read_excel("data/old_18.xls")
old19 <- read_excel("data/old_19.xls")

# import data from new dhis2 ----
new <- read_excel("data/dhis2_new.xls")

# merge old dataframe ----
old <- rbind(old16, old17, old18, old19)

# clean variable names ----
names(old) <- c( "country"
                 ,"region"
                 ,"district"
                 ,"district2"
                 ,"period"
                 ,"opd_new"
                 ,"opd_tot"
                 ,"measles_cases"
                 ,"measles_dth"
                 ,"mic_pos"
                 ,"mic_test"
                 ,"rdt_pos"
                 ,"rdt_test"
                 ,"fever"
                 ,"mic_neg_t"
                 ,"mic_pos_t"
                 ,"not_test_t"
                 ,"rdt_pos_t"
                 ,"rdt_neg_t"
)

names(new) <- c( "country"
                 ,"region"
                 ,"district"
                 ,"district2"
                 ,"period"
                 ,"opd_new"
                 ,"opd_tot"
                 ,"mal_cases"
                 ,"mal_dth"
                 ,"fever"
                 ,"rdt_test"
                 ,"rdt_pos"
                 ,"mic_test"
                 ,"mic_pos"
                 ,"not_test_t"
                 ,"rdt_neg_t"
                 ,"rdt_pos_t"
                 ,"mic_neg_t"
                 ,"mic_pos_t"
)
# select relevant data elements ----
new_old <- old %>% select(
  region, district, period, opd_tot, fever, 
  rdt_test, rdt_pos, mic_test, mic_pos, 
  not_test_t, rdt_neg_t, rdt_pos_t, mic_neg_t, mic_pos_t 
)

new_new <- new %>% select(
  region, district, period, opd_tot, fever, 
  rdt_test, rdt_pos, mic_test, mic_pos, 
  not_test_t, rdt_neg_t, rdt_pos_t, mic_neg_t, mic_pos_t 
)

# merge dataframes ----

new_old$data_age <- 0 # identify old DHIS2 data
new_new$data_age <- 1 # identify new DHIS2 data

data <- rbind(new_old  , new_new)

# clean data
data %<>% separate(
  period,
  c("epi_week_w","year"),
  sep=" ",
  remove=FALSE
) %>%
  filter(
    data_age==1 & year %in% c("2020","2021","2022") 
    |
      data_age==0 & year %in% c("2016","2017","2018","2019")
  )

# generating new variables

data <- data %>% 
  rowwise() %>%
  mutate(
    
    wk       = as.integer( substring(epi_week_w,2) ),
    yr       = as.integer( year ),
    t_test   = sum(rdt_test,mic_test, na.rm = TRUE),
    t_pos    = sum(rdt_pos, mic_pos, na.rm = TRUE),
    t_tr     = sum(not_test_t,rdt_pos_t,rdt_neg_t,mic_pos_t,mic_neg_t, na.rm = TRUE),
    t_pos_tr = sum(rdt_pos_t,mic_pos_t, na.rm = TRUE),
    t_neg_tr = sum(rdt_neg_t,mic_neg_t, na.rm = TRUE),
    t_neg    = t_test - t_pos,
    tpr      = 100 * t_pos /t_test,
    dt       = as.Date( paste( yr, wk, 1, sep="-"),"%Y-%U-%u" )
  ) 

# make a copy management ----

data_original <- data


#install.packages('terra', repos='https://rspatial.r-universe.dev')
#install.packages('raster', repos='https://rspatial.r-universe.dev')


# Read the shapefile using sf

shp_district <- st_read( here("shape", "ds.shp" ) )
shp_lake     <- st_read( here("shape", "lk.shp" ) )



district_map_border <- tm_shape(shp_district) + tm_borders()
district_map_fill   <- tm_shape(shp_district) + tm_fill()

district_map_polygon   <- tm_shape(shp_district) + tm_polygons()

lake_map_polygon    <- tm_shape(shp_lake)     + tm_polygons()

pm <- pm + tm_shape(lk) + tm_polygons() 

tm_shape(shp_district) + 
  tm_polygons("yellow") +
  tm_shape(lk) + 
  tm_polygons("blue" ) 


View(shp_district)
View(data)

# Join datasets for shape fill and the data: start by creating variables for matching
# shape files are flat-ish: unless you plan to facet the maps

data$districts <- sub(" District", "", data$district)
data$districts <- tolower(data$districts)

# 
# with one week
#

mapData <- data %>% filter(yr==2022 & wk==2) %>% select(districts, fever)

# merge data set
shp_district$districts <- tolower(shp_district$DName2018)
shp_district<- merge(x=shp_district,y=mapData, by="districts",all=TRUE)

# Generate breaks
mp1 <- tm_shape(shp_district)+  
  tm_fill( col=c("fever"),
           title=c("No of fever cases"), 
           textNA = "No cases", 
           style="fixed",
           breaks=c(0, 200, 800, 1200, 2000,5000,8000, Inf), 
           palette = "YlOrBr") +  
  tm_shape(shp_lake) + 
  tm_polygons("#a6cee3" ) +
  tm_layout(bg.color="white")

mp1

# 
# with more weeks
#

mapDatX <- data %>% 
  filter(yr==2022  & wk <= 2 )  %>% 
  select(districts, epi_week_w, fever) %>%
  spread(key = epi_week_w,value = fever)

shp_district$districts <- tolower(shp_district$DName2018)


shp_district<- merge(x=shp_district,y=mapDatX, by="districts",all=TRUE)


mp2 <- tm_shape(shp_district) +  
  tm_fill( col=c("W1", "W2"),
           title=c("Week 1", "Week 2"), 
           textNA = "No cases", 
           style="fixed",
           breaks=c(0, 200, 800, 1200, 2000,5000,8000, Inf), 
           palette = "YlOrBr") +  
  tm_shape(shp_lake) + 
  tm_polygons("#a6cee3" ) +
  tm_layout(bg.color="white")


# mapping points from excel sheet
geo_map <- read_excel("data/geo_map.xlsx")     # import data to dataframe: package readxl required
geo_map <- geo_map[   !is.na(geo_map$Latitude)   ,      ] # remove any NA


#package sf
#covert dataframe to shapefile object: package sf required
tif_sf <- st_as_sf(geo_map, coords = c("Longitude", "Latitude"), crs = 4326)

# example 1
tm_shape(tif_sf) + tm_dots()

pm  + tm_shape(tif_sf) + tm_dots()

# example 2
mp1 + tm_shape(tif_sf) + tm_dots()


# example 3
pm  + tm_shape(tif_sf) + 
  tm_dots( 
    "I:Ownership",  
    palette = c("red","blue","green"),
    stretch.palette = FALSE,
    size = 0.09
  )

# example 4
pm  + tm_shape(tif_sf) + 
  tm_dots( "I:Ownership",  
           palette = "Spectral",
           stretch.palette = FALSE,
           size = 0.05
  ) + 
  tm_legend(legend.outside = TRUE)

piratepal(palette = "all")   # show palettes
myTestColor <- piratepal(palette = "basel") # extract specific color

# example 5
pm  + tm_shape(tif_sf) + 
  tm_dots( "I:Ownership",  
           palette = myTestColor,
           stretch.palette = FALSE,
           size = 0.05
  ) + 
  tm_legend(legend.outside = TRUE)


# additional stats

geo_map_more <- read_excel("data/geo_map_more.xlsx")
geo_map_more <- geo_map_more[!is.na(geo_map_more$Latitude ) , ] # remove any NA
geo_map_more <- geo_map_more[!is.na(geo_map_more$Longitude), ] # remove any NA

geom_map_more1 <- geo_map_more %>% 
  filter(level %in% c("NRH", "RRH","Hospital","HC IV","HC III", "HC II", "HCIII","HCII","HC_III","HC_II","HCIV")) %>%
  rowwise() %>%
  mutate(
    level1 = switch(  
      level,  
      "NRH"       = 7,  
      "RRH"       = 6,  
      "Hospital"  = 5,  
      "HC IV"     = 4, "HCIV"   = 4,
      "HC III"    = 3, "HC_III" = 3, "HCIII" = 3,
      "HC II"     = 2, "HC_II"  = 2, "HCII"  = 2
    ) 
  )
#covert dataframe to shapefile object: package sf required
map_more <- st_as_sf(geom_map_more1, coords = c("Longitude", "Latitude"), crs = 4326)

# example 6
pm  + tm_shape(map_more) + 
  tm_symbols( col = "ownership", 
              palette = 'YlOrRd', 
              size = "level1",
              sizes.legend = c(2,3,4,5,6,7),
              scale=1
  )


