library(tidyverse) # includes ggplot2 and other data management tools
library(readxl)    # read excel files 
library(janitor)   # cleaning and summary tables
library(ggforce)   # ggplot extras
library(here)      # file locator
library(stringr)   # working with characters 

# current project folder
here()

# create path to data file and assign to object dataPath
dataPath <- here("data","linelist_cleaned.xlsx")

# show data file path
dataPath

# import data
linelist <- read_excel( dataPath )

# This will create plot that is a blank canvas
ggplot(data = linelist)

# create point graph age vs wt_kg
ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+
  geom_point()

# create histogram to show distribution of age
ggplot(data = linelist, mapping = aes(x = age))+
  geom_histogram()


# scatterplot with green dots. 
# Note that the color is NOT in the aes when we want a uniform color

ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+  # set data and axes mapping
  geom_point(color = "darkgreen", size = 0.5, alpha = 0.2)         # set static point aesthetics

# histogram
# we specify a uniform fill color and uniform border color
# AGAIN, this is out side the eas

ggplot(data = linelist, mapping = aes(x = age))+       # set data and axes
  geom_histogram(              # display histogram
    binwidth = 7,                # width of bins
    color = "red",               # bin line color
    fill = "blue",               # bin interior color
    alpha = 0.1)                 # bin transparency

# We include the color in the eas when we want coloring based on variable

# This is what we call scaling - Adjusting attributes based on values
# scatterplot
ggplot(data = linelist,     # set data
       mapping = aes(       # map aesthetics to column values
         x = age,           # map x-axis to age            
         y = wt_kg,         # map y-axis to weight
         color = age)       # map color to age
      ) +     
      geom_point()         # display data as points 

# scatterplot
ggplot(data = linelist,     # set data
       mapping = aes(       # map aesthetics to column values
         x = age,           # map x-axis to age            
         y = wt_kg,         # map y-axis to weight
         color = age,       # map color to age
         size = age)        # map size to age
       ) +                  
      geom_point(             # display data as points
        shape = "diamond",      # points display as diamonds
        alpha = 0.3)            # point transparency at 30%

# add line

ggplot(data = linelist,
       mapping = aes(           # map aesthetics to columns
         x = age,
         y = wt_kg,
         color = age_years)
      ) + 
      geom_point(                   # add points for each row of data
                size = 1,
                alpha = 0.5
                ) +  
      geom_smooth(                  # add a trend line 
                method = "lm",              # with linear method
                size = 2)                   # size (width of line) of 2

# Where to make mapping assignments

# Mapping assignments made in the top ggplot() command will be inherited as defaults across any geom below, like how x = and y = are inherited
# Mapping assignments made within one geom apply only to that geom

# These commands will produce the exact same plot
ggplot(data = linelist, mapping = aes(x = age))+
  geom_histogram()

ggplot(data = linelist)+
  geom_histogram(mapping = aes(x = age))

ggplot()+
  geom_histogram(data = linelist, mapping = aes(x = age))

# Groups
ggplot(data = linelist,
       mapping = aes(x = age, y = wt_kg, color = gender))+
  geom_point(alpha = 0.5)


# This alternative code produces the same plot
ggplot(data = linelist,
       mapping = aes(x = age, y = wt_kg)
       ) +
  geom_point( mapping = aes(color = gender), alpha = 0.5)


# faceting

# import data

mal_data_path <- here("data","malaria_facility_count_data.rds")
mal_data_path
malaria_data <- readRDS(mal_data_path)

# A plot with facets by district
ggplot(malaria_data, aes(x = data_date, y = malaria_tot)) +
  geom_col(width = 1, fill = "darkred") +       # plot the count data as columns
  theme_minimal()+                              # simplify the background panels
  labs(                                         # add plot labels, title, etc.
    x = "Date of report",
    y = "Malaria cases",
    title = "Malaria cases by district") +
  facet_wrap(~District)                       # the facets are created


# assigning plots to objects

# define plot
age_by_wt <- ggplot(data = linelist, mapping = aes(x = age_years, y = wt_kg, color = age_years))+
  geom_point(alpha = 0.1)

# print
age_by_wt

# Modifying saved plots
age_by_wt+
  geom_vline(xintercept = 50)

# Labels

# Within labs() you can provide character strings to these arguements:
#   
#   x = and y = The x-axis and y-axis title (labels)
#   title = The main plot title
#   subtitle = The subtitle of the plot, in smaller text below the title
#   caption = The caption of the plot, in bottom-right by default

age_by_wt <- ggplot(
  data = linelist,   # set data
  mapping = aes(     # map aesthetics to column values
    x = age,           # map x-axis to age            
    y = wt_kg,         # map y-axis to weight
    color = age))+     # map color to age
  geom_point()+           # display data as points
  labs(
    title = "Age and weight distribution",
    subtitle = "Fictional Ebola outbreak, 2014",
    x = "Age in years",
    y = "Weight in kilos",
    color = "Age",
    caption = stringr::str_glue("Data as of {max(linelist$date_hospitalisation, na.rm=T)}"))

age_by_wt

# Themes

ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+  
  geom_point(color = "darkgreen", size = 0.5, alpha = 0.2)+
  labs(title = "Theme classic")+
  theme_classic()

ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+  
  geom_point(color = "darkgreen", size = 0.5, alpha = 0.2)+
  labs(title = "Theme bw")+
  theme_bw()

ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+  
  geom_point(color = "darkgreen", size = 0.5, alpha = 0.2)+
  labs(title = "Theme minimal")+
  theme_minimal()

ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+  
  geom_point(color = "darkgreen", size = 0.5, alpha = 0.2)+
  labs(title = "Theme gray")+
  theme_gray()

# Modify theme

age_by_wt + 
  theme_classic()+                                 # pre-defined theme adjustments
  theme(
    legend.position = "bottom",                    # move legend to bottom
    
    plot.title = element_text(size = 30),          # size of title to 30
    plot.caption = element_text(hjust = 0),        # left-align caption
    plot.subtitle = element_text(face = "italic"), # italicize subtitle
    
    axis.text.x = element_text(color = "red", size = 15, angle = 90), # adjusts only x-axis text
    axis.text.y = element_text(size = 15),         # adjusts only y-axis text
    
    axis.title = element_text(size = 20)           # adjusts both axes titles
  )     

# Plot continuous data

# A) Regular histogram
ggplot(data = linelist, aes(x = age))+  # provide x variable
  geom_histogram()+
  labs(title = "A) Default histogram (30 bins)")

# B) More bins
ggplot(data = linelist, aes(x = age))+  # provide x variable
  geom_histogram(bins = 50)+
  labs(title = "B) Set to 50 bins")

# C) Fewer bins
ggplot(data = linelist, aes(x = age))+  # provide x variable
  geom_histogram(bins = 5)+
  labs(title = "C) Set to 5 bins")


# D) More bins
ggplot(data = linelist, aes(x = age))+  # provide x variable
  geom_histogram(binwidth = 1)+
  labs(title = "D) binwidth of 1")

# Frequency with proportion axis, smoothed
ggplot(data = linelist, mapping = aes(x = age)) +
  geom_density(size = 2, alpha = 0.2)+
  labs(title = "Proportional density")

# Stacked frequency with proportion axis, smoothed
ggplot(data = linelist, mapping = aes(x = age, fill = gender)) +
  geom_density(size = 2, alpha = 0.2, position = "stack")+
  labs(title = "'Stacked' proportional densities")

# "Stacked" histogram
ggplot(data = linelist, mapping = aes(x = age, fill = gender)) +
  geom_histogram(binwidth = 2)+
  labs(title = "'Stacked' histogram")

# Frequency 
ggplot(data = linelist, mapping = aes(x = age, color = gender)) +
  geom_freqpoly(binwidth = 2, size = 2)+
  labs(title = "Freqpoly")

# Frequency with proportion axis
ggplot(data = linelist, mapping = aes(x = age, y = after_stat(density), color = gender)) +
  geom_freqpoly(binwidth = 5, size = 2)+
  labs(title = "Proportional freqpoly")

# Frequency with proportion axis, smoothed
ggplot(data = linelist, mapping = aes(x = age, y = after_stat(density), fill = gender)) +
  geom_density(size = 2, alpha = 0.2)+
  labs(title = "Proportional, smoothed with geom_density()")

# A) Overall boxplot
ggplot(data = linelist)+  
  geom_boxplot(mapping = aes(y = age))+   # only y axis mapped (not x)
  labs(title = "A) Overall boxplot")

# B) Box plot by group
ggplot(data = linelist, mapping = aes(y = age, x = gender, fill = gender)) + 
  geom_boxplot()+                     
  theme(legend.position = "none")+   # remove legend (redundant)
  labs(title = "B) Boxplot by gender")   


# remove NA from data (this modification will be covered in detail later)

linelist2 =  drop_na(linelist, outcome)

# A) Jitter plot by group
ggplot(data = linelist2,      # remove missing values
       mapping = aes(y = age,                     # Continuous variable
                     x = outcome,                           # Grouping variable
                     color = outcome))+                     # Color variable
  geom_jitter()+                                  # Create the violin plot
  labs(title = "A) jitter plot by gender")     



# B) Violin plot by group
ggplot(data = linelist2,       # remove missing values
       mapping = aes(y = age,                      # Continuous variable
                     x = outcome,                            # Grouping variable
                     fill = outcome))+                       # fill variable (color)
  geom_violin()+                                   # create the violin plot
  labs(title = "B) violin plot by gender")    



# A) Sina plot by group
ggplot(
  data = linelist2, 
  aes(y = age,           # numeric variable
      x = outcome)) +    # group variable
  geom_violin(
    aes(fill = outcome), # fill (color of violin background)
    color = "white",     # white outline
    alpha = 0.2)+        # transparency
  geom_sina(
    size=1,                # Change the size of the jitter
    aes(color = outcome))+ # color (color of dots)
  scale_fill_manual(       # Define fill for violin background by death/recover
    values = c("Death" = "#bf5300", 
               "Recover" = "#11118c")) + 
  scale_color_manual(      # Define colours for points by death/recover
    values = c("Death" = "#bf5300", 
               "Recover" = "#11118c")) + 
  theme_minimal() +                                # Remove the gray background
  theme(legend.position = "none") +                # Remove unnecessary legend
  labs(title = "B) violin and sina plot by gender, with extra formatting")      


# Two continuous variables

# Basic scatter plot of weight and age
ggplot(data = linelist, 
       mapping = aes(y = wt_kg, x = age))+
  geom_point() +
  labs(title = "A) Scatter plot of weight and age")

# modify datafile to remove name
linelist3 <-  drop_na(linelist, c(gender, outcome) )
# Scatter plot of weight and age by gender and Ebola outcome
ggplot(data = linelist3, # filter retains non-missing gender/outcome
       mapping = aes(y = wt_kg, x = age))+
  geom_point() +
  labs(title = "B) Scatter plot of weight and age faceted by gender and outcome")+
  facet_grid(gender ~ outcome) 

















