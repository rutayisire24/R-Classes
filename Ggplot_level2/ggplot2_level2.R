
## load ggplot  in tidyverse

library(tidyverse) 
library(readr)




#load_data

chic <- readr::read_csv("https://cedricscherer.com/data/chicago-nmmaps-custom.csv")

#explore the data
glimpse(chic)

head(chic,10)


## set theme 


theme_set(theme_bw())

## start plotting 
g <- ggplot(chic, aes(x = date, y = temp))

g

g + geom_point()

g + geom_line()

## combine the geoms ( chart types)

g + geom_line() + geom_point()

## customise the plot 
g + geom_point(color = "firebrick", shape = "diamond", size = 2)


## change labels 

plot  <- ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)")

## change the font size of each axis label  and placement 
## vjust refers to the vertical alignment, which usually ranges between 0 and 1

plot_labels <- plot + 
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15))
  
## change color of the all axis  labels 

plot_labels <- plot + 
  theme(axis.title = element_text(size = 15,color = 'firebrick', 
                                  face = "italic"))

#The face argument can be used to make the font bold or
# italic or even bold.italic.

## see link for the customising the color and font for the axis labels 

## rotate the axis text 
## Specifying an angle allows you to rotate any text elements. With hjust and vjust 
#you can adjust the position of the text afterwards horizontally (0 = left, 1 = right) and vertically (0 = top, 1 = bottom):

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12))


## removing axis titles

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = NULL, y = "")

## Note that NULL removes the element (similarly to element_blank()) while empty quotes "" will keep the spacing for 
## the axis title and simply print nothing.


## limit the range of the plots 

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  ylim(c(0, 50))

## read link for the difference with scale_y_continuous(limits = c(0, 50))

### -------------------

## working with titles

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)",
       title = "Temperatures in Chicago",
       subtitle = "Seasonal pattern of daily temperatures from 1997 to 2001",
       caption = "Data: NMMAPS",
       tag = "Fig. 1")
## showing some text on the next line 

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  ggtitle("Temperatures in Chicago\nfrom 1997 to 2001") +
  theme(plot.title = element_text(lineheight = .8, size = 16))

## working with legends 
ggplot(chic,
       aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)")


## chage positiom of legend 

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(legend.position = "top")

## insert the label inside the plot 

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)",
       color = NULL) +
  theme(legend.position = c(.15, .15),
        legend.background = element_rect(fill = "transparent"))


## first comprehesive plot 

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = NULL, y = "Temperature (°F)",
       title = "Temperatures in Chicago from 1997 to 2001",
       subtitle = "Daily temperatures in °F from 1997 to 2001",
       caption = "Visualisation ~ Meddy| Data ~ NMAPS",
       color = NULL) +
  theme(legend.position = c(.15, .15),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
## save the plot 
ggsave("second_plot.jpg", width = 9, height = 6)

        