### Tidy Tuesday Assignment #2 working with Alternative Fueling Stations Data ###
### Created by: Jocelyn Martinez Rico ###
### Updated on: 2022-03-01 ###

### Load Libraries ###
library(tidyverse)
library(here)
library(beyonce)
library(showtext)
library(ggplot2)
library(dplyr)

### Loading fonts ###
font_add(family = "baskerville", regular = "Baskerville.ttc")
showtext_auto()

### Load data ###
stations <-read_csv(here("Tidy_Tuesday_2","data", "stations.csv")) #reads in data
view(stations)

### Filtering the data ###
altfuel_data <- stations %>%
  filter(STATE == c("CA","NY","TX","PA","FL")) %>% #filtering out top 5 most populous states
  select(LATITUDE, LONGITUDE, FUEL_TYPE_CODE, CITY, ZIP, STATE) #selects variables

view(altfuel_data)

### Plotting the data ###
altfuel_data %>%
  ggplot(aes(x = STATE, 
             y = LATITUDE,
             fill = FUEL_TYPE_CODE,
             color = FUEL_TYPE_CODE))+
  geom_boxplot(outlier.colour = NA)+
  labs(x = "State", #defines x axis
       y = "Latitude", #defines y axis
       title = "Most Used Alternative Fuel Types", #adds title
       subtitle = "Data in regards to top 5 US states", #adds subtitle
       caption = "Source: 2022-03-01 TidyTuesday Data Provided By US DOT")+ #adds caption)
  scale_fill_manual(values = beyonce_palette(90))+ #adds color
  scale_color_manual(values = beyonce_palette(98))+ #adds color to outline
  theme_bw()+
  theme(axis.title = element_text(size = 15), #changes axis title size
        plot.title = element_text(size = 20), #changes title size
        text = element_text(family = "baskerville")) #adds font

ggsave(here('Tidy_Tuesday_2/output',"alt_fuel.png"), width = 7, height = 6) #exports plot
