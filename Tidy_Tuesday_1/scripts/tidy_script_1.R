### Tidy Tuesday #1 assignment using freedom data ###
### Created by: Jocelyn Martinez Rico ###
### Updated on: 2022-02-26 ###

### Load Libraries ###
library(tidyverse)
library(here)
library(beyonce)
library(ggplot2)
library(showtext)

### Loading fonts ###
font_add(family = "baskerville", regular = "Baskerville.ttc")

showtext_auto()

### Load data ###
FreeData <-read_csv(here("Tidy_Tuesday_1","data", "freedom.csv")) #reads in data

view(FreeData)
glimpse(FreeData)

### Data analysis ###
Freedom <- FreeData %>% #change data script to view isolated variables properly
  select(country, year, PR, CL) %>% #selects variables for analysis
  filter(country == "Mexico" | country == "United States of America" | country == "Colombia") %>% #isolates specific data by country
  filter(year > 2000) #isolates specific years

view(Freedom)

Freedom %>%
  ggplot(data = Freedom, mapping = aes(x = year, y = PR,
             group = country,
             fill = country)) +
  geom_violin()+ #graph used to view distribution 
  scale_fill_manual(values = beyonce_palette(129))+ #adds color
  labs(x = "Year", #defines x axis
       y = "Political Rights", #defines y axis
       title = "Political Rights from 2000s to Present Day", #adds title
       subtitle = "Data in regards to Colombia, Mexico, & USA", #adds subtitle
       caption = "Source: 2022-02-22 TidyTuesday Data")+ #adds caption
  theme_bw()+ #makes background white and adds grid lines
  theme(axis.title = element_text(size = 15), #changes axis title size
        plot.title = element_text(size = 20), #changes title size
        text = element_text(family = "baskerville")) #adds font

ggsave(here('Tidy_Tuesday_1/output',"Free_CMU_plot.png"), width = 7, height = 6)





