### Tidy Tuesday Assignment #3 using EU Student mobility data ###
### Created by: Jocelyn Martinez Rico ###
### Updated on: 2022-03-08 ###

### Load Libraries ###
library(tidyverse)
library(here)
library(rcartocolor)
library(showtext)
library(ggplot2)
library(dplyr)

### Loading fonts ###
font_add(family = "baskerville", regular = "Baskerville.ttc")
showtext_auto()

### Load data ###
erasmus <-read_csv(here("Tidy_Tuesday_3","data", "erasmus.csv")) #reads in data
view(erasmus)
glimpse(erasmus)

### Data Analysis ###
erasmus %>%
  count(academic_year, sort = TRUE) %>% 
  arrange(desc(academic_year))

erasmus %>%
  count(participant_age, sort = TRUE) %>%
  arrange(desc(participant_age))

eu_student <- erasmus %>%
  filter(academic_year > 2018, #selects 2018 and on
         participant_gender != "Undefined", #removes undefined
         participant_nationality %in% c("FR","UK","IE","SE","DE", "ES", "IT"), #selects countries
         participant_age < 50, #selects ages under 50
         participant_age > 10) %>% #selects ages over 10
  select(project_reference, academic_year, participant_nationality, receiving_organization, participant_gender, participant_age) #selects variables
view(eu_student)

eu_student %>%
  count(participant_age, sort = TRUE) %>%
  arrange(participant_age)

### Plotting the data ###
eu_student %>%
  ggplot(aes(x = participant_nationality,
             y = participant_age, 
             fill = participant_nationality))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x",start=0)+
  scale_y_continuous(breaks = seq(10, 50, 10), #reorders y axis in increments of 10 (low, high, limit)
                     limits=c(10, 50))+ #creates limit for axis
  facet_grid(academic_year ~ participant_gender)+ #creates sections
  labs(x = "Participant Nationality", #defines x axis
       y = "Participant Age", #defines y axis
       title = "Erasmus Participant Demographics Between 2018-2020", #adds title
       fill = "Participant Nationality", #changes legend title
       caption = "Source: 2022-03-08 TidyTuesday Data Provided By Data.Europa hattip to Data is Plural")+ #adds caption
  theme_bw()+ #b/w background
  theme(axis.title = element_text(size = 15), #changes axis title size
        plot.title = element_text(size = 20), #changes title size
        text = element_text(family = "baskerville"))+ #adds font
  scale_color_carto_d(name = "species", palette = "Safe")+ #adds color to fill
  scale_fill_discrete(name = "Participant Nationality",
                      labels = c("Germany","Spain","France","Ireland","Italy","Sweden","United Kingdom")) #changes legend labels 


ggsave(here("Tidy_Tuesday_3/output","eu_student.png"), width = 7, height = 6) #exports plot
