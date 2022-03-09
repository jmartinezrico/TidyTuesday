### Tidy Tuesday Assignment #3 using EU Student mobility data ###
### Created by: Jocelyn Martinez Rico ###
### Updated on: 2022-03-08 ###

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
erasmus <-read_csv(here("Tidy_Tuesday_3","data", "erasmus.csv")) #reads in data
view(erasmus)
glimpse(erasmus)

### Data Analysis ###
erasmus %>%
  count(academic_year, sort = TRUE) %>% 
  arrange(desc(academic_year))

eu_student <- erasmus %>%
  filter(academic_year > 2018) %>% #selects 2018 and on
  select(project_reference, academic_year, participant_nationality, participant_gender, participant_age) #selects variables
view(eu_student)

### Plotting the data ###
eu_student %>%
  ggplot(aes(x = academic_year,
             y = participant_age,
             fill = participant_nationality))+
  geom_boxplot()+
  facet_wrap(~ participant_gender)+
  labs(x = "Academic Year", #defines x axis
       y = "Participant Age", #defines y axis
       title = "Erasmus Participant Ages Between 2018-2020", #adds title
       caption = "Source: 2022-03-08 TidyTuesday Data Provided By Data.Europa hattip to Data is Plural")+ #adds caption
  guides(fill = FALSE)+
  theme_bw()+
  theme(axis.title = element_text(size = 15), #changes axis title size
        plot.title = element_text(size = 20), #changes title size
        text = element_text(family = "baskerville")) #adds font

ggsave(here('Tidy_Tuesday_3/output',"eu_student.png"), width = 7, height = 6) #exports plot
