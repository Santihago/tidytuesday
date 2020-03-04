# TidyTuesday Week 9
# Author: Santiago Muñoz Moldes
# Date: March 2nd, 2020

# Dataset information: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-25/readme.md
# Dataset source: https://github.com/WSJ/measles-data

library(tidyverse)
library(ggtext)

#load the dataset
measles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv',
                    na = c("-1", "null", "NA"))

# DATA WRANGLING

#some schools have repeated data under different locations
d1 <- measles %>% 
  distinct(name, city, state, county, enroll, mmr, overall, type, xrel, xmed, xper)

#see how many schools there are for each type
# d1 %>% group_by(type) %>% summarise(n = n())
# A tibble: 7 x 2
# type             n
# <chr>        <int>
# 1 BOCES           47
# 2 Charter        273
# 3 Kindergarten  1302
# 4 Nonpublic      166
# 5 Private       4910
# 6 Public       12539
# 7 NA           27174

#summarise the mmr vaccination rate for each school type
d2 <- d1 %>%
  group_by(type) %>%
  summarise(mmr_average = round(mean(mmr, na.rm = T))) 

# create a grid of 10x10
grid <- expand.grid(persons_x = 1:10, persons_y = 1:10)

#expand each school type with the grid
#this step removes the mmr_average column, so I add it back again
#to show vaccination rates on the grid, I set values as either 1 or 0
#I also remove some of the school types (BOCES and Nonpublic)
d3 <- d2 %>%
  expand(type, grid) %>%
  left_join(d2, by = "type") %>%  #re-add the lost column
  group_by(type) %>%
  mutate(vaccinated = c(rep(1, each=mmr_average), rep(0, each=(100-mmr_average)))) %>%
  filter(!type %in% c('BOCES', "Nonpublic", NA))  # remove some school types

# VISUALISATION

#set some colors
col_unvacc <- '#D8CABF'
col_vacc <- '#2F302A'

#plot
p <- d3 %>%
  ggplot(aes(persons_x, persons_y, fill=factor(vaccinated))) + 
  geom_tile(colour='white') +  #this sets the borde color between tiles
  facet_wrap(type~.) +
  theme_minimal() +
  scale_fill_manual(values=c(col_unvacc, col_vacc)) + 
  # Add title, subtitle and caption
  labs(
    title = '**Unvaccinated children in the USA**',
    subtitle = "Children in charter and private schools are 
    <b style='color:#a39890'>less protected</b> against <br/>
    <b>measles, mumps & rubeolla</b>, according to data from ~19.000 US<br/>schools",
    caption = 'Source: <i>The Wall Street Journal.</i><br/> Vaccination rates were 
    obtained from 12.539 public, 4.910 private,<br/> 1.302 kindergarten and 273 
    charter schools. 27.174 other schools<br/>were missing school type information') +
  # Customize text size, grid lines, etc.
  theme_minimal() + 
  theme(
    text=element_text(family='Helvetica Neue'),
    plot.title = element_markdown(size = 12, color = '#333333'),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 6, color = '#333333'),
    plot.caption = element_markdown(size = 5, color = '#333333'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill="white", color = NA), ##D7CDCB
    #strip.background=element_rect(fill="#D7CDCB"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank( ),
    legend.position = 'none',
    panel.border = element_blank(),
    aspect.ratio = 1 
  ) + 
  NULL

p

filename <- '/Users/santiago/Dropbox/MyCode/tidytuesday/imgs/tidytuesday-2020-02-week-9.png'
ggsave(filename, 
       plot = p, 
       scale = 1, 
       width = 10, 
       height = 10, 
       units = 'cm', 
       dpi = 600)