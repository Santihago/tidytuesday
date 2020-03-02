
# TidyTuesdat Week 9

# The data this week comes from The Wallstreet Journal. They recently published 
# an article around 46,412 schools across 32 US States.
# 
# "This repository contains immunization rate data for schools across the U.S.,
# as compiled by The Wall Street Journal. The dataset includes the overall and 
# MMR-specific vaccination rates for 46,412 schools in 32 states. As used in 
# “What’s the Measles Vaccination Rate at Your Child’s School?“.
# 
# Vaccination rates are for the 2017-18 school year for Colorado, Connecticut, 
# Minnesota, Montana, New Jersey, New York, North Dakota, Pennsylvania, South 
# Dakota, Utah and Washington. Rates for other states are 2018-19."

library(tidyverse)
library(ggtext)

#load the dataset
measles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv',
                    na = c("-1", "null", "NA"))
#comments: dataset looks quite dirty
#comments: missing data appears to be coded as either `-1`, `null` and `NA`
#comments: `xrel` column appears to have both logical and numbers
#comments: `overall` also has problems
#comments: not clear whether some schools are repeated or not

#some schools have repeated data under different locations
#remove repetitions
d1 <- measles %>% 
  distinct(name, city, state, county, enroll, mmr, overall, type, xrel, xmed, xper)

#see how many schools for each type
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

d2 <- d1 %>%
  group_by(type) %>%
  summarise(mmr_average = round(mean(mmr, na.rm = T))) 

# create a grid of "100 people" per school type
grid <- expand.grid(persons_x = 1:10, persons_y = 1:10)

d3 <- d2 %>%
  expand(type, grid) %>%
  left_join(d2, by = "type") %>%  #re-add the lost column
  group_by(type) %>%
  mutate(vaccinated = c(rep(1, each=mmr_average), rep(0, each=(100-mmr_average)))) %>%
  filter(!type %in% c('BOCES', "Nonpublic", NA))  # remove some school types

#colors
col_unvacc <- '#D8CABF'
col_vacc <- '#2F302A'

#plot
p <- d3 %>%
  ggplot(aes(persons_x, persons_y, fill=factor(vaccinated))) + 
  geom_tile(colour='white') +
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
  # Adjust text placement, text size, grid lines, etc.
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





