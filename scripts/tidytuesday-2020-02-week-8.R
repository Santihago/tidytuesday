library(tidyverse)
library(ggtext)

d <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# Recode and create food group 
d2 <- d %>%
  mutate(
    food_category = recode(food_category, 
                           'Milk - inc. cheese' = 'Dairy', 
                           'Wheat and Wheat Products' = 'Wheat',
                           'Nuts inc. Peanut Butter' = 'Nuts'),
    #food_category = factor(food_category, levels =  c('Nuts', 'Soybeans', 'Rice', 'Wheat', 'Dairy', 'Eggs', 'Fish', 'Poultry', 'Lamb & Goat', 'Pork', 'Beef')),
    food_group = case_when(food_category %in% c('Pork', 'Poultry', 'Beef', 'Lamb & Goat', 'Fish', 'Eggs', 'Dairy') ~ 'Animal',
                           food_category %in% c('Wheat', 'Rice', 'Soybeans', 'Nuts') ~ 'Non-Animal')
  )

# Order countries by total consumption per person, from lowest to highest
d3 <- d2 %>%
  group_by(country) %>%
  summarise(total_consumption = sum(consumption)) %>%
  arrange(total_consumption)

# Add the total_consumption info and ordering to the previous table
# To make the back-to-back barplot, make the non-animal values negative  
d4 <- d3 %>%
  left_join(d2, by = 'country') %>%
  mutate(consumption = ifelse(food_group=='Non-Animal', consumption * -1, consumption))

# Manual color selection
my_palette <- c('Beef' = "#A50026", 
                'Pork' = "#D73027", 
                'Lamb & Goat' = "#F46D43", 
                'Poultry' = "#ff9a42", 
                'Fish' = "#FDAE61",
                'Eggs' = "#f7cc74", 
                'Dairy' = "#ffe596", 
                'Wheat' = "#A6D96A", 
                'Rice' = "#66BD63", 
                'Soybeans' = "#1A9850", 
                'Nuts' = "#006837")

#colors <- c("#006837", "#1A9850", "#66BD63", "#A6D96A", "#ffe596", "#f7cc74", "#FDAE61", "#ff9a42", "#F46D43", "#D73027", "#A50026")
#levels <- c('Nuts', 'Soybeans', 'Rice', 'Wheat', 'Dairy', 'Eggs', 'Fish', 'Poultry', 'Lamb & Goat', 'Pork', 'Beef')

p <- ggplot(data=d4, aes(x=reorder(country, total_consumption), y=consumption, order=food_category)) +
  # To make the back-to-back barplot, I will plot separately the animal and non-animal data
  # For barplots, 'identity' stops default of counting cases to define bar height
  geom_bar(data=d4[which(d4$food_group=='Animal'),], 
           aes(fill=factor(food_category, levels = c('Beef',  'Pork',  'Lamb & Goat', 'Poultry', 'Fish', 'Eggs', 'Dairy'))), 
           position='stack', 
           stat='identity',
           width = .75)  +
  geom_bar(data=d4[which(d4$food_group=='Non-Animal'),], 
           aes(fill=factor(food_category, levels = c('Nuts', 'Soybeans', 'Rice', 'Wheat'))), 
           position='stack', 
           stat='identity',
           width = .75)  +
  coord_flip() +
  # Set equal scale length around 0
  scale_y_continuous(limits=c(-220,580), breaks = c(-200, 0, 200, 400, 600), labels = c(200, 0, 200, 400, 600)) +
  # Set fixed colors
  scale_fill_manual(values=my_palette) +
  # Add title, subtitle and caption
  labs(
    title = '**How much do you eat?**',
    subtitle = "Average <b>consumption</b> (in kilograms/person/year) <br/>of <b style='color:#006837'>plant-based</b>, <b style='color:#f2c94e'>dairy</b> and <b style='color:#A50026'>meat</b> products",
    caption = 'Source: nu3.de and the Food and Agriculture <br/>Organization of the United Nations (FAO), 2013. <br/>Consumption includes potential food waste.',
    fill = '',
    y = '**Consumption** (kg/person/year)') +
  # Adjust text placement, text size, grid lines, etc.
  theme(
    text=element_text(family='Helvetica Neue'),
    plot.title = element_markdown(size = 20, color = '#333333'),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 11, color = '#333333'),
    plot.caption = element_markdown(size = 7, color = '#333333'),
    axis.title.y = element_blank(),
    axis.title.x = ggtext::element_markdown(size = 9),
    axis.text=element_text(size=6),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill="white"),
    panel.grid.major.x = element_line(color='grey', size=.2),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = c(.8,.65),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
  ) + 
  NULL

p

#ggsave('/Users/santiago/Dropbox/MyCode/tidytuesday/2020/8/consumption.png', plot = p, scale = 1, width = 12, height = 26, units = 'cm', dpi = 900 )



