#TidyTuesday 2019 - Week 25
#By: Dewi Koning
#Github: https://github.com/KoningD
#Twitter: https://twitter.com/DewiKoning

# load libraries -----------------------------------------------------------
library(tidyverse)
library(gganimate)

# load data ----------------------------------------------------------------
bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

#count amount of birds total per year -----------------------------------------------
total_bird_counts <- bird_counts %>% group_by(year) %>% tally(how_many_counted)

total_bird_counts %>% ggplot(aes(x=year, y = n)) + 
  geom_line(size = 0.7, colour = "darkblue") +
  geom_point(size = 2, colour = "lightgreen") +
  transition_reveal(year) + 
  theme_minimal() +
  labs(title = "Total amount of birds counted since 1921", 
       subtitle = "Birds heard or seen every year around Christmas in the Hamilton area of Ontario",
       caption = "Data by Bird Studies Canada | Hamilton area of Ontario - Plot by Dewi Koning - Tidytuesday 2019-Week 25") +
  ylab("Number of birds spotted")

anim_save("./2019_Week25/gif_bird_counts.gif")


#plot for selected birds 

birds_more_than_100 <- bird_counts %>% group_by(species) %>% tally(how_many_counted) %>% filter(n > 10)

#selection purely based on names sounding funny
birds_selection <- c("Black-capped Chickadee", "Bufflehead", "Rock Pigeon")

bird_counts_selection <- bird_counts %>% filter(species %in% birds_selection)

bird_counts_selection %>% ggplot(aes(x=year, y = how_many_counted, group = species, colour = species)) + 
  geom_line(size = 0.7) +
  geom_point(size = 2) +
  geom_segment(aes(xend = 2017, yend = how_many_counted), linetype = 2, colour = 'grey') + 
  geom_text(aes(x = 2017, label = species), hjust = 0) +
  transition_reveal(year)+
  theme_minimal() + 
  labs(title = "Spottings of selected birds with funny names since 1921", 
       subtitle = "Birds heard or seen every year around Christmas in the Hamilton area of Ontario", 
       caption = "Data: Bird Studies Canada | Hamilton area of Ontario - Plot: Dewi Koning - Tidytuesday 2019-Week 25") +
  ylab("Number of birds spotted") + 
  xlab("Year") +
  facet_grid(species~.) + 
  theme(legend.position = "none",
        plot.margin = margin(5.5, 120, 5.5, 5.5),
        strip.background = element_blank(), 
        strip.text = element_blank(), 
        plot.caption = element_text(hjust = 0)) + 
  coord_cartesian(clip = 'off')


anim_save("./2019_Week25/selected_birds.gif")


