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


