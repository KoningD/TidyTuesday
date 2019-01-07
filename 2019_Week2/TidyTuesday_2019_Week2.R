#TidyTuesday 2019 - Week 2
#By: Dewi Koning
#Github: https://github.com/KoningD
#Twitter: https://twitter.com/DewiKoning

#load libraries 
library(tidyverse)
library(tidytext)
library(magrittr)

#load data
df_original <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

#average rating per genre
df <- df_original %>% select(c("av_rating", "genres"))
df %<>% separate(genres, into = c("genre1", "genre2", "genre3"),sep = ",")
df_table <- gather(df, type, genre, 2:4, na.rm = TRUE) %>% group_by(genre) %>% summarize(Average_Rating = mean(av_rating)) %>% arrange(desc(Average_Rating))

df_table %>%
  ggplot(aes(reorder(genre, Average_Rating), Average_Rating)) + 
  geom_col(fill = "lightblue") + 
  theme_classic() + 
  coord_flip() +
  labs(title = "Average Rating of TV Shows per Genre", 
       x = "  ",
       y = "Average Rating", 
       caption = "Data from The Economist") + 
  scale_y_continuous(expand = c(0,0))


