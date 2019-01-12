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


#boxplot rating per genre

df_boxplot <- gather(df, type, genre, 2:4, na.rm=TRUE) 

df_boxplot %>%
  ggplot(aes(genre, av_rating)) + 
  geom_boxplot() + 
  coord_flip()

#does it help to have multiple genres per serie? 
df %<>% mutate(number_of_genres = rowSums(!is.na(df[2:4])))
df$number_of_genres %<>% as.factor()

df %>% ggplot(aes(x=av_rating, fill = number_of_genres)) + geom_density(alpha = 0.5) + facet_grid(.~number_of_genres)

df %>% ggplot(aes(x=av_rating, x = number_of_genres)) + geom_point()


#which genres are often combined? 
df_table_combined_genres <- summary(as.factor(df_original$genres)) %>% as.data.frame()
df_table_combined_genres$genres <- row.names(df_table_combined_genres)
colnames(df_table_combined_genres) <- c("Amount", "Genres")
df_table_combined_genres %<>% select("Genres", "Amount")
df_table_combined_genres$Genres %<>% as.factor()

df_table_combined_genres %<>% top_n(10) %>% arrange(desc(Amount))

#Not really happy with this plot, trying to manually set a theme for the first time
df_table_combined_genres %>% 
  ggplot(aes(x=reorder(Genres, Amount), y = Amount)) + 
  geom_col(fill = "orange") + 
  coord_flip() + 
  theme(plot.background = element_rect(fill = "black"), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "black"), 
        text = element_text(colour = "white", size = 10), 
        axis.text = element_text(colour = "white", size = rel(1.2)))

#how do these combined genres score?
df_top_genres <- df_original %>% filter(genres %in% df_table_combined_genres$Genres)

df_top_genres %>% ggplot(aes(x=date, y=av_rating, colour = as.factor(genres))) + geom_jitter() + facet_wrap(as.factor(df_top_genres$genres))


#which genres are often in combination with other genres?
df_more_genres <- df_original %>% separate(genres, into = c("genre1", "genre2", "genre3"),sep = ",")
df_more_genres %<>% filter(!is.na(genre2)) #filter out all entries that only have one genre

df_final <- df_more_genres[1,]
j <- 1


for (i in 1:nrow(df_more_genres)) { 
  
  df_current <- df_more_genres[i,]
  
  if(is.na(df_current$genre3)){ #if there are only 2 genres, just copy the current row, put the genres in alphabetical order 
    df_final[nrow(df_final)+1,] <- NA
    df_final[j,] <- df_current
    genres <- c(df_final$genre1[j], df_final$genre2[j]) %>% sort() #sort the genres alphabetically 
    df_final$genre1[j] <- genres[1]
    df_final$genre2[j] <- genres[2]
    
    
    }
  
  if(!is.na(df_current$genre3)){ #if there are 3 genres, then make 3 rows with the combinations of genres as genre1 en genre2
    k <- j+1
    l <- j+2
    
    df_final[nrow(df_final)+3,] <- NA
    
    df_final[j:l,] <- df_current #genre 1 and 2
    genres <- c(df_final$genre1[j], df_final$genre2[j], df_final$genre3[j]) %>% sort() #sort the genres alphabetically 
    
    df_final$genre1[j:k] <- genres[1] #first two rows have the first genre
    df_final$genre2[j] <- genres[2] #first row is genre one and two
    df_final$genre2[k] <- genres[3] #second row is genre one and three 
    df_final$genre1[l] <- genres[2] #third row is genre two and three
    df_final$genre2[l] <- genres[3] #third row is genre two and three

         }
  j <- nrow(df_final) + 1
  }


df_final_subset <- df_final %>% count(genre1, genre2) %>% top_n(20) %>% arrange(desc(n))

graph_genres <- df_final %>% count(genre1, genre2) %>% unite("genre_combinations", c(genre1, genre2), sep = " ") %>% filter(n > 20) %>% graph_from_data_frame()


ggraph(graph_genres, layout = "fr") + 
  geom_edge_link() + geom_node_point() + geom_node_text(aes(label = name)) + theme_void()


#which genres have the highest rating?
