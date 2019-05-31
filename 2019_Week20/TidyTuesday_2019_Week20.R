#TidyTuesday 2019 - Week 19
#By: Dewi Koning
#Github: https://github.com/KoningD
#Twitter: https://twitter.com/DewiKoning

#load libraries 
library(tidyverse)
library(magrittr)
library(ggmap)
library(viridis)

#load data
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

#clean names 
colnames(coast_vs_waste) <- c("Country", "Country_Code", "Year", "Mismanaged_Waste_Tonnes", "Coastal_Pop", "Total_Pop")
colnames(mismanaged_vs_gdp) <- c("Country", "Country_Code", "Year", "Mismanaged_Waste_Kg_Pp_Pd", "GDP", "Total_Pop")
colnames(waste_vs_gdp) <- c("Country", "Country_Code", "Year", "Per_Capita_Waste_Kg_Pp_Pd", "GDP", "Total_Pop")

#get world map and join with dataset
world_map <- map_data("world")

#look at relationship between mismanaged waste and waste per capita

waste_vs_gdp_2010 <- waste_vs_gdp %>% filter(Year == 2010)
mismanaged_vs_gdp_2010 <- mismanaged_vs_gdp %>% filter(Year == 2010)

#make new dataset which combines the waste per capita and mismanagement per capita
generated_vs_mismanaged <- left_join(waste_vs_gdp, mismanaged_vs_gdp) %>% 
  filter(Year == 2010) %>% #there is only data for 2010, so filter out this year
  select(c("Country", "Country_Code", "Mismanaged_Waste_Kg_Pp_Pd","Per_Capita_Waste_Kg_Pp_Pd", "GDP")) %>% 
  filter(!is.na(Mismanaged_Waste_Kg_Pp_Pd) & !is.na(Per_Capita_Waste_Kg_Pp_Pd)) %>% 
  mutate(perc_mismanaged = (Mismanaged_Waste_Kg_Pp_Pd/Per_Capita_Waste_Kg_Pp_Pd)*100) %>%
  mutate(Country = ifelse(Country == "United States", "USA", Country)) %>% #both the US and the UK are coded differently, in order to be able to join, changed the name
  mutate(Country = ifelse(Country == "United Kingdom", "UK", Country)) %>% 
  right_join(world_map, by = c("Country" = "region"))

#make a df with the 3 countries with the highest levels of mismanagement of plastic waste, in order to plot these as geom_label later
generated_vs_mismanaged_top_n <- generated_vs_mismanaged %>% 
  select(c("Country", "perc_mismanaged")) %>% 
  unique() %>% 
  filter(!is.na(perc_mismanaged)) %>% 
  top_n(3, perc_mismanaged) %>%
  left_join(world_map, by = c("Country" = "region")) %>% 
  group_by(Country) %>% 
  top_n(1, lat) %>% 
  mutate(label_text = paste0(Country, ": ", round(perc_mismanaged), "%"))

#make a df with the 3 countries with the lowest levels of mismanagement of plastic waste, in order to plot these as geom_label later
generated_vs_mismanaged_bottom_n <- generated_vs_mismanaged %>% 
  select(c("Country", "perc_mismanaged")) %>% 
  unique() %>% 
  filter(!is.na(perc_mismanaged)) %>% 
  top_n(-3, perc_mismanaged) %>%
  left_join(world_map, by = c("Country" = "region")) %>% 
  group_by(Country) %>% 
  top_n(1, lat) %>% 
  mutate(label_text = paste0(Country, ": ", round(perc_mismanaged), "%"))



#generate plot 
generated_vs_mismanaged %>%
  ggplot(aes(x=long, y = lat, group = group, fill = perc_mismanaged)) + 
  geom_polygon(color = "lightyellow1", size = 0.1) +
  scale_fill_viridis(option = "E") +
  theme(text = element_text(colour = "darkblue", family = "mono"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20),
        plot.background=element_rect(fill = "lightyellow1"),
        panel.background = element_rect(fill = 'lightyellow1'),
        legend.background = element_rect(fill = "lightyellow1"),
        legend.position = "bottom"
  ) +
  labs(title = "Plastic polution: how do countries handle the disposal of their plastic waste?",
       subtitle = ~ atop("The map shows the percentage of inadequately disposed plastic waste per country in the year 2010.                                                                                ",
                         "The countries highlighted in light blue have the lowest percentage of mismanaged plastic waste, the countries highlighted in orange have the highest percentage of mismanagement."),
       caption = "By Dewi Koning for TidyTuesday - Week 20 2019 - Data: Our World in Data", 
       fill = "Percentage Mismanaged Plastic Waste") +
  geom_label(data = generated_vs_mismanaged_bottom_n, aes(x=long, y = lat, label = label_text, group = group), fill = "lightblue", fontface = "bold", nudge_x = 12) + 
  geom_label(data = generated_vs_mismanaged_top_n, aes(x=long, y = lat, label = label_text, group = group), fill = "orange", fontface = "bold", nudge_y = 2)


ggsave("./2019_Week20/Mismanaged_Plastic_Waste.png")
