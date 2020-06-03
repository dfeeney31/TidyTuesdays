#### Tidy Tuesdays 6/30/20 ###
library(tidyverse)
rm(list=ls())

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')
marbles


# How many teams and marbles in the dataset -------------------------------
#32 marbles
length(unique(marbles$marble_name))
# on 16 teams 
length(unique(marbles$team_name))
#16 races
length(unique(marbles$date))


# Are any teams especially good? ------------------------------------------

marbles %>%
  group_by(race, team_name) %>%
  summarize(
    AvgTime = mean(avg_time_lap),
    SDTime = sd(avg_time_lap),
  ) %>%
  ggplot(mapping = aes(x = race, y = AvgTime)) +
    geom_point(stat="identity") +
    geom_errorbar(aes(ymin=AvgTime+SDTime, ymax=AvgTime-SDTime), width=0.2, size=1, color="blue") +
    facet_wrap(~ team_name)

