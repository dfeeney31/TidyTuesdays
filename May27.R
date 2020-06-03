library(tidyverse)
library(ggridges)

rm(list=ls())
BC <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')


# Basic look at the data  --------------------------------------------------------------
BC

BC %>%
  group_by(ingredient) %>%
  summarize(
    nObs = n()
    )

BC %>%
  group_by(name, category) %>%
  summarize(
    nIngred = n()
  ) %>%
  ggplot(mapping = aes(nIngred, fill = category )) + geom_histogram(stat="count") + 
  facet_wrap(~ category)


BC %>%
  group_by(name, category) %>%
  summarize(
    nIngred = n()
  ) %>%
  ggplot(mapping = aes(x = nIngred, y = category, fill = category )) + geom_density_ridges()


# Figure out volume of cocktails ------------------------------------------


BC %>% count(measure, sort = TRUE)
# replace all of the fractions with decimals (taken from Hadley Wickham's YouTube on parsing the text)
sizes <- BC %>%
  filter(str_detect(measure, "oz")) %>%
  mutate(oz = str_replace(measure, " oz", "")) %>%
  mutate(oz = str_replace(oz, " ?1/2", "0.5")) %>%
  mutate(oz = str_replace(oz, " ?1/4", "0.25")) %>%
  mutate(oz = str_replace(oz, " ?3/4", "0.75")) %>%
  mutate(oz = str_replace(oz, " ?2/3", "0.33")) %>%
  mutate(oz = str_replace(oz, " ?2/3", "0.66")) 

 