library(tidyverse)

rm(list=ls())
BC <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')
head(BC, 10)

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
  ggplot(mapping = aes(nIngred, fill = category)) + geom_histogram(stat="count")


