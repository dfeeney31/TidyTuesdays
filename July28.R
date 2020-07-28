rm(list=ls())
library(tidyverse)
library(mice)

penguinDat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

head(penguinDat)


penguinDat %>%
  group_by(species, island, sex) %>%
  summarize(
    avgBillLen = mean(bill_length_mm),
    avgBillDep = mean(bill_depth_mm),
    avgFlipLen = mean(flipper_length_mm),
    avgMass = mean(body_mass_g)
  )

#investigating the NA. Some NA exist across all measurements, which may be imputed based on year or removed.
#Other NA are just for the sex, and could be imputed by the other dependent variables
#use mice to evaluate where missingness exists
md.pattern(penguinDat)
#Use Mice (Multivariate Imputation by Chained Equations) to impute missingness
miceMod <- mice(penguinDat, method="rf")  # perform mice imputation, based on random forests.
penguinOut <- complete(miceMod)  # generate the completed data.
md.pattern(penguinOut) #missingness fixed except for sex

#Similar to Iris data, they seem to be separated by flipper length and body mass (at least Gentoo and the others.)
# Males seem to have slightly longer flipper lengths and are heavier
ggplot(mapping = aes(x = body_mass_g, y = flipper_length_mm, colour = species), data = penguinOut) +
  geom_point() + facet_wrap(~ sex)


# Does mass differ by island for a given penguin? Not really. Only the Adelie penguins come frmo different islands anyway
penguinDat %>%
  group_by(species, island) %>%
  summarize(
    avgMass = mean(body_mass_g)
  )


#Are there things that better differentiate Adelle and Chinstrap?