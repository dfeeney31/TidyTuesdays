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
penguinDat$sex <- as.factor(penguinDat$sex)
#Use Mice (Multivariate Imputation by Chained Equations) to impute missingness
miceMod <- mice(penguinDat, method="rf")  # perform mice imputation, based on random forests.
penguinOut <- complete(miceMod)  # generate the completed data.

#Similar to Iris data, they seem to be separated by flipper length and body mass (at least Gentoo and the others.)
# Males seem to have slightly longer flipper lengths and are heavier
ggplot(mapping = aes(x = body_mass_g, y = flipper_length_mm, colour = species), data = penguinOut) +
  geom_point() + facet_wrap(~ sex)


# Does mass differ by island for a given penguin? Not really. Only the Adelie penguins come frmo different islands anyway
penguinOut %>%
  group_by(species, island, sex) %>%
  summarize(
    avgMass = mean(body_mass_g)
  )


#Are there things that better differentiate Adelle and Chinstrap?

# PCA ---------------------------------------------------------------------
#prepare by scaling
library(ggfortify)
library(magick)
library(cowplot)
penguinScaled <- scale(penguinOut[,3:6])

penguinPCA <- prcomp(penguinScaled)
penguinPCA
summary(penguinPCA)

pcaPlot <- autoplot(penguinPCA, data = penguinOut, colour = 'species')

ggdraw() +
  draw_plot(pcaPlot) +
  draw_image("C:/Users/Daniel.Feeney/Documents/tidyTuesdays/Chinstrap-penguin.JPG",  x = -0.3, y = -0.3, scale = .2) +
  draw_image("C:/Users/Daniel.Feeney/Documents/tidyTuesdays/adelie.JPG", x = -0.2, y = 0.4, scale = .15) +
  draw_image("C:/Users/Daniel.Feeney/Documents/tidyTuesdays/gentoo.JPG", x = 0.27, y = 0.35, scale = .25) 