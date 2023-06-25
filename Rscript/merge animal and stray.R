
# library
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(scales)
library(extrafont)

setwd("C:/Users/ccdfg/OneDrive/桌面/R_Final")

# stray
stray <- read_excel("Data/stray.xlsx", sheet = "109", col_names = FALSE, skip = 1)
names(stray)[1] = "City"
names(stray)[2] = "Population"
names(stray)[3] = "StrayEstimate"
stray <- stray[c(1:3)]
#View(stray)

# animal
animal <- read_excel("Data/animal.xlsx", sheet = "109", col_names = FALSE, skip = 2)
names(animal)[1] = "City"
names(animal)[2] = "AnimalNumber"
names(animal)[3] = "AdoptionNumber"
names(animal)[4] = "AdoptionRate"
names(animal)[5] = "EuthanasiaNumber"
names(animal)[6] = "EuthanasiaRate"
names(animal)[7] = "DeathNumber"
names(animal)[8] = "DeathRate"
#View(animal)


# merge
strayAnimal <- merge(stray, animal, by = "City")
#View(strayAnimal)

# output
write_xlsx(strayAnimal, "Data/strayAnimal109.xlsx")


