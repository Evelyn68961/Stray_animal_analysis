
# library
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(scales)
library(extrafont)


merge_stray_and_animal <- function(year){
  # stray data
  sheet_name <- as.character(year)
  stray <- read_excel("Data/stray.xlsx", sheet = sheet_name, col_names = FALSE, skip = 1)
  names(stray)[1] = "City"
  names(stray)[2] = "Population"
  names(stray)[3] = "StrayEstimate"
  stray <- stray[c(1:3)]
  
  # animal
  animal <- read_excel("Data/animal.xlsx", sheet = sheet_name, col_names = FALSE, skip = 2)
  names(animal)[1] = "City"
  names(animal)[2] = "AnimalNumber"
  names(animal)[3] = "AdoptionNumber"
  names(animal)[4] = "AdoptionRate"
  names(animal)[5] = "EuthanasiaNumber"
  names(animal)[6] = "EuthanasiaRate"
  names(animal)[7] = "DeathNumber"
  names(animal)[8] = "DeathRate"
  
  # merge
  strayAnimal <- merge(stray, animal, by = "City")
  strayAnimalWithYear <- cbind(strayAnimal, Year=year)
  return(strayAnimalWithYear)
}

combine_strayAnimal_in_year <- function(year_list){
  empty <- 1
  strayAnimalCombined = 0
  for(year in year_list){
    strayAnimalCurrentYear <- merge_stray_and_animal(year)
    if(empty == 0){
      strayAnimalCombined = strayAnimalCurrentYear
      empty <- 1
    }
    else{
      strayAnimalCombined <- rbind(strayAnimalCombined,strayAnimalCurrentYear)
    }
  }
  return(strayAnimalCombined)
}
animalStray <- combine_strayAnimal_in_year(list(104, 107, 109))
animalStray <- animalStray[-1, ]
View(animalStray)


# output
write_xlsx(animalStray, "Data/animalStray.xlsx")


