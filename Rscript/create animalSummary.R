
# library
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(scales)
library(extrafont)



merge_animal <- function(year){

  # animal
  sheet_name <- as.character(year)
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
  AnimalCombine <- cbind(animal, Year=year)
  
  return(AnimalCombine)
}

combine_animal_in_year <- function(year_list){
  empty <- 1
  Combined = 0
  for(year in year_list){
    AnimalCurrentYear <- merge_animal(year)
    if(empty == 0){
      Combined = AnimalCurrentYear
      empty <- 1
    }
    else{
      Combined <- rbind(Combined,AnimalCurrentYear)
    }
  }
  return(Combined)
}


animalSummary<- combine_animal_in_year(list(103, 104, 105, 106, 107, 108, 109, 110, 111))
animalSummary <- animalSummary[-1, ]
View(animalSummary)

# output
write_xlsx(animalSummary, "Data/animalSummary.xlsx")


