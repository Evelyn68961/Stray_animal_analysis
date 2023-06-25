

# library
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(scales)
library(extrafont)
library(ggpubr)

# import data
strayAnimal <- read_excel("Data/animalStray.xlsx")
View(strayAnimal)

# filter
strayAnimal <- strayAnimal |>
  filter(City != "全國")
View(strayAnimal)


# Population vs StrayEstimate
ggplot(strayAnimal, aes(Population, StrayEstimate)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm",colour = "brown", linewidth = 0.5) +
  labs(x = "人口數", y = "遊蕩犬估計數", 
       title = "人口數 vs. 遊蕩犬估計數") +
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif")) +
  stat_cor(size = 2,label.x.npc = 0.7,label.y.npc = 0.89) +
  stat_regline_equation(size = 2,label.x.npc = 0.7,label.y.npc = "top")

ggsave("Plot/Correlation/Population vs. Estimate.png", dpi = 300)

# Population vs AnimalNumber
ggplot(strayAnimal, aes(Population, AnimalNumber)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm",colour = "brown", linewidth = 0.5) +
  labs(x = "人口數", y = "流浪動物收容隻數", 
       title = "人口數 vs. 流浪動物收容隻數") +
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_y_continuous(limits = c(0,12000), breaks = seq(0,12000,2000)) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif")) +
  stat_cor(size = 2,label.x.npc = 0.65,label.y.npc = 0.89) +
  stat_regline_equation(size = 2,label.x.npc = 0.65,label.y.npc = "top")

ggsave("Plot/Correlation/Population vs. AnimalNumber.png", dpi = 300)

# Population vs AdoptionNumber
ggplot(strayAnimal, aes(Population, AdoptionNumber)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm",colour = "brown", linewidth = 0.5) +
  labs(x = "人口數", y = "流浪動物認領養隻數", 
       title = "人口數 vs. 流浪動物認領養隻數") +
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_y_continuous(limits = c(0,10000), breaks = seq(0,10000,2000)) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif")) +
  stat_cor(size = 2,label.x.npc = 0.6,label.y.npc = 0.89) +
  stat_regline_equation(size = 2,label.x.npc = 0.6,label.y.npc = "top")

ggsave("Plot/Correlation/Population vs. AdoptionNumber.png", dpi = 300)

# AnimalNumber vs AdoptionNumber
ggplot(strayAnimal, aes(AnimalNumber, AdoptionNumber)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm",colour = "brown", linewidth = 0.5) +
  labs(x = "流浪動物收容隻數", y = "流浪動物認領養隻數", 
       title = "流浪動物收容隻數 vs. 認領養隻數") +
  scale_x_continuous(limits = c(0,8000), breaks = seq(0,8000,2000)) +
  scale_y_continuous(limits = c(0,8000), breaks = seq(0,8000,2000)) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif")) +
  stat_cor(size = 2,label.x.npc = 0.7,label.y.npc = 0.89) +
  stat_regline_equation(size = 2,label.x.npc = 0.7,label.y.npc = "top")

ggsave("Plot/Correlation/AnimalNumber vs. AdoptionNumber.png", dpi = 300)

# AnimalNumber vs DeathNumber
ggplot(strayAnimal, aes(AnimalNumber, DeathNumber)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm",colour = "brown", linewidth = 0.5) +
  labs(x = "流浪動物收容隻數", y = "流浪動物死亡隻數", 
       title = "流浪動物收容隻數 vs. 死亡隻數") +
  scale_x_continuous(limits = c(0,8000), breaks = seq(0,8000,2000)) +
  scale_y_continuous(limits = c(0,2000), breaks = seq(0,2000,400)) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif")) +
  stat_cor(size = 2,label.x.npc = 0.7,label.y.npc = 0.89) +
  stat_regline_equation(size = 2,label.x.npc = 0.7,label.y.npc = "top")

ggsave("Plot/Correlation/AnimalNumber vs. DeathNumber.png", dpi = 300)

# AnimalNumber vs StrayEstimate
ggplot(strayAnimal, aes(AnimalNumber, StrayEstimate)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm",colour = "brown", linewidth = 0.5) +
  labs(x = "流浪動物收容隻數", y = "遊蕩犬估計數", 
       title = "人口數 vs. 遊蕩犬估計數") +
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif")) +
  stat_cor(size = 2,label.x.npc = 0.7,label.y.npc = 0.89) +
  stat_regline_equation(size = 2,label.x.npc = 0.7,label.y.npc = "top")

ggsave("Plot/Correlation/Population vs. Estimate.png", dpi = 300)
