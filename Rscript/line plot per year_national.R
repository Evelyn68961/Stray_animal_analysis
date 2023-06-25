
# library
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(scales)
library(extrafont)

# import data
animalSum <- read_excel("Data/animalSummary.xlsx", col_names = TRUE)
View(animalSum)

# create folder
dir.create("Plot/Taiwan", showWarnings = FALSE)

# geom_line
TaiwanCity <- animalSum |> filter(City == "全國")
View(TaiwanCity)


# animal number by year
ggplot(TaiwanCity, aes(Year, AnimalNumber)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(102,111,1)) +
  scale_y_continuous(limits = c(0,100000), 
                     breaks = seq(0,100000,20000),
                     labels = label_number(suffix = " K", scale = 1e-3)) +
  labs(title = "全國收容所 流浪動物收容隻數",
       y = "收容隻數", x = "民國年份") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Taiwan/全國收容所_流浪動物收容隻數.png", dpi = 500)
  

# adoption number by year
ggplot(TaiwanCity, aes(Year, AdoptionNumber)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(102,111,1)) +
  scale_y_continuous(limits = c(0,100000), 
                     breaks = seq(0,100000,20000),
                     labels = label_number(suffix = " K", scale = 1e-3)) +
  labs(title = "全國收容所 流浪動物認領養隻數",
       y = "認領養隻數", x = "民國年份") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Taiwan/全國收容所_流浪動物認領養隻數.png", dpi = 500)

# adoption rate by year
ggplot(TaiwanCity, aes(Year, AdoptionRate)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(102,111,1)) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1,0.2)) +
  labs(title = "全國收容所 流浪動物認領養率",
       y = "認領養率", x = "民國年份") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Taiwan/全國收容所_流浪動物認領養率.png", dpi = 500)

# euthanasia number by year
ggplot(TaiwanCity, aes(Year, EuthanasiaNumber)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(102,111,1)) +
  scale_y_continuous(limits = c(0,30000), 
                     breaks = seq(0,30000,10000),
                     labels = label_number(suffix = " K", scale = 1e-3)) +
  labs(title = "全國收容所 流浪動物依法人道處理隻數",
       y = "依法人道處理隻數", x = "民國年份") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Taiwan/全國收容所_流浪動物依法人道處理隻數.png", dpi = 500)

# euthanasia rate by year
ggplot(TaiwanCity, aes(Year, EuthanasiaRate)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(102,111,1)) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1,0.2)) +
  labs(title = "全國收容所 流浪動物依法人道處理率",
       y = "依法人道處理率", x = "民國年份") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Taiwan/全國收容所_流浪動物依法人道處理率.png", dpi = 500)

# death number by year
ggplot(TaiwanCity, aes(Year, DeathNumber)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(102,111,1)) +
  scale_y_continuous(limits = c(0,30000), 
                     breaks = seq(0,30000,10000),
                     labels = label_number(suffix = " K", scale = 1e-3)) +
  labs(title = "全國收容所 流浪動物死亡隻數",
       y = "死亡隻數", x = "民國年份") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Taiwan/全國收容所_流浪動物死亡理隻數.png", dpi = 500)

# death rate by year
ggplot(TaiwanCity, aes(Year, DeathRate)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(102,111,1)) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1,0.2)) +
  labs(title = "全國收容所 流浪動物死亡率",
       y = "死亡率", x = "民國年份") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = 0.2),
        axis.title.y = element_text(vjust = 0.2,hjust = 0.5),
        text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Taiwan/全國收容所_流浪動物死亡率.png", dpi = 500)

