

# library
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(scales)
library(extrafont)
library(plotrix)

setwd("C:/Users/ccdfg/OneDrive/桌面/R_Final")

# import data
animalSum <- read_excel("Data/animalSummary.xlsx", col_names = TRUE)
animalSum <- filter(animalSum, City != "全國")
animalSum <- subset(animalSum, select = c("City", "AnimalNumber", "Year"))
View(animalSum)

# create folder
dir.create("Plot/AllCity", showWarnings = FALSE)

# create 105 chart
animal105 <- subset(animalSum, Year == "105")
View(animal105)
animal105$City <- factor(animal105$City, levels = animal105$City[order(-animal105$AnimalNumber)])
ggplot(animal105) +
  geom_col(mapping = aes(x = City, y = AnimalNumber, fill = City)) +
  labs(x = "Category", y = "Value", title = "Bar Chart") +
  scale_y_continuous(expand=c(0, 0), lim=c(0,10000)) +
  labs(title="民國105年 各縣市收容所 流浪動物收容隻數", x="縣市", y = "收容隻數", fill = "縣市")+
  theme_classic() +
  theme (axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/AllCity/民國105年各縣市收容所流浪動物收容隻數.png", dpi = 300)

# create 107 chart
animal107 <- subset(animalSum, Year == "107")
animal107$City <- factor(animal107$City, levels = animal107$City[order(-animal107$AnimalNumber)])
ggplot(animal107) +
  geom_col(mapping = aes(x = City, y = AnimalNumber, fill = City)) +
  labs(x = "Category", y = "Value", title = "Bar Chart") +
  scale_y_continuous(expand=c(0, 0), lim=c(0,10000)) +
  labs(title="民國107年 各縣市收容所 流浪動物收容隻數", x="縣市", y = "收容隻數", fill = "縣市")+
  theme_classic() +
  theme (axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/AllCity/民國107年各縣市收容所流浪動物收容隻數.png", dpi = 300)

# create 109 chart
animal109 <- subset(animalSum, Year == "109")
animal109$City <- factor(animal109$City, levels = animal109$City[order(-animal109$AnimalNumber)])
ggplot(animal109) +
  geom_col(mapping = aes(x = City, y = AnimalNumber, fill = City)) +
  labs(x = "Category", y = "Value", title = "Bar Chart") +
  scale_y_continuous(expand=c(0, 0), lim=c(0,10000)) +
  labs(title="民國109年 各縣市收容所 流浪動物收容隻數", x="縣市", y = "收容隻數", fill = "縣市")+
  theme_classic() +
  theme (axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/AllCity/民國109年各縣市收容所流浪動物收容隻數.png", dpi = 300)

# create 111 chart
animal111 <- subset(animalSum, Year == "111")
animal111$City <- factor(animal111$City, levels = animal111$City[order(-animal111$AnimalNumber)])
ggplot(animal111) +
  geom_col(mapping = aes(x = City, y = AnimalNumber, fill = City)) +
  labs(x = "Category", y = "Value", title = "Bar Chart") +
  scale_y_continuous(expand=c(0, 0), lim=c(0,10000)) +
  labs(title="民國111年 各縣市收容所 流浪動物收容隻數", x="縣市", y = "收容隻數", fill = "縣市")+
  theme_classic() +
  theme (axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/AllCity/民國111年各縣市收容所流浪動物收容隻數.png", dpi = 300)