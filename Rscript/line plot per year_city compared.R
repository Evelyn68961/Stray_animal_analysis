# library
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(scales)
library(extrafont)

setwd("C:/Users/ccdfg/OneDrive/桌面/R_Final")

# import data
animalSum <- read_excel("Data/animalSummary.xlsx", col_names = TRUE)
View(animalSum)

# create folder
dir.create("Plot/Compared", showWarnings = FALSE)

# city filter
TaiwanCity_compared<- animalSum |> 
filter(Year %in% c(103,104,105,106,107,108,109,110,111))|>
filter(City =="新北市"|City =="新北市"|City =="臺北市"|City == "桃園市"|City =="臺中市"|City =="高雄市"|City =="臺南市")
view(TaiwanCity_compared)

# import data
animalSum_longer_city<- TaiwanCity_compared |> 
  tidyr::gather("Category", "value", c(2:8))
View(animalSum_longer_city)


# compare animal number
animalSum_longer_city |>
  mutate (Year_ch = as.character(Year)) |>
  filter (Category == "AnimalNumber") |>
  ggplot (aes(x = Year_ch, y = value, fill = Year_ch)) + 
  geom_col (position = position_dodge(width =0.6), color="black", width=0.7, alpha=0.7) +
  scale_y_continuous(expand=c(0, 0), lim=c(0,15000)) +
  labs(title="直轄市收容所 流浪動物收容隻數", x="民國年份", y = "收容隻數", fill = "民國年份")+
  theme_classic() +
  facet_wrap (~City, ncol = 3) +
  theme (strip.text.y = element_text(size = 5, color = "black", face = "bold"), 
         strip.text.x = element_text(size = 5, color = "black", face = "bold"),
         strip.background = element_rect(fill = "lightgray", color = NA),
         strip.text = element_text(size = 8,color = "black", face = "bold"),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Compared/直轄市收容所流浪動物收容隻數.png", dpi = 300)

# compare adoption number
animalSum_longer_city |>
  mutate (Year_ch = as.character(Year)) |>
  filter (Category == "AdoptionNumber") |>
  ggplot (aes(x = Year_ch, y = value, fill = Year_ch)) + 
  geom_col (position = position_dodge(width =0.6), color="black", width=0.7, alpha=0.7) +
  scale_y_continuous(expand=c(0, 0), lim=c(0,15000)) +
  labs(title="直轄市收容所 流浪動物認領養隻數", x="民國年份", y = "認領養隻數", fill = "民國年份")+
  theme_classic() +
  facet_wrap (~City, ncol = 3) +
  theme (strip.text.y = element_text(size = 5, color = "black", face = "bold"), 
         strip.text.x = element_text(size = 5, color = "black", face = "bold"),
         strip.background = element_rect(fill = "lightgray", color = NA),
         strip.text = element_text(size = 8,color = "black", face = "bold"),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Compared/直轄市收容所流浪動物認領養隻數.png", dpi = 300)

# compare euthanasia number
animalSum_longer_city |>
  mutate (Year_ch = as.character(Year)) |>
  filter (Category == "EuthanasiaNumber") |>
  ggplot (aes(x = Year_ch, y = value, fill = Year_ch)) + 
  geom_col (position = position_dodge(width =0.6), color="black", width=0.7, alpha=0.7) +
  scale_y_continuous(expand=c(0, 0), lim=c(0,4000)) +
  labs(title="直轄市收容所 流浪動物依法人道處理隻數", x="民國年份", y = "依法人道處理隻數", fill = "民國年份")+
  theme_classic() +
  facet_wrap (~City, ncol = 3) +
  theme (strip.text.y = element_text(size = 5, color = "black", face = "bold"), 
         strip.text.x = element_text(size = 5, color = "black", face = "bold"),
         strip.background = element_rect(fill = "lightgray", color = NA),
         strip.text = element_text(size = 8,color = "black", face = "bold"),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Compared/直轄市收容所流浪動物依法人道處理隻數.png", dpi = 300)

# compare death number
animalSum_longer_city |>
  mutate (Year_ch = as.character(Year)) |>
  filter (Category == "DeathNumber") |>
  ggplot (aes(x = Year_ch, y = value, fill = Year_ch)) + 
  geom_col (position = position_dodge(width =0.6), color="black", width=0.7, alpha=0.7) +
  scale_y_continuous(expand=c(0, 0), lim=c(0,3000)) +
  labs(title="直轄市收容所 流浪動物死亡隻數", x="民國年份", y = "死亡隻數", fill = "民國年份")+
  theme_classic() +
  facet_wrap (~City, ncol = 3) +
  theme (strip.text.y = element_text(size = 5, color = "black", face = "bold"), 
         strip.text.x = element_text(size = 5, color = "black", face = "bold"),
         strip.background = element_rect(fill = "lightgray", color = NA),
         strip.text = element_text(size = 8,color = "black", face = "bold"),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Compared/直轄市收容所流浪動物死亡隻數.png", dpi = 300)

# compare adoption rate
animalSum_longer_city |>
  mutate (Year_ch = as.character(Year)) |>
  filter (Category == "AdoptionRate") |>
  ggplot (aes(x = Year_ch, y = value*100, fill = Year_ch)) + 
  geom_col (position = position_dodge(width =0.6), color="black", width=0.7, alpha=0.7) +
  scale_y_continuous(expand=c(0, 0), lim=c(0,100)) +
  labs(title="直轄市收容所 流浪動物認領養率(%)", x="民國年份", y = "認領養率(%)", fill = "民國年份")+
  scale_fill_brewer(palette = "Greys") +
  theme_classic() +
  facet_wrap (~City, ncol = 3) +
  theme (strip.text.y = element_text(size = 5, color = "black", face = "bold"), 
         strip.text.x = element_text(size = 5, color = "black", face = "bold"),
         strip.background = element_rect(fill = "lightgray", color = NA),
         strip.text = element_text(size = 8,color = "black", face = "bold"),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Compared/直轄市收容所流浪動物認領養率(%).png", dpi = 300)

# compare euthanasia rate
animalSum_longer_city |>
  mutate (Year_ch = as.character(Year)) |>
  filter (Category == "EuthanasiaRate") |>
  ggplot (aes(x = Year_ch, y = value*100, fill = Year_ch)) + 
  geom_col (position = position_dodge(width =0.6), color="black", width=0.7, alpha=0.7) +
  scale_y_continuous(expand=c(0, 0), lim=c(0,50)) +
  labs(title="直轄市收容所 流浪動物依法人道處理率(%)", x="民國年份", y = "依法人道處理率(%)", fill = "民國年份")+
  scale_fill_brewer(palette = "Greys") +
  theme_classic() +
  facet_wrap (~City, ncol = 3) +
  theme (strip.text.y = element_text(size = 5, color = "black", face = "bold"), 
         strip.text.x = element_text(size = 5, color = "black", face = "bold"),
         strip.background = element_rect(fill = "lightgray", color = NA),
         strip.text = element_text(size = 8,color = "black", face = "bold"),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Compared/直轄市收容所流浪動物依法人道處理率(%).png", dpi = 300)

# compare death rate
animalSum_longer_city |>
  mutate (Year_ch = as.character(Year)) |>
  filter (Category == "DeathRate") |>
  ggplot (aes(x = Year_ch, y = value*100, fill = Year_ch)) + 
  geom_col (position = position_dodge(width =0.6), color="black", width=0.7, alpha=0.7) +
  scale_y_continuous(expand=c(0, 0), lim=c(0,50)) +
  labs(title="直轄市收容所 流浪動物死亡率(%)", x="民國年份", y = "死亡率(%)", fill = "民國年份")+
  scale_fill_brewer(palette = "Greys") +
  theme_classic() +
  facet_wrap (~City, ncol = 3) +
  theme (strip.text.y = element_text(size = 5, color = "black", face = "bold"), 
         strip.text.x = element_text(size = 5, color = "black", face = "bold"),
         strip.background = element_rect(fill = "lightgray", color = NA),
         strip.text = element_text(size = 8,color = "black", face = "bold"),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 7, face = "bold"),
         axis.title = element_text(size = 8, face = "bold"),
         plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2),
         legend.text = element_text(size = 4, face = "bold"),
         legend.title = element_text(size = 4, face = "bold"),
         legend.key.size = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"),
         text = element_text(family = "Microsoft Sans Serif"))

ggsave("Plot/Compared/直轄市收容所流浪動物死亡率(%).png", dpi = 300)

