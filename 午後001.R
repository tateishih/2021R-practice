# 2021 R 研修会
# Hiroto Tateishi
# 2021 / 06 / 29
##################################################################

# Load packages

library(tidyverse)
library(lubridate) #時刻
library(gnnlab) #lab
library(magick)
library(showtext)
library(readxl) #Excel読み込みパッケージ
library(patchwork)

# Plotの設定
color = viridis::viridis(6)
font_add_google("Noto Sans JP", "notosans-jp")
font_add_google("Noto Sans", "notosans")
showtext_auto()

# Read data file
filename = "~/Lab_Data/kabeyamam/arikawa_amamo_line.xlsx"

esheet = excel_sheets(filename)

d1 = read_xlsx(filename, sheet = esheet[1],
               col_types = "text")
d2 = read_xlsx(filename, sheet = esheet[2],
               col_types = "text")

# d1の処理はここから

d1 = d1 |> select(matches("ライン|距離|時刻|アマモ"))  # select:列を選ぶ

d1 = d1 |> pivot_longer(cols = everything())

d1 = d1 |> 
  mutate(line = str_extract(name, "[0-9]+$")) |>
  mutate(line = as.numeric(line))

d1 = d1 |> 
  mutate(name = str_remove(name, "[0-9]+$"))

d1 = d1 |> pivot_wider(names_from = name,
                       values_from = value) |> 
  unnest(everything())

d1 = d1 |> 
  mutate(距離 = str_extract(距離, "[0-9]+")) |> 
  mutate(距離 = as.numeric(距離)) |> 
  mutate(アマモ = factor(アマモ))

d1b = d1 |> 
  mutate(アマモ = str_remove(アマモ,
                             "orB"))

########################################################

# d2の処理はここから

d2 = d2 |> select(matches("ライン|距離|時刻|アマモ"))  

d2 = d2 |> pivot_longer(cols = everything())

d2 = d2 |> 
  mutate(line = str_extract(name, "[0-9]+$")) |>
  mutate(line = as.numeric(line))

d2 = d2 |> 
  mutate(name = str_remove(name, "[0-9]+$"))

d2 = d2 |> pivot_wider(names_from = name,
                       values_from = value) |> 
  unnest(everything())

d2 = d2 |> 
  mutate(距離 = str_extract(距離, "[0-9]+")) |> 
  mutate(距離 = as.numeric(距離)) |> 
  mutate(アマモ = factor(アマモ))

d2b = d2 |> 
  mutate(アマモ = str_remove(アマモ,
                             "orC")) |> 
  mutate(アマモ = ifelse(str_detect(アマモ, "^T$"), NA, アマモ))


# Plots

p1 = 
  ggplot() +
  geom_tile(aes(x = line,
                y = 距離,
                fill = アマモ),
            data = d1b) +
  annotate(geom = "text",
           x = 1,
           y = 166,
           label = "Seawall side",
           vjast = 0, hjust = 0,
           color = "white") +
  annotate(geom = "text",
           x = 6,
           y = 166,
           label = "Entrance to seagrass meadow",
           vjast = 0, hjust = 1,
           color = "white") +
  scale_y_continuous(name = "Distance (m)",
                     trans = "reverse",
                     limits = c(170,100),
                     breaks = seq(170,100,
                                  by = -10)) +
  scale_x_discrete(name = "Transect") +
  scale_fill_manual(name = "Coverage",
                    values = rev(color[-6])) +
  theme_gray(base_family = "notosans") # 文字のフォント

p2 = 
  ggplot() +
  geom_tile(aes(x = line,
                y = 距離,
                fill = アマモ),
            data = d2b) +
  annotate(geom = "text",
           x = 1,
           y = 166,
           label = "Seawall side",
           vjast = 0, hjust = 0,
           color = "white") +
  annotate(geom = "text",
           x = 11,
           y = 166,
           label = "Entrance to seagrass meadow",
           vjast = 0, hjust = 1,
           color = "white") +
  scale_y_continuous(name = "Distance (m)",
                     trans = "reverse",
                     limits = c(170,100),
                     breaks = seq(170,100,
                                  by = -10)) +
  scale_x_discrete(name = "Transect") +
  scale_fill_manual(name = "Coverage",
                    values = rev(color[-6])) +
  theme_gray(base_family = "notosans") # 文字のフォント

p1 + p2
p1 / p2

# 二つのdataを結合
# d1b + d2b

d1b = d1b |> mutate(month = 5)
d2b = d2b |> mutate(month = 6)
dall = 
  bind_rows(d1b, d2b) |> 
  mutate(month = factor(month,
                        levels = 1:12,
                        labels = month.abb[1:12]))

color = viridis::viridis(6)

dall |> 
  ggplot() +
  geom_tile(aes(x = line, y = 距離, fill = アマモ)) +
  scale_x_continuous("Line",
                     breaks = 1:11,
                     limits = c(0,12)) +
  scale_y_continuous("Distance (m)",
                     trans = "reverse",
                     breaks = seq(170, 100, by = -10),
                     limits = c(170,100)) +
  scale_fill_manual("Coverage", 
                    values = rev(color[-6])) +
  facet_grid(rows = vars(month)) +
  theme_gray(base_family = "notosans")
  
wh = aseries(6) 

plotname = "kabeyama_all.pdf"
ggsave(filename = plotname,
       width = wh[2],
       height = wh[1], units = "mm")

image_read_pdf(plotname) |> 
  image_resize("500x") |> 
  image_write(str_replace(plotname, "pdf", "png"))


dall |> 
  group_by(line, month, アマモ) |> 
  summarise(N = length(アマモ)) |> 
  arrange(month, desc(line), アマモ) |>  # 優先順位　# desc(…)にすると逆順
  ggplot() +
  geom_col(aes(x = line, y = N, fill = アマモ)) +
  scale_x_continuous("Line",
                     breaks = 1:11,
                     limits = c(0,12)) +
  scale_y_continuous("Counts") +
  scale_fill_manual("Coverage",
                    values = rev(color[-6])) +
  facet_grid(row = vars(month))


