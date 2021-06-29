# 2021 R 研修会
# データロガーの読み込みと作図
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
# dataのフォルダー
folder = "~/Lab_Data/kawatea/"
# ファイル名とパス
mnames = str_glue("{folder}Microstation") |> dir(full = T)
onames = str_glue("{folder}Oxygen") |> dir(full = T)
# 2021/04のデータの存在を確認する
str_subset(mnames, "_2104")
str_subset(onames, "_2104")

# Microstation 2021 April を読み込む
micro = str_subset(mnames, "_2104") |> read_onset()

# Oxygenを読み込む

oxygen = 
  tibble(onames) |> 
  filter(str_detect(onames,"2104")) |> 
  mutate(data = map(onames, read_onset)) 
# それぞれのonamesにread_onsetの処理を行う

oxygen = 
  oxygen |> 
  mutate(onames = basename(onames)) |> 
  # basenameでpath(どこから引っ張っているか)を削除
  separate(onames,
           c("logger",
             "ID",
             "location",
             "position",
             "survey",
             "extension")) |> 
  select(-logger, -survey, -extension)

oxygen = 
  oxygen |> 
  mutate(location = ifelse(str_detect(location,"amamo"),
                           "Zostera","Sargassum"))

oxygen = oxygen |> unnest(data)

# data の確認
oxygen |> slice(1:3)
micro |> slice(1:3)


micro =
  micro |>
  mutate(datetime = floor_date(datetime, "min"))


dall = full_join(oxygen,
          micro,
          by = "datetime")
# inner_join は合致する部分だけ出す

s0 = as_date("2021/04/24")
s1 = as_date("2021/05/18")

interval(s0, s1)

dall = dall |> filter(datetime %within% interval(s0, s1))

dall |> 
  ggplot() +
  geom_line(aes(x = datetime,
                y = temperature,
                color = position)) +
  facet_grid(rows = vars(location))

dall |> 
  ggplot() +
  geom_line(aes(x = datetime,
                y = mgl,
                color = position)) +
  facet_grid(rows = vars(location))

dall |> 
  ggplot() +
  geom_point(aes(x = ppfd,
                 y = temperature,
                color = position)) +
  facet_grid(rows = vars(location))

dsum = dall |> 
  mutate(date = as_date(datetime), # datetimeをdateに変換
         .before = "location") |> 
  # locationの前にdateをもってくる
  group_by(location, position, date) |> 
  summarise(across(c(mgl, temperature,
                     ppfd, wind),
                   list(mean = mean,
                        sd = sd,
                        median = median))) # それぞれをリストに追加
# mean = mean : 左は列名、右はその列に何をさせるか 

ggplot(dsum) +
  geom_point(aes(x = ppfd_mean,
                 y = mgl_mean)) +
  facet_grid(rows = vars(location),
             cols = vars(position))

dall |> 
  mutate(date = as_date(datetime),
         .before = "location") |> 
  group_by(location, position, date) |> 
  mutate(rate = mgl - lag(mgl), .before = "mgl") |> 
  # lag:前にデータを1個ずらす / lead:後ろにデータを1個ずらす
  select(location, position, date,
         rate, ppfd) |> 
  drop_na() |> 
  summarise(across(c(rate, ppfd),
                   list(mean = mean,
                        sd = sd))) |> 
  ggplot() +
  geom_point(aes(x = ppfd_mean,
                 y = rate_mean)) +
  facet_grid(rows = vars(location),
             cols = vars(position))


