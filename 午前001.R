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

# shift + PgDn 
# shift + End

# Ctrl + Alt + 矢印　/ カーソル複製

# Ctrl + Alt + B  全て実行
# Ctrl + Alt + E　その行から後ろ全て実行

# Ctrl + D 一行消す


# Read data file
filename = "~/Lab_Data/kabeyamam/arikawa_amamo_line.xlsx"

esheet = excel_sheets(filename)

esheet
esheet[1] # esheetの1番目の要素を抽出

d1 = read_xlsx(filename, sheet = esheet[1])
d2 = read_xlsx(filename, sheet = esheet[2])


# d1の構造を修正する
# ...5, ...10, などの列を削除する
# 正規表現で検索している

d1 = d1 |> select(matches("ライン|距離|時刻|アマモ"))  # select:列を選ぶ

# dbl : 実数
# chr : キャラクター、文字
# dttm : 時刻
# lgl : 論理値
# NA : No data

d1l5 = d1 |> select(matches("[5]$")) 
# 列名の最後(後ろに$)に5が入ったものを抽出

d1l5 = d1l5 |> 
  mutate(amamo = 
                 ifelse(is.na(`時刻5`),
                        "なし", "あり")) 
# NAなら「なし」、NAじゃなければ「あり」の列を作成

ggplot(d1l5) +
  geom_point(aes(x = `ライン5`,
                 y = amamo))


d1l7 = d1 |> select(matches("[7]$")) 

d1l7 = d1l7 |> 
  mutate(amamo = 
           ifelse(is.na(`時刻7`),
                  "なし", "あり")) 

ggplot(d1l7) +
  geom_point(aes(x = `ライン7`,
                 y = amamo))

ggplot() +
  geom_point(aes(x = `ライン5`,
                 y = amamo,
                 color = "L5"),
             data =d1l5)+
  geom_point(aes(x = `ライン7`,
                 y = amamo,
                 color = "L7"),
             data =d1l7)
# よくない図

#################################################

# ここでエラー、縦にするにはつなげたい列の種類は同じじゃないといけない
d1 |> pivot_longer(cols = everything())

# 列の種類を統一する

d1 |> 
  select(matches("ライン"))

d1 |> 
  select(matches("時刻"))　# 種類が混ざっている

d1 |> 
  select(matches("距離"))

# data処理のやり直し
# Sheetの読み直し
# 全ての列をtextとして読み込む

d1 = read_xlsx(filename, sheet = esheet[1],
               col_types = "text")
d2 = read_xlsx(filename, sheet = esheet[2],
               col_types = "text")

# d1からは必要な列だけ抽出する
d1 = d1 |> select(matches("ライン|距離|時刻|アマモ"))  # select:列を選ぶ

# 横長のd1を縦長に変換する
d1 = d1 |> pivot_longer(cols = everything())

# str_extract:指定した文字列を抜き出す
# as.numeric:lineの文字列を数字列に変換
d1 = d1 |> 
  mutate(line = str_extract(name, "[0-9]+$")) |> # +を入れると数字が一回以上出るときに使用可能
  mutate(line = as.numeric(line))

# name列から数字を削除
d1 = d1 |> 
  mutate(name = str_remove(name, "[0-9]+$"))

# 最後の部分を抽出して確認
d1 |> tail()

# 横長の列を作成
d1 = d1 |> pivot_wider(names_from = name,
                  values_from = value) |> 
  unnest(everything())

d1 = d1 |> 
  mutate(距離 = str_extract(距離, "[0-9]+")) |> 
  mutate(距離 = as.numeric(距離)) |> 
  mutate(アマモ = factor(アマモ))

ggplot() +
  geom_point(aes(x = 距離,
                 y = アマモ,
                 color = line),
             data = d1)

d1b = d1 |> 
  mutate(line = factor(line)) |> 
  mutate(アマモ = str_remove(アマモ,
                             "orB"))
ggplot() +
  geom_point(aes(x = 距離,
                 y = アマモ,
                 color = line),
             data = d1b) +
  facet_grid(rows = vars(line)) # 列でlineごとのグラフを作成

# Plots
color = viridis::viridis(6)
font_add_google("Noto Sans JP", "notosans-jp")
font_add_google("Noto Sans", "notosans")
showtext_auto()

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

wh = aseries(6) 
# aシリーズの寸法(数字で寸法変わる)、bシリーズもある

plotname = "kabeyama_01.pdf"
ggsave(filename = plotname,
       width = wh[2],
       height = wh[1],units = "mm") # mmはwhの数値の単位を示す

image_read_pdf(plotname) |> 
  image_resize("500x") |> 
  image_write(str_replace(plotname, "pdf", "png"))

########################################################

d2 = d2 |> select(matches("ライン|距離|時刻|アマモ"))  
# select:列を選ぶ

d2 = d2 |> pivot_longer(cols = everything())

d2 = d2 |> 
  mutate(line = str_extract(name, "[0-9]+$")) |> # +を入れると数字が一回以上出るときに使用可能
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

ggplot() +
  geom_point(aes(x = 距離,
                 y = アマモ,
                 color = line),
             data = d2)

d2b = d2 |> 
  mutate(line = factor(line)) |> 
  mutate(アマモ = str_remove(アマモ,
                             "orC")) |> 
  mutate(アマモ = ifelse(str_detect(アマモ, "^T$"), NA, アマモ))
# ^:最初の文字が... / $:最後の文字が...
# str:文字列

ggplot() +
  geom_point(aes(x = 距離,
                 y = アマモ,
                 color = line),
             data = d2b) +
  facet_grid(rows = vars(line)) # 列でlineごとのグラフを作成

# Plots
color = viridis::viridis(6)
font_add_google("Noto Sans JP", "notosans-jp")
font_add_google("Noto Sans", "notosans")
showtext_auto()

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

wh = aseries(6) 
# aシリーズの寸法(数字で寸法変わる)、bシリーズもある

plotname2 = "kabeyama_02.pdf"
ggsave(filename = plotname2,
       width = wh[2],
       height = wh[1],units = "mm")

image_read_pdf(plotname2) |> 
  image_resize("500x") |> 
  image_write(str_replace(plotname2, "pdf", "png"))
