# 長大・琉大合同Rゼミ
# Greg Nishihara
# 2020 Jul 03 13:00:00

# パッケージの読み込み

library(tidyverse)
library(lubridate)

# データの読み込み

## ファイルの場所

path = "~/Lab_Data/2020JUL_ZOOM_DATA/"
fname = dir(path, recursive = TRUE, pattern = "[Cc][Ss][Vv]$", full.names = TRUE)

depth = tibble(fname) %>%
  filter(str_detect(fname, "Depth"))

# depth %>% slice(1) %>% pull(fname) %>% read_csv(skip = 1)

depth = depth %>%
  mutate(data = map(fname, read_csv, skip = 1))

depth %>%
  unnest(data) %>%
  select(fname,
         datetime = matches("日付 時間"),
         kpa = matches("^絶対, kPa$"),
         temperature_air = matches("^温度, °C$"))


# データの結合

# 統計量の求め方

# 作図

# 図書き込み


