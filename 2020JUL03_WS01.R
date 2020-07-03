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

depth =
  depth %>%
  unnest(data) %>%
  select(fname,
         datetime = matches("日付 時間"),
         kpa = matches("^絶対圧力, kPa$"),
         temperature_air = matches("^温度, °C$"))
# 正規表現

# depth %>% mutate(datetime = parse_date_time(datetime,
#                                             "%d/%m/%y %H:%M:%S")) %>%
#   slice(70:75)

# MACOS の場合、次のコード
# depth %>%
#   mutate(datetime = str_replace(datetime, "午後", "PM")) %>%
#   mutate(datetime = str_replace(datetime, "午前", "AM")) %>%
#   mutate(datetime = parse_date_time(datetime, "%d/%m/%y %H:%M:%S"))

depth = depth %>% mutate(datetime = parse_date_time(datetime, "mdyT*"))



######################################

# Microstation

microstation = tibble(fname) %>% filter(str_detect(fname, "Microstation"))

microstation = microstation %>%
  mutate(data = map(fname, read_csv, skip = 1)) %>% unnest(data)

microstation %>%
  select(fname,
         datetime = matches("日付 時間"),
         insolation = matches("日射"),
         wind = matches("風速"),
         gust = matches("突風"),
         kpa_micro = matches("圧力"))


# データの結合

# 統計量の求め方

# 作図

# 図書き込み


