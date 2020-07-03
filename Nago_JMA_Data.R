# 沖縄県名護市気象台データ取得
# 2020 Jul 02
# Greg Nishihara
# 一度実行して、データは保存してください。
# 何度も実行するとけ60

library(tidyverse)
library(rvest) # HTMLの読み込みに必要
library(lubridate)

source("scrape_jma_table.R")


# この関数は一回実行して、結果を保存してください。
# 実行したあと、コメントにしてください。
# prec_no = scrape_prec_no()
# write_csv(prec_no, path = "list_of_prec_no.csv")


prec_no = read_csv("list_of_prec_no.csv")
block_no = scrape_block_no(prec_no = 91)

nago = block_no %>% filter(str_detect(block, "名護"))

nago2019 = nago %>%
  mutate(year = 2019) %>%
  unnest(year) %>%
  mutate(month = list(1:12)) %>%
  unnest(month) %>%
  mutate(day = map(month, function(x) {
    1:days_in_month(x)
  })) %>% unnest(day) %>%
  mutate(data = pmap(list(year, month, day, prec_no, block_no, kubun), get_hpa)) %>%
  select(block, data)

nago2020 = nago %>%
  mutate(year = 2020) %>%
  unnest(year) %>%
  mutate(month = list(1:5)) %>%
  unnest(month) %>%
  mutate(day = map(month, function(x) {
    1:days_in_month(x)
  })) %>% unnest(day) %>%
  mutate(data = pmap(list(year, month, day, prec_no, block_no, kubun), get_hpa)) %>%
  select(block, data)

nago = nago2019 %>% bind_rows(nago2020)

nago = nago %>% unnest(data) %>% mutate(H = hour(datetime) + minute(datetime)/60)

write.csv(nago, file = "nago_2019to2020_06.csv")
# file.copy("nago_2019to2020_06.csv", "~/Data/Mozuku2019/nago_2019to2020_06.csv")
