# Reading meteorological data from the kishocho site.
# Get list of prec_no and block_no
# Greg Nishihara
# 2020 Jul 02

# 気象庁 (JMA) の過去のデータ検索サイトからデータを抽出するときに使う関数。

library(tidyverse)
library(rvest)
library(lubridate)

scrape_prec_no = function() {
  # Function to retrieve all the prec_no.
  # 県の prec_no 番号を取得する関数
  URL = "http://www.data.jma.go.jp/obd/stats/etrn/select/prefecture00.php?prec_no=&block_no=&year=&month=&day=&view="
  out = read_html(URL)
  out = out %>% html_nodes(xpath = '//*[@id="main"]/map')
  df = out %>% html_children()
  df %>%
    map(xml_attrs) %>%
    map_df(~as.list(.)) %>%
    select(alt, href) %>%
    mutate(value = str_extract(href, "prec_no=[0-9]+")) %>%
    mutate(value = str_extract(value, "[0-9]+")) %>%
    select(prec_no = alt, value)
}

scrape_block_no = function(prec_no=84) {
  # Retrieve all of the block_no for the specified prec_no.
  # 県内の block_no 番号を取得する関数。
  # prec_no = 84 は長崎県です。
  # prec_no = 91 は沖縄です。
  # scape_prec_no() %>% print(n = Inf) を実行すると他の番後が表示されます。
  URL = paste0("https://www.data.jma.go.jp/obd/stats/etrn/select/prefecture.php?",
               "prec_no=", prec_no,
               "&block_no=&year=&month=&day=&view=")
  out = read_html(URL)
  out = out %>% html_nodes(xpath = '//*[@id="ncontents2"]/map')
  df = out %>% html_children()

  df %>%
    map(xml_attrs) %>%
    map_df(~as.list(.)) %>%
    select(alt, href, onmouseover) %>%
    mutate(site = str_extract(href, "block_no=[0-9]+")) %>%
    mutate(site = str_extract(site, "[0-9]+")) %>%
    mutate(kubun = str_extract(onmouseover, "'s'|'a'")) %>%
    mutate(kubun = str_extract(kubun, "a|s")) %>%
    mutate(prec_no) %>%
    select(prec_no, block = alt, block_no = site, kubun) %>% drop_na() %>% distinct()
}

# prec_no = scrape_prec_no()
# scrape_block_no()
# write_csv(prec_no, "list_of_prec_no.csv")





