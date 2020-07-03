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
         temperature = matches("^温度, °C$"))
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

depth = depth %>% mutate(site = str_extract(fname, "north|west|south"))


######################################

# Microstation

microstation = tibble(fname) %>% filter(str_detect(fname, "Microstation"))

microstation = microstation %>%
  mutate(data = map(fname, read_csv, skip = 1)) %>% unnest(data)

microstation = microstation %>%
  select(fname,
         datetime = matches("日付 時間"),
         insolation = matches("日射"),
         wind = matches("風速"),
         gust = matches("突風"),
         kpa_micro = matches("圧力")) %>%
  mutate(datetime = parse_date_time(datetime, "mdyT*"))

# データの結合


microstation = microstation %>%
  mutate(datetime = floor_date(datetime, unit = "10 minutes"))


depth_wide = depth %>%
  select(-fname) %>%
  drop_na() %>%
  pivot_wider(names_from = site,
              values_from = c(kpa, temperature))

alldata = full_join(depth_wide,
          microstation %>% select(-fname),
          by = c("datetime"))

alldata

# 水深のもとめかた

alldata = alldata %>%
  mutate(depth_south = (kpa_south - kpa_micro)/9.81,
         depth_north = (kpa_north - kpa_micro)/9.81,
         depth_west =  (kpa_west - kpa_micro)/9.81) %>%
  drop_na()

kpadata = alldata %>%
  select(datetime, contains("kpa"), -kpa_micro) %>%
  pivot_longer(cols = c(kpa_south, kpa_north, kpa_west),
               names_to = "kpa_site",
               values_to = "kpa")

depthdata = alldata %>%
  select(datetime, contains("depth"))%>%
  pivot_longer(cols = c(depth_south, depth_north, depth_west),
               names_to = "depth_site",
               values_to = "depth")

temperaturedata = alldata %>%
  select(datetime, contains("temperature")) %>%
  pivot_longer(cols = c(temperature_south,
                        temperature_north,
                        temperature_west),
               names_to = "temperature_site",
               values_to = "temperature")


kpadata = kpadata %>%
  separate(kpa_site, into = c("measurement", "site"))
depthdata = depthdata %>%
  separate(depth_site, into = c("measurement", "site"))
temperaturedata = temperaturedata %>%
  separate(temperature_site, into = c("measurement", "site"))

tmp1 = full_join(kpadata %>% select(-measurement),
          depthdata %>% select(-measurement),
          by = c("datetime", "site"))

tmp2 = full_join(temperaturedata %>% select(-measurement),
          tmp1,
          by = c("datetime", "site"))

light_wind_data = alldata %>% select(datetime, insolation, wind, gust)


# 統計量の求め方

tmp2 = tmp2 %>%
  mutate(date = as_date(datetime)) %>%
  group_nest(site, date) %>%
  mutate(n = map_dbl(data, function(X) {
    X %>% nrow()
  })) %>%
  filter(near(n, 144))

temperature_summary = tmp2 %>% unnest(data) %>%
  group_by(site, date) %>%
  summarise_at(vars(temperature),
               list(mean = mean, min = min, max = max, sd = sd))

temperature_summary

# 作図

ylabel = "Water temperature (°C)"
xlabel = "Year-Month-Date"
clabel = "Site"

plot1 =
  temperature_summary %>%
  mutate(site = str_to_sentence(site)) %>%
  ggplot() +
  geom_line(aes(x = date,
                y = mean,
                color = site)) +
  scale_x_date(xlabel, date_labels = "%Y-%m-%d",
               date_breaks = "2 weeks") +
  scale_y_continuous(ylabel, limits = c(20, 28)) +
  scale_color_discrete(clabel) +
  ggtitle("Mean water temperature") +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))
plot1

ggplot(temperature_summary) +
  geom_line(aes(x = date,
                y = min,
                color = site))

ggplot(temperature_summary) +
  geom_line(aes(x = date,
                y = max,
                color = site))

# 図書き込み

ggsave("mean_temperature.png",
       plot = plot1,
       width = 160,
       height = 160,
       unit = "mm",
       dpi = 600)


