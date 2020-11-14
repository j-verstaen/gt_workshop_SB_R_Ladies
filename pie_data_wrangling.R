## Wrangle Thanksgiving food survey
## Data source: fivethirtyeight, from TidyTuesday repo: https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-11-20
## Author: Juliette Verstaen
## Date: 11/14/2020


library(tidyverse) 
library(here)

thanksgiving_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-20/thanksgiving_meals.csv')


## There are a lot of naming mismatches unfortunately, but we can fix that! Note: Will group together all cheesecake flavors into one, and custards as well.


thanksgiving_pie <- thanksgiving_raw %>% 
  dplyr::select(pie1:pie11, pie13) %>% 
  pivot_longer(cols =  pie1:pie13, names_to = "pies_num") %>% 
  select(type = value) %>% 
  table() %>%
  as_tibble() %>% 
  rename(Type = ".",
         Count = n) %>% 
  mutate(Type = tolower(Type)) %>% 
  mutate(Type = ifelse(Type == "blueberry or peach cobbler", "blueberry",
                       ifelse(Type == "cheesecak", "cheesecake",
                              ifelse(Type == "homemade pumpkin pie", "pumpkin", 
                                     ifelse(Type == "cherry cheescake", "cherry cheesecake", 
                                            ifelse(Type %in% c("lemon merang", "lemon mirangue"), "lemon meringue", 
                                                   ifelse(Type %in% c("mince meat", "minced meat"), "mincemeat", Type))))))) %>% 
  add_row(Type = "blueberry pie", Count = 1) %>% 
  add_row(Type = "banana cream pie", Count = 1) %>% 
  filter(Type != "blueberry pie, banana cream pie") %>% 
  mutate(Type = ifelse(str_detect(Type, "cheesecake"), "cheesecake", Type)) %>% 
  mutate(Type = removeWords(Type, "pie")) %>% 
  add_row(Type = "lemon meringue", Count = 1) %>% 
  add_row(Type = "mincemeat", Count = 1) %>% 
  filter(Type != "lemon maraigne, minch meat") %>% 
  mutate(Type = ifelse(str_detect(Type, "custard"), "custard", Type)) %>% 
  filter(Type != "usually some other  along with an apple ",
         Type != "we serve pine nut cake not ",
         Type != " with ice cream and whipped cream")%>% 
  add_row(Type = "pecan", Count = 1) %>% 
  add_row(Type = "mincemeat", Count = 1) %>% 
  filter(Type != "pecan, mincemeat") %>% 
  mutate(Type = str_trim(Type, side = c("both"))) %>% 
  group_by(Type) %>% 
  dplyr::summarise(Count = sum(Count)) %>% 
  ungroup() %>% 
  mutate(Type = ifelse(Count <5, "other", Type))%>% 
  group_by(Type) %>% 
  dplyr::summarise(Count = sum(Count)) %>% 
  ungroup() %>% 
  rename(Flavor = Type) %>% 
  mutate(Type = case_when(Flavor %in% c("buttermilk", "chocolate", "mincemeat", "other", "sweet potato") ~ "other",
                          Flavor == "none" ~ "none", 
                          T ~ "fruit"),
         r_ladies_count = Count) %>% 
  select(flavor = Flavor, type = Type, survey_counts = Count, r_ladies_count) %>% 
  mutate(survery_sum = sum(survey_counts),
         r_sum = sum(r_ladies_count)) %>% 
  rowwise() %>% 
  mutate(survey_percent = survey_counts/survery_sum,
         r_percent = r_ladies_count/r_sum)  %>% 
  select(-survery_sum, -r_sum) 

write_csv(thanksgiving_pie, here("data/thanksgiving_pie.csv"))
