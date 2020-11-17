## Wrangle Thanksgiving food survey
## Data source: fivethirtyeight, from TidyTuesday repo: https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-11-20
## Author: Juliette Verstaen
## Date: 11/14/2020

library(tidyverse) 
library(here)
library(tm)

'%!in%' <- function(x,y)!('%in%'(x,y))

## FiveThirtyEight data
thanksgiving_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-20/thanksgiving_meals.csv')


## There are a lot of naming mismatches unfortunately, but we can fix that! Note: Will group together all cheesecake flavors into one, and custards as well.
fivethirtyeight_pies <- thanksgiving_raw %>% 
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
  mutate(Type = case_when(Flavor %in% c("buttermilk", "chocolate", "mincemeat", "other", "sweet potato", "pecan") ~ "other",
                          Flavor == "none" ~ "none", 
                          T ~ "fruit")) %>% 
  select(flavor = Flavor, type = Type, survey_counts = Count) %>% 
  mutate(survery_sum = sum(survey_counts)) %>% 
  rowwise() %>% 
  mutate(survey_percent = survey_counts/survery_sum)  %>% 
  select(-survery_sum) 


## R-ladies data
### read in the data from google sheets

pie_list <- c("apple", "blueberry", "buttermilk", "cherry", "chocolate", "coconut cream", "key lime", "lemon meringue", "mincemeat", "peach", "pecan", "pumpkin", "sweet potato", "none")

r_ladies_raw <- read_csv(here("data/r_ladies_survey.csv")) 

r_ladies_pies <- r_ladies_raw %>% 
  dplyr::select("What pies are eaten during your chosen Thanksgiving Day holiday? (Check all that apply)") %>% 
  rename(pie_selections = "What pies are eaten during your chosen Thanksgiving Day holiday? (Check all that apply)") %>% 
  mutate(flavor = strsplit(pie_selections, ",")) %>%
  unnest(flavor)  %>% 
  mutate(flavor = str_trim(flavor, side = c("both"))) %>% 
  select(flavor) %>% 
  count(flavor) %>% 
  mutate(flavor = ifelse(flavor %!in% pie_list, "other", flavor)) %>% 
  rename(r_ladies_count = n) %>% 
  group_by(flavor) %>% 
  dplyr::summarise(r_ladies_count = sum(r_ladies_count)) %>% 
  ungroup() %>% 
  mutate(total = sum(r_ladies_count),
         r_percent = r_ladies_count/total) %>% 
  select(-total)

thanksgiving_pie <- left_join(fivethirtyeight_pies, r_ladies_pies, by = "flavor") %>% 
  mutate(r_ladies_count = ifelse(is.na(r_ladies_count), 0, r_ladies_count),
         r_percent = ifelse(is.na(r_percent), 0, r_percent)) %>% 
  dplyr::select(flavor, type, survey_counts, r_ladies_counts = r_ladies_count, survey_percent, r_percent)

write_csv(thanksgiving_pie, here("data/thanksgiving_pie.csv"))
