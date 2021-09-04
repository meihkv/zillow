library(tidyverse)
library(lubridate)
library(plotly)
sales_count = read.csv('./data/Metro_sales_count_now_uc_sfrcondo_month.csv',colClasses = 'character')

str(sales_count)

sales_count_pivot = sales_count %>% 
    pivot_longer(col = starts_with('X'),
                 names_to = 'Date',
                 values_to = 'SalesCount') %>% 
  mutate(Date = as.Date(Date, tryFormats = c("X%Y.%m.%d"))) %>% 
  mutate(SalesCount = as.numeric(SalesCount)) %>% 
  mutate(Year = floor_date(Date, unit = 'year')) %>% 
  group_by(StateName, Year) %>% 
  summarise(SalesCount = sum(SalesCount)) %>% 
  ungroup() %>% 
  mutate(StateName = if_else(StateName == '', 'US', StateName)) %>% 
  pivot_wider(names_from = StateName, values_from = SalesCount)