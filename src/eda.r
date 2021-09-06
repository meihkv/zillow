library(tidyverse)
library(lubridate)
library(plotly)

sales_count = read.csv('./data/Metro_sales_count_now_uc_sfrcondo_month.csv',
                       colClasses = 'character') %>%
pivot_longer(col = starts_with('X'),
             names_to = 'Date',
             values_to = 'SalesCount') %>%
  mutate(Date = as.Date(Date, tryFormats = c("X%Y.%m.%d")),
         SalesCount = as.numeric(SalesCount),
         StateName = if_else(StateName == "", "US", StateName)) %>%
  group_by(SizeRank, StateName, Date) %>% 
  summarise(SalesCount = sum(SalesCount)) %>% 
  group_by(StateName) %>% 
  mutate(SalesCount_yoy = round(SalesCount/lag(SalesCount, 12),4)-1) %>% 
  drop_na()

inventory = read.csv('./data/Metro_invt_fs_uc_sfrcondo_month.csv',
                     colClasses = 'character') %>%
  pivot_longer(col = starts_with('X'),
               names_to = 'Date',
               values_to = 'Inventory') %>%
  mutate(Date = as.Date(Date, tryFormats = c("X%Y.%m.%d")),
         Inventory = as.numeric(Inventory),
         StateName = if_else(StateName == "", "US", StateName)) %>%
  group_by(SizeRank, StateName, Date) %>% 
  summarise(Inventory = sum(Inventory)) %>% 
  group_by(StateName) %>% 
  mutate(Inventory_yoy = round(Inventory/lag(Inventory, 12),4)-1) %>% 
  drop_na()

zillow = inventory %>% 
  inner_join(sales_count)
