library(tidyverse)
library(lubridate)
library(plotly)
sales_count = read.csv('./data/Metro_sales_count_now_uc_sfrcondo_month.csv',colClasses = 'character')

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

inventory = read.csv('./data/Metro_invt_fs_uc_sfrcondo_month.csv', 
                     colClasses = 'character')

inventory_pivot = sales_count %>% 
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
  pivot_wider(names_from = StateName, values_from = SalesCount) %>% 
  column_to_rownames(., var = "Year")

relative_change = (inventory_pivot/lag(inventory_pivot)-1) %>% 
  as_tibble(rownames = "Year") %>% 
  pivot_longer(col = -Year,names_to = 'StateName', values_to = 'Inventory') %>% 
  mutate(Year = as.Date(Year),
         Inventory = round(Inventory, 2))

result=ggplot(relative_change) + 
  geom_line(aes(x = Year, y = Inventory, color = StateName)) +
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()

ggplotly(result)
