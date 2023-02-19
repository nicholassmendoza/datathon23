library(tidyverse)
library(stringr)
library(readr)
library(lubridate)
setwd("C:/Users/14252/Documents/datathon")

sales <- read_delim("sales_data_2017_2018.csv")
sales

colnames(sales)

## How does the year 2017 compare to the year 2018?
## Were there any significant changes in purchasing habits of the customers?

salessplit <- sales
  salessplit[c('date', 'time')] <- str_split_fixed(salessplit$date, ' ', 2)
  salessplit <- salessplit[c('date', 'time', 'receipt_id', 'item_code', 'item_name'
                             , 'main_category', 'sub_category',
                           'quantity', 'payment_type', 'unit_buying_price', 
                           'unit_selling_price', 'unit_price_margin',
                           'total_buying_price', 'total_selling_price', 'total_profit')]
  
salessplit$date <-mdy(salessplit$date) 

sales2017 <- salessplit %>% 
  filter(between(date, as.Date('2017-01-01'), as.Date('2017-12-31')))

sales2018 <- salessplit %>% 
  filter(between(date, as.Date('2018-01-01'), as.Date('2018-12-31')))

write_csv(sales2017, "2017.csv")
write_csv(sales2018, "2018.csv")

## Which items should the store increase/decrease the price of? By how much? Why? 

## Which items should the store stop selling? Why?

## Which hour of the day should the store offer discounts at? Why? 

## Which items should potentially be sold together or kept closer together in the store to increase sales (Basket Analysis)? 

## What trends do we notice in the basket size (total items in one receipt)? 

## What trends do you notice for the store with respect to transactions?

## What trends do you notice for the store with respect to time?

totalprofitbytimesample <- read_delim("Sum of total_profit by time.csv")
dim(totalprofitbytimesample)

tsbts <- read_delim("Sum of total_selling_price by time.csv")
dim(tsbts)

sample1 <- read_delim("2017salessample.csv")

sample2 <- read_delim("2018sample.csv")

salessplit %>% 
  filter()

newsales <- read_delim("sales_data_2017_2018_for_tableau_with_new_date_columns.csv")

newsales %>% 
  select(month_number, total_profit) %>% 
  group_by(month_number) %>% 
  summarize(profit = sum(total_profit)) %>% 
  arrange(desc(profit))

newsales %>% 
  select(item_name, total_profit) %>% 
  group_by(item_name) %>% 
  summarize(profit = sum(total_profit)) %>% 
  arrange(profit)

newsales %>% 
  select(item_name, total_selling_price, month_number) %>% 
  group_by(item_name, month_number) %>% 
  summarize(sales = (sum(total_selling_price))) %>%
  arrange(desc(max))

newsales %>% 
  group_by(month_number, item_name) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))

## top 10 most sales per month

newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 1) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 2) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 3) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 4) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 5) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 6) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 7) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 8) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 9) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 10) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 11) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 12) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange(desc(sales))

## top 10 least sales per month
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 1) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 2) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 3) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 4) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 5) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 6) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 7) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 8) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))

newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 9) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 10) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 11) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))
newsales %>% 
  group_by(month_number, item_name) %>%
  filter(month_number == 12) %>% 
  select(item_name, total_selling_price, month_number) %>% 
  summarize(sales = sum(total_selling_price)) %>% 
  arrange((sales))





