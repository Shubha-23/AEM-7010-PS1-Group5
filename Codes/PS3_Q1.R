library(dplyr)
library(tidyverse)
library(haven)
library(readr)
library(readxl)
library(stargazer)
library(tidyr)

rm(list=ls())

#setwd("/Users/shubhalakshminag/Dropbox/Cornell coursework/Semester 1/Applied micro 1/PS 3")
base_dir <- getwd()
data_dir <- paste0(base_dir,"/Data")
out_dir <- paste0(base_dir,"/Output")
# Import data

data <- read.csv(paste0(data_dir,"/raw/OTC_Headache.csv"))

# (a) Reshape wide to long

data_resh <- data %>% 
  pivot_longer(
    cols=-c(STORE, WEEK, COUNT),
    names_to = c("Var", "Brand"),
    names_pattern = "(.*)_(.*)",
    values_to = "Values"
  ) %>% 
  pivot_wider(
    names_from ="Var",
    values_from = "Values"
  ) %>% 
  arrange(STORE, Brand, WEEK) %>% 
  rename(
    store=STORE,
    week=WEEK, 
    count=COUNT,
    brand=Brand,
    sales=Sales,
    price=PRICE,
    cost=COST,
    prom=PROM
  )

# (b) Imputing missing observations 

# Get the max number of weeks in the data 
max_week <- max(data$WEEK, na.rm = T)
week_df <- data_resh %>% 
  select(store, brand) %>% 
  distinct() %>% 
  expand(store, brand, 1:48) %>% 
  rename(week="1:48")

# Merge store week brand data frame with main data 
data_final <- data_resh %>% 
  # get missing weeks for each store-brand
  right_join(week_df) %>% 
  arrange(store, brand, week) %>% 
  group_by(store, brand) %>% 
  # Create dummies for missing values (missing for 1 var implies missing for all the other vars)
  mutate(
    count_na=ifelse(is.na(count),1,0)
  ) %>% 
  # Find number of continuously missing values
  mutate(
    cons_count_na=case_when(count_na==1~sequence(rle(count_na)$lengths))
  ) %>% 
  ungroup()

# Dropping observations at the store-week level which have majority data missing 
data_final <- data_final %>% 
  group_by(store, brand) %>% 
  mutate(total_missing=sum(count_na),
         total_obs=n(),
         propn_missing=total_missing/total_obs) %>% 
  ungroup()

# Summarising proprtion missing 
table(data_final$propn_missing)

# Dropping data with more than 60% missing data 
data_final <- data_final %>% 
  filter(propn_missing<0.6)
  
# imputing data iteratively 
# First - check for consecutive missing data 
table(data_final$cons_count_na) # Max 6 consecutive missing data 

# Variable - count
data_final <- data_final %>% 
  group_by(store, brand) %>% 
  arrange(store, brand, week) %>% 
  mutate(# imputing the first missing term in a series of NA - 
         count=ifelse(cons_count_na==1 & is.na(count), lag(count), count),
         count=ifelse(cons_count_na==1 & is.na(count) & is.na(lag(count)), lead(count), count), # if the lag is not present
         # imputing for variables with NAs before and after
         count=ifelse(cons_count_na==2 & is.na(count) & is.na(lead(cons_count_na)), lead(count), count),
         count=ifelse(cons_count_na==3 & is.na(count) & is.na(lead(cons_count_na)), lead(count), count),
         count=ifelse(cons_count_na==6 & is.na(count) & is.na(lead(cons_count_na)), lead(count), count),
         count=ifelse(cons_count_na==5 & is.na(count), lead(count), count),
         count=ifelse(cons_count_na==4 & is.na(count), lead(count), count),
         count=ifelse(cons_count_na==2 & is.na(count) & (!is.na(lead(cons_count_na))|is.na(lead(count))), lag(count), count),
         count=ifelse(cons_count_na==3 & is.na(count) & (!is.na(lead(cons_count_na))|is.na(lead(count))), lag(count), count)
         ) %>% 
  ungroup()

# Variable - sales
data_final <- data_final %>% 
  group_by(store, brand) %>% 
  arrange(store, brand, week) %>% 
  mutate(# imputing the first missing term in a series of NA - 
    sales=ifelse(cons_count_na==1 & is.na(sales), lag(sales), sales),
    sales=ifelse(cons_count_na==1 & is.na(sales) & is.na(lag(sales)), lead(sales), sales), # if the lag is not present
    # imputing for variables with NAs before and after
    sales=ifelse(cons_count_na==2 & is.na(sales) & is.na(lead(cons_count_na)), lead(sales), sales),
    sales=ifelse(cons_count_na==3 & is.na(sales) & is.na(lead(cons_count_na)), lead(sales), sales),
    sales=ifelse(cons_count_na==6 & is.na(sales) & is.na(lead(cons_count_na)), lead(sales), sales),
    sales=ifelse(cons_count_na==5 & is.na(sales), lead(sales), sales),
    sales=ifelse(cons_count_na==4 & is.na(sales), lead(sales), sales),
    sales=ifelse(cons_count_na==2 & is.na(sales) & (!is.na(lead(cons_count_na))|is.na(lead(sales))), lag(sales), sales),
    sales=ifelse(cons_count_na==3 & is.na(sales) & (!is.na(lead(cons_count_na))|is.na(lead(sales))), lag(sales), sales)
  ) %>% 
  ungroup()

# Variable - price
data_final <- data_final %>% 
  group_by(store, brand) %>% 
  arrange(store, brand, week) %>% 
  mutate(# imputing the first missing term in a series of NA - 
    price=ifelse(cons_count_na==1 & is.na(price), lag(price), price),
    price=ifelse(cons_count_na==1 & is.na(price) & is.na(lag(price)), lead(price), price), # if the lag is not present
    # imputing for variables with NAs before and after
    price=ifelse(cons_count_na==2 & is.na(price) & is.na(lead(cons_count_na)), lead(price), price),
    price=ifelse(cons_count_na==3 & is.na(price) & is.na(lead(cons_count_na)), lead(price), price),
    price=ifelse(cons_count_na==6 & is.na(price) & is.na(lead(cons_count_na)), lead(price), price),
    price=ifelse(cons_count_na==5 & is.na(price), lead(price), price),
    price=ifelse(cons_count_na==4 & is.na(price), lead(price), price),
    price=ifelse(cons_count_na==2 & is.na(price) & (!is.na(lead(cons_count_na))|is.na(lead(price))), lag(price), price),
    price=ifelse(cons_count_na==3 & is.na(price) & (!is.na(lead(cons_count_na))|is.na(lead(price))), lag(price), price)
  ) %>% 
  ungroup()

# Variable - cost
data_final <- data_final %>% 
  group_by(store, brand) %>% 
  arrange(store, brand, week) %>% 
  mutate(# imputing the first missing term in a series of NA - 
    cost=ifelse(cons_count_na==1 & is.na(cost), lag(cost), cost),
    cost=ifelse(cons_count_na==1 & is.na(cost) & is.na(lag(cost)), lead(cost), cost), # if the lag is not present
    # imputing for variables with NAs before and after
    cost=ifelse(cons_count_na==2 & is.na(cost) & is.na(lead(cons_count_na)), lead(cost), cost),
    cost=ifelse(cons_count_na==3 & is.na(cost) & is.na(lead(cons_count_na)), lead(cost), cost),
    cost=ifelse(cons_count_na==6 & is.na(cost) & is.na(lead(cons_count_na)), lead(cost), cost),
    cost=ifelse(cons_count_na==5 & is.na(cost), lead(cost), cost),
    cost=ifelse(cons_count_na==4 & is.na(cost), lead(cost), cost),
    cost=ifelse(cons_count_na==2 & is.na(cost) & (!is.na(lead(cons_count_na))|is.na(lead(cost))), lag(cost), cost),
    cost=ifelse(cons_count_na==3 & is.na(cost) & (!is.na(lead(cons_count_na))|is.na(lead(cost))), lag(cost), cost)
  ) %>% 
  ungroup()

# Variable - prom
data_final <- data_final %>% 
  group_by(store, brand) %>% 
  arrange(store, brand, week) %>% 
  mutate(# imputing the first missing term in a series of NA - 
    prom=ifelse(cons_count_na==1 & is.na(prom), lag(prom), prom),
    prom=ifelse(cons_count_na==1 & is.na(prom) & is.na(lag(prom)), lead(prom), prom), # if the lag is not present
    # imputing for variables with NAs before and after, the last missing in a series takes the value of the
    # next variable
    prom=ifelse(cons_count_na==2 & is.na(prom) & is.na(lead(cons_count_na)), lead(prom), prom),
    prom=ifelse(cons_count_na==3 & is.na(prom) & is.na(lead(cons_count_na)), lead(prom), prom),
    prom=ifelse(cons_count_na==6 & is.na(prom) & is.na(lead(cons_count_na)), lead(prom), prom),
    prom=ifelse(cons_count_na==5 & is.na(prom), lead(prom), prom),
    prom=ifelse(cons_count_na==4 & is.na(prom), lead(prom), prom),
    prom=ifelse(cons_count_na==2 & is.na(prom) & (!is.na(lead(cons_count_na))|is.na(lead(prom))), lag(prom), prom),
    prom=ifelse(cons_count_na==3 & is.na(prom) & (!is.na(lead(cons_count_na))|is.na(lead(prom))), lag(prom), prom)
  ) %>% 
  ungroup()

# Checking for missing values
sapply(data_final, function(x) sum(is.na(x))) # looks good


# (c) Complete the table

# Creating a variable for brand names
data_final <- data_final %>% 
  mutate(
    brand_name=case_when(
      brand==1 ~ "Tylenol 25",
      brand==2 ~ "Tylenol 50",
      brand==3 ~ "Tylenol 100",
      brand==4 ~ "Advil 25",
      brand==5 ~ "Advil 50",
      brand==6 ~ "Advil 100",
      brand==7 ~ "Bayer 25",
      brand==8 ~ "Bayer 50",
      brand==9 ~ "Bayer 100",
      brand==10 ~ "Store brand 50",
      brand==11 ~ "Store brand 100"
    )
  )

# Calculating variables for the summary statistics table
data_final$total_sales=sum(data_final$count)/11

table <- data_final %>% 
  group_by(brand) %>% 
  reframe(
    mkt_share=(sum(sales)/total_sales)*100,
    unit_price=sum(price*sales)/sum(sales),
    wholesale_price=sum(cost*sales)/sum(sales),
    propn_promotion=(sum(sales*prom)/sum(sales))*100,
    brand_name=first(brand_name)
  ) %>% 
  distinct() %>% 
  mutate(brand=as.numeric(brand)) %>% 
  arrange(brand) %>% 
  mutate(across(where(is.numeric), round, 3))
  
total_mkt_share=sum(table$mkt_share)
outside_option =  round(100-total_mkt_share, 3)

# Adding an extra row to the table of market share
table[nrow(table) + 1,] <- list(NA, outside_option, NA, NA, NA, "Outside good")

# Final touches 
table <- table %>% 
  select(brand_name, brand, everything()) %>% 
  rename(
    Brand=brand,
    `Data ID`=brand_name,
    `Market share`=mkt_share,
    `Unit price` = unit_price,
    `Wholesale price` = wholesale_price,
    `% units sold during promotion`=propn_promotion
  ) 

stargazer(table, summary = F, rownames = T, out = paste0(out_dir,"/sumstats.tex")) 

# Export cleaned data
write.csv(data_final, paste0(data_dir,"/clean/data_clean.csv"), row.names=FALSE)













