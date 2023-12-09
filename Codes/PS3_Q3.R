rm(list=ls())

library(dplyr)
library(tidyverse)
library(haven)
library(readr)
library(readxl)
library(stargazer)
library(tidyr)
library(data.table)
library(ivreg)
library(modelsummary)

#setwd("Dropbox/PhD_Course/Applied_Micro/PS3")

# The code should be based on the previous 2 parts

base_dir <- getwd()
data_dir <- paste0(base_dir,"/Data")
out_dir <- paste0(base_dir,"/Output")

# Import the table in Question 1
table_temp <- table[1:11,] %>% dplyr::select(1,3,4)
names(table_temp) <- c("Brand","Market_Share","Unit_Price")

# Calculate elasticity

# (a)
table_temp$elasticity1 <- round(model_1$coefficients[2]*table_temp$Unit_Price*(1-table_temp$Market_Share),digits = 3)
table_temp$elasticity2 <- round(model_2$coefficients[2]*table_temp$Unit_Price*(1-table_temp$Market_Share),digits = 3)
table_temp$elasticity3 <- round(model_3$coefficients[2]*table_temp$Unit_Price*(1-table_temp$Market_Share),digits = 3)

# (b)
table_temp$elasticity4 <- round(model_4$coefficients[2]*table_temp$Unit_Price*(1-table_temp$Market_Share),digits = 3)
table_temp$elasticity5 <- round(model_5$coefficients[2]*table_temp$Unit_Price*(1-table_temp$Market_Share),digits = 3)
table_temp$elasticity6 <- round(model_6$coefficients[2]*table_temp$Unit_Price*(1-table_temp$Market_Share),digits = 3)
table_temp$elasticity7 <- round(model_7$coefficients[2]*table_temp$Unit_Price*(1-table_temp$Market_Share),digits = 3)
table_temp$elasticity8 <- round(model_8$coefficients[2]*table_temp$Unit_Price*(1-table_temp$Market_Share),digits = 3)
table_temp$elasticity9 <- round(model_9$coefficients[2]*table_temp$Unit_Price*(1-table_temp$Market_Share),digits = 3)

# (c)
table_temp$cross_elasticity <- round(model_9$coefficients[2]*table_temp$Unit_Price*table_temp$Market_Share,digits = 3)

# Report the Table for Question (a)
table_temp %>% select(`Brand`,"(i)"=elasticity1,"(ii)"=elasticity2,"(iii)"=elasticity3) %>% stargazer(summary = F, rownames = F )

# Report the Table for Question (b)
table_temp %>% select(`Brand`,"(iv)"=elasticity4,"(v)"=elasticity5,"(vi)"=elasticity6) %>% stargazer(summary = F, rownames = F )
table_temp %>% select(`Brand`,"(vii)"=elasticity7,"(viii)"=elasticity8,"(ix)"=elasticity9) %>% stargazer(summary = F, rownames = F )

# Report the Table for Question (c)
table_elasticity <- data.frame("Brand"=c(table_temp$Brand[2],table_temp$Brand[5],table_temp$Brand[8]),
                               "c2"=c(table_temp$elasticity9[2],table_temp$cross_elasticity[5],table_temp$cross_elasticity[8]),
                               "c3"=c(table_temp$cross_elasticity[2],table_temp$elasticity9[5],table_temp$cross_elasticity[8]),
                               "c4"=c(table_temp$cross_elasticity[2],table_temp$cross_elasticity[5],table_temp$elasticity9[8])) %>% 
  setNames(c("Brand",table_temp$Brand[2],table_temp$Brand[5],table_temp$Brand[8])) %>% 
  stargazer(summary = F, rownames = F)
