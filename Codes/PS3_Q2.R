
rm(list=ls())

pacman::p_load("tidyverse","dplyr","data.table",
               "ivreg","modelsummary","stargazer")

base_dir <- getwd()
data_dir <- paste0(base_dir,"/Data")
out_dir <- paste0(base_dir,"/Output")

data <- fread(paste0(data_dir,"/clean/data_clean.csv"))

# data <- data %>% 
#   group_by(brand) %>% 
#   mutate(mkt_share=(sum(sales)/total_sales)*100)%>%
#   ungroup()%>%
#   mutate(total_mkt_share = sum(mkt_share),
#          outside = round(100-total_mkt_share,3))
         
      
data_reg <- data %>%
  mutate(u_ijt = log(sales / count)) 

# 2(a)
# Model 1
model1 <- lm(u_ijt ~ price + prom, data = data_reg)

# 2(b)
# Model 2
model2 <- lm(u_ijt ~ price + prom + factor(brand), data = data_reg)

# 2(c)
# Model 3
model3 <-  lm(u_ijt ~ price + prom + factor(store*brand), data = data_reg)

mdlatoc <- msummary(list("(i)"=model1, "(ii)"=model2,"(iii)"=model3), coef_omit = "factor", gof_omit = ".*",stars = TRUE,output = "Output/model_summary1.tex")

# part (d),(e),(f)

# 2(d)
model_4 = ivreg(u_ijt ~ price + prom | cost + prom, data = data_reg)
model_5 = ivreg(u_ijt ~ price + prom + factor(brand) | cost  + prom + factor(brand), data = data_reg)
model_6 = ivreg(u_ijt ~ price + prom + factor(store*brand) | cost  + prom + factor(store*brand), data = data_reg)

mdld <- msummary(list("(iv)"=model_4, "(v)"=model_5,"(vi)"=model_6), coef_omit = "factor|Intercept", gof_omit = ".*",stars = TRUE,output = "Output/model_summary2.tex")


# 2(e)
data_reg <- data_reg %>%
  group_by(brand) %>%
  mutate(hausman = (sum(price) - price) / (n()-1))


model_7 = ivreg(u_ijt ~ price + prom | hausman + prom, data = data_reg)
model_8 = ivreg(u_ijt ~ price + prom + factor(brand) | hausman + prom + factor(brand), data = data_reg)
model_9 = ivreg(u_ijt ~ price + prom + factor(store*brand) | hausman + prom + factor(store*brand), data = data_reg)

mdlde <- msummary(list("(vii)"=model_7, "(viii)"=model_8, "(ix)"=model_9), coef_omit = "factor|Intercept", gof_omit = ".*",stars = TRUE,output = "Output/model_summary3.tex")

