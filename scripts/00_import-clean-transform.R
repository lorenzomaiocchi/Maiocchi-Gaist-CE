library(tidyverse)
library(rio)

#importing data  (might require to create a new folder in directory)

options(scipen = 999)

df = rio::import('../data/ESS11.csv')

#extrapolate just Italy and select the variables of interest.

df =  df %>% 
  filter(cntry == 'IT') %>% 
  select(cntry, gndr, polintr, trstlgl, trstplc,
         trstprt, trstprl)


table(df$trstlgl)

#70 --> refusal// 88 --< don't known, 9 --> no answer





## ---- DATAFRAME WITHOUT NAs

df_nona = df %>% 
  mutate(across(c("trstlgl", "trstplc", "trstprt", "trstprl"),
                ~ if_else(. > 70, NA, .)
                
                ))


labels_cut = c('Not interested', 'Neutral', 'Interested')

#make a cetogorical of the variables.

df_nona = df_nona %>% 
  mutate(
    trstlgl_cat = cut(trstlgl, breaks = c(0, 5, 7, 10), labels = labels_cut),
    trstplc_cat = cut(trstplc, breaks = c(0, 5, 7, 10), labels = labels_cut),
    trstprt_cat = cut(trstprt, breaks = c(0, 5, 7, 10), labels = labels_cut),
    trstprl_cat = cut(trstprl, breaks = c(0, 5, 7, 10), labels = labels_cut))
    



#quick check.
table(df_nona$trstplc)

#create a small mean index for institutional trust based on "trst..."

df_nona$trust_inst.index = round(rowSums(df_nona[, c("trstprt", "trstlgl", "trstplc", "trstprl")], na.rm = TRUE) / 4,2)



## ---- DATAFRAME WITH NAs


df_filtered = df %>% 
  filter(trstlgl > 70 & trstplc > 70 & trstprt > 70 & trstprl > 70)

#relabel Nas.

df_filtered = df_wna %>%
  mutate(across(c("trstlgl", "trstplc", "trstprt", "trstprl"),
                ~ recode(.,
                         `77` = 'Refusal',
                         `88` = 'Don\'t know',
                         `90` = 'No answer')))


