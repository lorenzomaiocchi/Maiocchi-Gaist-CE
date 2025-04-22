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
                ~ if_else(. > 70, NA, .)))


labels_cut = c('Very interested', 'Quite interested', 'Hardly interested', 'Not at all interested', 'No answer')



#make a cetogorical of the variables.

df_nona <- df_nona %>%
  mutate(
    polintr_cat = cut(polintr,
                      breaks = c(0, 1.5, 2.5, 3.5, 4.5, Inf),
                      labels = labels_cut,
                      right = FALSE))

table(df_nona$polintr_cat)

df_nona = df_nona %>% 
  mutate(gender = recode(gndr, `1` = 'M', `2` = 'F'))



#quick check.
table(df_nona$trstplc)

#create a small mean index for institutional trust based on "trst..."

df_nona$trust_inst.index = round(rowSums(df_nona[, c("trstprt", "trstlgl", "trstplc", "trstprl")], na.rm = TRUE) / 4,2)


