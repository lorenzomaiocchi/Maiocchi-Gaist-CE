library(tidyverse)
library(texreg)

#Plot of trust



df_nona %>% 
  ggplot(aes(trstlgl))+
  geom_bar()+
  scale_x_continuous(breaks = 0:10)

df_nona %>% 
  ggplot(aes(trstplc))+
  geom_bar()+
  scale_x_continuous(breaks = 0:10)

df_nona %>% 
  ggplot(aes(trstprt))+
  geom_bar()+
  scale_x_continuous(breaks = 0:10)


df_nona %>% 
  ggplot(aes(trstprl))+
  geom_bar()+
  scale_x_continuous(breaks = 0:10)


df_nona %>% 
  ggplot(aes(polintr))+
  geom_bar()+
  scale_x_continuous(breaks = 0:10)

#index


df_nona %>% 
  ggplot(aes(trust_inst.index))+
  geom_density()


#how this index changes accross the (NAs are included)

#general trust index

df_nona %>% 
  ggplot(aes(polintr_cat, trust_inst.index))+
  geom_boxplot()



df_nona %>% 
  ggplot(aes(polintr_cat, trstlgl))+
  geom_boxplot()


#result regression


reg = lm(data = df_nona, trust_inst.index ~ polintr_cat)

screenreg(reg)

