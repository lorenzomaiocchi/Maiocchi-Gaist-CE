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


density_graph = df_nona %>% 
  ggplot(aes(trust_inst.index))+
  geom_density()+
  theme_classic()
  
ggsave('density_plot_index.png',plot = density_graph, path = "./plots/")


#how this index changes accross the (NAs are included)

#general trust index

df_nona %>% 
  ggplot(aes(polintr_cat, trust_inst.index))+
  geom_boxplot()



df_nona %>% 
  ggplot(aes(polintr_cat, trstlgl))+
  geom_boxplot()


#result regression

coef_name = c('Intercept (Very interested)', 'Quite interested', 'Hardly interested', 'Not at all interested', 'No answer')

reg = lm(data = df_nona, trust_inst.index ~ polintr_cat)

screenreg(reg, custom.coef.names = coef_name)

