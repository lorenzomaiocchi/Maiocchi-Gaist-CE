library(tidyverse)
library(texreg)

#Plot of trust to see the distribiutions



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



#plot of the index  of trust


density_graph = df_nona %>% 
  ggplot(aes(trust_inst.index))+
  geom_density(fill = '#86bbd8', color = 'white', alpha = 0.8)+
  theme_classic()+
  ylab('Density')+
  xlab('Trust Mean Index')+
  labs(title = 'Distribution of Trust Mean Index')

ggsave('density_plot_index.png',plot = density_graph, path = "./plots/")

#graph to  see wheter there a genre differences.

df_nona %>% 
  ggplot(aes(trust_inst.index, color = polintr_cat))+
  geom_density()+
  theme_classic()
  



#how this index changes accross the (NAs are included)

#bivariate relation; is there a relationship between Interest in politics and Trust in Instituions 

#general trust index

df_nona %>% 
  ggplot(aes(polintr_cat, trust_inst.index, color = polintr_cat))+
  geom_boxplot()+
  theme_classic()+
  xlab('')+
  theme(legend.position = 'none')



#result regression

coef_name = c('Intercept (Very interested)', 'Quite interested', 'Hardly interested', 'Not at all interested', 'No answer')

reg = lm(data = df_nona, trust_inst.index ~ polintr_cat)

screenreg(reg, custom.coef.names = coef_name)

