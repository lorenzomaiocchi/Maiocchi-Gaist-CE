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



### ---- 
####BIVARIATE RELATIONSHIP



df_nona %>% 
  ggplot(aes(trust_inst.index, color = polintr_cat))+
  geom_density()+
  theme_classic()

##

#general trust index /boxpolot

boxplot_ind = df_nona %>% 
  ggplot(aes(polintr_cat, trust_inst.index, color = polintr_cat))+
  geom_boxplot()+
  theme_classic()+
  xlab('')+
  theme(legend.position = 'none')+
  ylab('Trust Mean Index')+
  theme(axis.text.x = element_text(size = 10, face = 'bold'))

ggsave('boxplot_index_polintr.png',plot = boxplot_ind, path = "./plots/")



#result regression

coef_name = c('Intercept (Very interested)', 'Quite interested', 'Hardly interested', 'Not at all interested', 'No answer')

reg = lm(data = df_nona, trust_inst.index ~ polintr_cat)

screenreg(reg, custom.coef.names = coef_name)

