library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(stringi)
library(stringr)


max_braki <- data_99 %>% 
  arrange(desc(ile_99)) %>% 
  slice(1:8) %>% 
  arrange(ile_99) %>% 
  mutate(pytanie = c('Zawód wykonywany\n przez matkę?',
                     'Ocena\n z biologii',
                     'Czego się\n dowiedziałeś\n - w trzech zdaniach',
                     'Chciałbyś\n się o coś\n dopytać?',
                     'Ocena\n z przyrody',
                     'Ocena \n z fizyki',
                     'Ocena\n z chemii',
                     'Z kim innym\n rozmawiasz o nauce?'
                     )) %>% 
  mutate(procent = round(ile_99/1598 * 100,1)) %>% 
  mutate(procent1 = paste(as.character(procent),"%",sep = ''))

max_braki %>% 
  ggplot(aes(x = forcats::fct_inorder(factor(pytanie)), y = procent)) +
  geom_col(fill = 'royalblue1', width = 0.7) +
  geom_text(aes(label = procent1), nudge_y = 4, size = 7.5) +
  labs(y = 'Procent braków odpowiedzi', x = 'Pytania', title = 'Najbardziej omijane pytania') +
  theme(text = element_text(size = 12))
  
