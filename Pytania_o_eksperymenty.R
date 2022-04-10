library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(stringi)
library(stringr)

W <- data_99 %>% 
  filter(str_detect(columns, "W5")) %>% 
  mutate(etap1 = case_when(
    str_detect(columns,"W5_1") ~ "Czy pamiętasz?", 
    str_detect(columns,"W5_2") ~ "Czy spędziłeś\n przy nim co najmniej\n kilka minut?",
    str_detect(columns,"W5_3") ~ "Czy rozumiesz\n na jakiej zasadzie\n działa ten eksponat?",
    str_detect(columns,"W5_4") ~ "Jak ogólnie oceniasz\n ten eksponat?"
  )) %>% 
  group_by(etap1) %>% 
  summarise(srednia = round(mean(ile_99)/1598 * 100,1)) %>% 
  mutate(procent1 = paste(as.character(srednia),"%",sep = ''))

W %>% 
  ggplot(aes(x = forcats::fct_inorder(factor(etap1)), y = srednia)) +
  geom_col(fill = 'darkolivegreen4', width = 0.7) +
  geom_text(aes(label = procent1), nudge_y = 0.5, size = 7.5) +
  labs(y = 'Procent braków odpowiedzi', x = 'Pytania', title = 'Średni procent braków danych w pytaniach o eksponaty') +
  theme(text = element_text(size = 12))
