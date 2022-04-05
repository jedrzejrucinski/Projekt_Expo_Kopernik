library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
kopernik <- read_csv("Projekt_eksplo/kopernik.csv")
kopernik <- kopernik %>% 
  distinct(id_ucznia, miejsc, .keep_all = TRUE)
roboty <- kopernik %>% 
  select(P4, P5, D18) %>% 
  filter(P4 %in% c(1,2)&  P5 %in% c(1,2) & D18 %in% c(1,2)) %>% 
  group_by(P4,P5,D18) %>% 
  summarise(liczba = n()) %>% 
  ungroup() %>% 
  group_by(P4,P5) %>%
  mutate(suma = sum(liczba)) %>% 
  mutate(stosunek = round((liczba/suma)*100,1)) %>% 
  arrange(-stosunek)
powtrki <- kopernik[duplicated(kopernik$id_ucznia),]

kopernik %>% 
  select(W5_3A:W5_3S) %>%
  filter(!is.na(W5_3A)&!is.na(W5_3S)) %>%
  filter(W5_3A %in% c(1,2) &  W5_3B %in% c(1,2)) %>% 
  group_by(W5_3A,W5_3B) %>% 
  summarise(liczba = n()) %>% 
  arrange(-liczba)

kopernik %>% 
  select(54, W2_4) %>% 
  rename(P23 = 1) %>% 
  filter(P23<7,W2_4<7) %>% 
  group_by(P23) %>% 
  summarise(srednia = mean(W2_4)) %>% 
  ggplot(mapping = aes(x = P23, y = srednia)) +
    geom_col(color = 'royalblue1', fill = 'royalblue1', width = 0.6)+
    labs(title = "Zainteresowanie mysleniem a zadowolenie z warsztatów", y= "Srednia", x = "Lubie poglówkowac, zeby znalezc rozwiazanie jakiegos problemu.")
#mniej wiecej to samo wychodzi dla P24    
kopernik %>% 
  select(58, W2_4) %>% 
  rename(P27 = 1) %>% 
  filter(P27<7,W2_4<7) %>% 
  group_by(P27) %>% 
  summarise(srednia = mean(W2_4)) %>% 
  ggplot(mapping = aes(x = P27, y = srednia)) +
  geom_col(color = 'royalblue1', fill = 'royalblue1', width = 0.6)+
  labs(title = "Zainteresowanie mysleniem a zadowolenie z warsztatów", y= "Srednia", x = "Zawsze chce wszystko dokladnie zrozumiec.")
#P27 daje bardziej wyrazne
  
kopernik %>% 
  select(klasa, 60) %>% 
  rename(P29 = 2) %>% 
  filter(P29 <7, klasa <9) %>% 
  ggplot(mapping = aes(group = klasa, x = klasa, y = P29))+
  geom_boxplot()+
  labs(title="Nauka przyda mi sie w przyszlosci",subtitle = "Odpowiedzi w zakresie od 1 do 5", x = 'Klasa', y = 'Odpowiedzi')
  
kopernik %>% 
  select(`P 41`,`P 44`,id_ucznia) %>% 
  rename(Lepiej_wygladac =1, Lepiej_umiec = 2) %>% 
  filter(Lepiej_wygladac<9, Lepiej_umiec<9) %>% 
  pivot_wider(names_from = 'id_ucznia', values_from = c('Lepiej_wygladac','Lepiej_umiec'))
  