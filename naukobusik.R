library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(stringi)
library(stringr)
kopernik <- read_csv("Projekt_eksplo/kopernik.csv")
kopernik <- kopernik %>% 
  distinct(id_ucznia, miejsc, .keep_all = TRUE)
colnames(kopernik) <- stri_replace_all_fixed(colnames(kopernik), "P ", "P")

kopernik <- kopernik %>% 
  select(!weryfikator:woj_nr) %>% 
  select(!P45:P45k)

data_99 <- data.frame(columns = colnames(kopernik), ile_99 = rep(0, dim(kopernik)[2]))

iter <- 1:dim(kopernik)[2]
for (i in iter){
  data_99[i,2] <- kopernik %>% 
    select(i) %>% 
    rename(col1 = 1) %>% 
    filter(col1 == 99) %>% 
    count()
}
subs <- data_99 %>% 
  filter(ile_99 <= quantile(ile_99,probs = 0.96), row_number() <= n()-1) %>%
  mutate(ile_99 = 100*ile_99/dim(kopernik)[1]) %>%
  mutate("prev_value" = lag(ile_99)) %>%
  filter(ile_99-prev_value > 1.5) %>% 
  select(columns,ile_99)




g <- data_99 %>% 
  filter(ile_99 <= quantile(ile_99,probs = 0.96), row_number() <= n()-1) %>% 
  filter(!str_detect(columns, "P45")) %>% 
  mutate(ile_99 = 100*ile_99/dim(kopernik)[1]) %>% 
  ggplot(aes(x = forcats::fct_inorder(factor(columns)), y = ile_99))+
  geom_col(fill = 'royalblue1', color = 'royalblue1')+
  geom_text(data = subs, aes(label = columns), size = 3,nudge_y = 0.5)

g + labs(title = "Procent braków danych w pytaniach w ankiecie Naukobus", x = 'Pytania', y = 'Procent braków danych(%)')+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


