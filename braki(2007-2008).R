library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(stringi)
library(stringr)



kopernik1 <- kopernik %>% 
  distinct(id_ucznia, miejsc, .keep_all = TRUE)
colnames(kopernik1) <- stri_replace_all_fixed(colnames(kopernik1), "P ", "P")

kopernik1 <- kopernik1 %>% 
  select(!weryfikator:woj_nr) %>% 
  select(!P45:P45k)

kopernik1 <- kopernik1 %>% 
  filter(str_detect(P2, "200[78]"))

data_99 <- data.frame(columns = colnames(kopernik1), ile_99 = rep(0, dim(kopernik1)[2]))

iter <- 1:dim(kopernik1)[2]
for (i in iter){
  data_99[i,2] <- kopernik1 %>% 
    select(i) %>% 
    rename(col1 = 1) %>% 
    filter(col1 == 99) %>% 
    count()
}
subs <- data_99 %>% 
  filter(ile_99 <= quantile(ile_99,probs = 0.96), row_number() <= n()-1) %>%
  mutate(ile_99 = 100*ile_99/dim(kopernik1)[1]) %>%
  mutate("prev_value" = lag(ile_99)) %>%
  filter(ile_99-prev_value > 1.5) %>% 
  select(columns,ile_99)


data_99 <- data_99 %>% 
  mutate(etap = case_when(
    str_detect(columns,"W5_") ~ "4", 
    str_detect(columns,"W") ~ "3",
    str_detect(columns,"D") ~ "2",
    str_detect(columns,"P") ~ "1"
  ))


g <- data_99 %>% 
  filter(ile_99 <= quantile(ile_99,probs = 0.96), row_number() <= n()-1) %>% 
  filter(!str_detect(columns, "P45")) %>% 
  mutate(ile_99 = 100*ile_99/dim(kopernik1)[1]) %>% 
  ggplot(aes(x = forcats::fct_inorder(factor(columns)), y = ile_99, fill = etap, color = etap))+
  geom_col()

g + labs(title = "Procent brak�w danych w pytaniach w ankiecie Naukobus", x = 'Pytania', y = 'Procent brak�w danych(%)')+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())