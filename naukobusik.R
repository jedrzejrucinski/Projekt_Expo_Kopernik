library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
kopernik <- read_csv("kopernik.csv")
kopernik <- kopernik %>% 
  distinct(id_ucznia, miejsc, .keep_all = TRUE)


kopernik <- kopernik %>% 
  select(!weryfikator:woj_nr)
kopernik %>% 
  filter_all(any_vars(. %in% c(99,'99')))

data <- data.frame(columns = colnames(kopernik), ile_99 = rep(0, 195))

iter <- 1:dim(kopernik)[2]
for (i in iter){
  data[i,2] <- kopernik %>% 
    select(i) %>% 
    rename(col1 = 1) %>% 
    filter(col1 == 99) %>% 
    count()
}
data %>% 
  filter(ile_99 <2000) %>% 
  ggplot(aes(x = columns, y = ile_99))+
  geom_col()


