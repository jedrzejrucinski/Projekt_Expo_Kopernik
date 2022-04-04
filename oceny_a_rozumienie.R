install.packages('janitor')
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)



kopernik %>% 
  select(id_ucznia, starts_with('W5_3')) -> y
y <- t(y)
y <- as_data_frame(y)
names(y) <- lapply(y[1, ], as.character)
y <- y[-1,] 
duplicated(colnames(y), fromLast = TRUE) -> dopy
y <- y[!dopy]
y <- as.data.frame(sapply(y,as.numeric))
y[y == 99 | y == 88] <- 0
sumy <- as.data.frame(colSums(y))
sumy <- tibble::rownames_to_column(sumy)
colnames(sumy) <- c('id_ucznia','laczne_rozum')



kopernik %>% 
  select(id_ucznia,P10_M:P10F) -> ocenki
ocenki[ocenki == 99] <- 0
ocenki <- t(ocenki)
ocenki <- as.data.frame(ocenki)
names(ocenki) <- lapply(ocenki[1,],as.character)
ocenki <- ocenki[-1,]
duplicated(colnames(ocenki), fromLast = TRUE) -> dopy2
ocenki <- ocenki[!dopy2]
ocenki <- as.data.frame(sapply(ocenki,as.numeric))
srednie_oceny <- as.data.frame(colMeans(ocenki))
srednie_oceny <- tibble::rownames_to_column(srednie_oceny)
colnames(srednie_oceny) <- c('id_ucznia','srednia_ocen')

oceny_rozum <- inner_join(srednie_oceny,sumy)

ggplot(data = oceny_rozum, mapping = aes(srednia_ocen, laczne_rozum)) + geom_point()