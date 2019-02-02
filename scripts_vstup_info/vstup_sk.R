library('stats')
library('dplyr')
library('stringr')
library('tidyverse')
library('tidyr')
library('magrittr')

vstup <- read.csv('/home/olena/vstup2018/vstup_info.csv', stringsAsFactors=FALSE, sep = ",")

head(vstup) %>% View()

# екстрактуємо інформацію з колонки з балами та коефіцієнтами

vstup <- vstup %>% 
  mutate(ukr=zno %>% str_extract("Українська мова та література.*") %>% str_extract('[0-9.]+')) %>% 
  mutate(math=zno %>% str_extract("Математика.*") %>% str_extract('[0-9.]+')) %>% 
  mutate(bio=zno %>% str_extract("Біологія.*") %>% str_extract('[0-9.]+')) %>%
  mutate(hist=zno %>% str_extract("Історія України.*") %>% str_extract('[0-9.]+')) %>%
  mutate(eng=zno %>% str_extract("Англійська мова.*") %>% str_extract('[0-9.]+')) %>%
  mutate(geo=zno %>% str_extract("Географія.*") %>% str_extract('[0-9.]+')) %>%
  mutate(him=zno %>% str_extract("Хімія.*") %>% str_extract('[0-9.]+')) %>%
  mutate(phiz=zno %>% str_extract("Фізика.*") %>% str_extract('[0-9.]+')) %>%
  mutate(inoz=zno %>% str_extract("Іноземна мова.*") %>% str_extract('[0-9.]+')) %>%
  mutate(sk=zno %>% str_extract("СК")) %>%
  mutate(rk=zno %>% str_extract("РК")) %>%
  mutate(gk=zno %>% str_extract("ГК")) %>%
  mutate(rk_2=quota %>% str_extract("РК")) %>% # в деяких рядках потрібні значення опинились в цих колонках
  mutate(sk_2=quota %>% str_extract("СК")) %>% 
  mutate(rk_3=link %>% str_extract("РК")) %>%
  mutate(sk_3=link %>% str_extract("СК"))

# відкидаємо тих, хто вступав не за результатами ЗНО

cols = c("ukr", "math", "bio", 'hist', 'eng', 'geo', 'him', 'phiz', 'inoz')
vstup[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
vstup$mark <- sapply(vstup$mark, as.numeric)

vstup_out <- vstup %>% filter(is.na(ukr) & is.na(math) & is.na(bio) & is.na(hist) & is.na(eng) & is.na(geo) & is.na(phiz) & 
                                is.na(him) & is.na(inoz))
vstup <- vstup %>% anti_join(vstup_out, by = c("ukr", "math", "bio", "hist", "eng", "geo", 
                                               "him", "phiz", "inoz"))
# фільтруємо вступників з СК

sk <- vstup %>%
  filter_all(any_vars(str_detect(., 'СК'))) 

# переносимо значення з коефіцієнтами в потрібні колонки

sum(!is.na(sk$sk))
sk <- sk %>%
  mutate(sk=ifelse(is.na(sk) & !is.na(sk_2), sk_2, sk)) %>%
  mutate(sk=ifelse(is.na(sk) & !is.na(sk_3), sk_3, sk))

sum(!is.na(sk$rk))
sk <- sk %>%
  mutate(rk=ifelse(is.na(rk) & !is.na(rk_2), rk_2, rk)) %>%
  mutate(rk=ifelse(is.na(rk) & !is.na(rk_3), rk_3, rk))

sk <- subset(sk, select=-c(rk_2, rk_3, sk_2, sk_3))
head(sk) %>% View()

# рахуємо унікальних (1 абітурієнт - 1 унікальна комбінація ім'я-бал ЗНО з укр мови-бал атестату)
unique <- sk %>% 
  select(name, ukr, school_ball) %>% distinct() 

unique(vstup$status) 

# фільтруємо зарахованих на навчання

sk_vstup <- rbind(
  filter(sk, is.na(status) | status == 'До наказу (бюджет)'), 
  filter(sk, is.na(status) | status == 'До наказу (контракт)'))  

# агрегуємо 

sk_vstup %>% group_by(univ) %>% count() %>% View()

sk_vstup %>% 
  group_by(galus) %>%
  # group_by(rk) %>%
  summarise (n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>%
  arrange(desc(n)) 
