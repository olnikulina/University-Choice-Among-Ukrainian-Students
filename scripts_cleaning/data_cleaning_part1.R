library(tidyverse)
library(stats)
library(dplyr)
library(tidyr)
library(stringr)
library(stringr)
library(stringi)


colnames(edebo_2018) <- c('id_abit', 'id_univ', 'univ', 'id_galus', 'galus', 'id_spec', 'spec', 'zno_1', 'ukr_bal', 
                              'zno_2', 'zno_2_bal', 'zno_3', 'zno_3_bal', 'region', 'raion', 'place', 'place_type', 'school',
                              'school_oblast', 'school_raion', 'school_location')

# clean schools -----------------------------------------------------------
edebo_2018$school <- edebo_2018$school %>% 
  str_replace_joined_with_schools('A', 'А') %>%
  str_replace_joined_with_schools('B', 'В') %>%
  str_replace_joined_with_schools('C', 'С') %>%
  str_replace_joined_with_schools('E', 'Е') %>%
  str_replace_joined_with_schools('H', 'Н') %>%
  str_replace_joined_with_schools('I', 'І') %>% # mega
  str_replace_joined_with_schools('K', 'К') %>%
  str_replace_joined_with_schools('M', 'М') %>%
  str_replace_joined_with_schools('O', 'О') %>%
  str_replace_joined_with_schools('P', 'Р') %>%
  str_replace_joined_with_schools('T', 'Т') %>%
  str_replace_joined_with_schools('X', 'Х') %>%
  str_replace_joined_with_schools('Y', 'У')

edebo_2018$school <- edebo_2018$school %>% 
  str_to_upper() %>% 
  str_replace_joined_with_schools('[[:punct:]=+|$^]', ' ') %>% 
  str_replace_joined_with_schools('\\s+', ' ')

edebo_2018$school <- str_c(' ', edebo_2018$school, ' ') %>% 
  str_replace_joined_with_schools('L', 'І') %>% # використовують замість римської одиниці
  str_replace_joined_with_schools('N', '№') %>% # номер
  str_replace_joined_with_schools('№\\s+', '№') %>% # мега (+ ~ 1000)
  str_replace_joined_with_schools('^ ЗАКЛАД ', ' ') %>% 
  str_replace_joined_with_schools('^ ОПОРНИЙ ЗАКЛАД ', ' ') %>% 
  str_replace_joined_with_schools(' СЕЛА ', ' С ') %>% 
  str_replace_joined_with_schools(' ЗОШ ', ' ЗАГАЛЬНООСВІТНЯ ШКОЛА ') %>% 
  str_replace_joined_with_schools(' НВК ', ' НАВЧАЛЬНО ВИХОВНИЙ КОМПЛЕКС ') %>% 
  str_replace_joined_with_schools(' ЗНЗ ', ' ЗАГАЛЬНООСВІТНІЙ НАВЧАЛЬНИЙ ЗАКЛАД ') %>% 
  str_replace_joined_with_schools(' ДНЗ ', ' ДОШКІЛЬНИЙ НАВЧАЛЬНИЙ ЗАКЛАД ') %>% 
  str_replace_joined_with_schools(' КЗ ', ' КОМУНАЛЬНИЙ ЗАКЛАД ') %>% 
  str_replace_joined_with_schools(' РКЗО ', ' РАЙОННИЙ КОМУНАЛЬНИЙ ЗАКЛАД ОСВІТИ ') %>% 
  str_replace_joined_with_schools('^ РАЙОННИЙ КОМУНАЛЬНИЙ ЗАКЛАД ОСВІТИ ', ' ') %>% 
  str_replace_joined_with_schools('^ КОМУНАЛЬНИЙ ЗАКЛАД ОСВІТИ ', ' ') %>% 
  str_replace_joined_with_schools('^ КОМУНАЛЬНИЙ ЗАКЛАД ', ' ') %>%
  str_replace_joined_with_schools(' СЗШ ', ' СЕРЕДНЯ ЗАГАЛЬНООСВІТНЯ ШКОЛА ') %>%
  str_replace_joined_with_schools(' СШ ', ' СПЕЦІАЛІЗОВАНА ШКОЛА ') %>%
  str_replace_joined_with_schools(' СШІ ', ' СПЕЦІАЛІЗОВАНА ШКОЛА ІНТЕРНАТ ') %>% #nothing
  str_replace_joined_with_schools(' ІМ ', ' ІМЕНІ ') %>% # mega
  str_replace_joined_with_schools(' ІСТ ', ' І СТ ') %>%
  str_replace_joined_with_schools(' ІІСТ ', ' ІІ СТ ') %>%
  str_replace_joined_with_schools(' ІІІСТ ', ' ІІІ СТ ') %>%
  str_replace_joined_with_schools(' І ІІ СТ ', ' І ІІ СТУПЕНІВ ') %>%
  str_replace_joined_with_schools(' І ІІІ СТ ', ' І ІІІ СТУПЕНІВ ') %>%
  str_replace_joined_with_schools(' І СТ ', ' І СТУПЕНЯ ') %>%
  str_replace_joined_with_schools(' ІІ СТ ', ' ІІ СТУПЕНЯ ') %>%
  str_replace_joined_with_schools(' ІІІ СТ ', ' ІІІ СТУПЕНЯ ') %>%
  
  str_replace_joined_with_schools(' ІСТУПЕНЯ ', ' І СТУПЕНЯ ') %>%
  str_replace_joined_with_schools(' ІІСТУПЕНЯ ', ' ІІ СТУПЕНЯ ') %>%
  str_replace_joined_with_schools(' ІІІСТУПЕНЯ ', ' ІІІ СТУПЕНЯ ') %>%
  str_replace_joined_with_schools(' І ІІСТУПЕНІВ ', ' І ІІ СТУПЕНІВ ') %>%
  str_replace_joined_with_schools(' І ІІІСТУПЕНІВ ', ' І ІІІ СТУПЕНІВ ') %>%
  
  str_replace_joined_with_schools(' ІСТУПЕНІ ', ' І СТУПЕНЯ ') %>%
  str_replace_joined_with_schools(' ІІСТУПЕНІ ', ' ІІ СТУПЕНЯ ') %>%
  str_replace_joined_with_schools(' ІІІСТУПЕНІ ', ' ІІІ СТУПЕНЯ ') %>%
  str_replace_joined_with_schools(' І ІІСТУПЕНІ ', ' І ІІ СТУПЕНІВ ') %>%
  str_replace_joined_with_schools(' І ІІІСТУПЕНІ ', ' І ІІІ СТУПЕНІВ ') %>%
  
  str_replace_joined_with_schools(' 1 3 СТУПЕНІ ', ' І ІІІ СТУПЕНІВ ') %>%
  str_replace_joined_with_schools(' 1 3 СТУПЕНІВ ', ' І ІІІ СТУПЕНІВ ') %>%
  str_replace_joined_with_schools(' 1 3 СТУПЕНЯ ', ' І ІІІ СТУПЕНІВ ') %>%
  
  str_replace_joined_with_schools(' ОБЛ\\s+$', ' ОБЛАСТІ ') %>% 
  
  #str_replace_joined_with_schools('\\s.\\s', ' ') %>% 
  str_replace_joined_with_schools('\\s+', ' ') %>% 
  str_trim()

edebo_2018$school <- str# %>% 
#str_to_upper() %>% 
#str_replace_joined_with_schools('[[:punct:]=+|$^]', ' ') %>% 
#str_replace_joined_with_schools('\\s+', ' ')

edebo_2018$school <- str_c(' ', edebo_2018$school, ' ') %>% 
  str_replace_joined_with_schools(' МІСЬКОЇ РАДИ ', ' РАЙОННОЇ РАДИ ') %>% 
  
  str_replace_joined_with_schools('\\s+', ' ') %>% 
  str_trim()


# separate zno results for each subject -----------------------------------
edebo_2018 <- unite(edebo_2018, 'zno', select = c('zno_1', 'ukr_bal', 'zno_2', 'zno_2_bal', 'zno_3', 'zno_3_bal'), sep = '/', remove = TRUE)

edebo_2018 <- edebo_2018 %>% 
  mutate(ukr=zno %>% str_extract("Українська мова та література.*") %>% str_extract('[0-9.]+')) %>% 
  mutate(math=zno %>% str_extract("Математика.*") %>% str_extract('[0-9.]+')) %>% 
  mutate(bio=zno %>% str_extract("Біологія.*") %>% str_extract('[0-9.]+')) %>%
  mutate(hist=zno %>% str_extract("Історія України.*") %>% str_extract('[0-9.]+')) %>%
  mutate(eng=zno %>% str_extract("Англійська мова.*") %>% str_extract('[0-9.]+')) %>%
  mutate(geo=zno %>% str_extract("Географія.*") %>% str_extract('[0-9.]+')) %>%
  mutate(him=zno %>% str_extract("Хімія.*") %>% str_extract('[0-9.]+')) %>%
  mutate(phiz=zno %>% str_extract("Фізика.*") %>% str_extract('[0-9.]+')) %>%
  mutate(inoz=zno %>% str_extract("Іноземна мова.*") %>% str_extract('[0-9.]+'))

library(magrittr)
cols = c("ukr", "math", "bio", "hist", "eng", "geo", "him", "phiz", "inoz")
edebo_2018[,cols] %<>% lapply(function(x) as.integer(as.character(x)))


# check people who entered without zno ------------------------------------

#1 do not have any zno marks
new_edebo_out <- edebo_2018 %>% filter(is.na(ukr) & is.na(math) & is.na(bio) & is.na(hist) & is.na(eng) & is.na(geo) & is.na(phiz) & 
                                             is.na(him) & is.na(inoz))

#3 outliers and obvious errors in zno marks
new_edebo_out <- edebo_2018 %>% filter(ukr  > 200 | ukr < 100 | math  > 200 | math < 100 | bio  > 200 |
                                             bio < 100 | hist  > 200 | hist  < 100 | eng  > 200 | eng  < 100 | geo  > 200 | 
                                             geo  < 100 | phiz  > 200 | phiz  < 100 | him  > 200 | him < 100 | inoz  > 200 | inoz < 100)

#2, #4
edebo_2018 <- edebo_2018 %>% anti_join(new_edebo_out, by = c("ukr", "math", "bio", "hist", "eng", "geo", 
                                                                     "him", "phiz", "inoz"))
# data cleaning from foreign entrants -------------------------------------
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ІРАН', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | school != 'ТУРЕЧЧИНА')
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | school != '-')
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | school != 'ІЗРАЇЛЬ')
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | school != 'МАРОККО')
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | school != 'НІГЕРІЯ')
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | school != 'УЗБЕКИСТАН')
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ЗАХІДНОАФРИКАНСЬКА', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('МІНІСТЕРСТВО ОСВІТИ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ІНДІЯ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ТУРКМЕНІСТАН', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('БАКУ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ЕКЗАМЕНАЦІЙНА', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ГОМЕЛЬ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ТБІЛІСІ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('БЕНДЕРИ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ТИМЧАСОВО', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ГІТАНДЖАЛІ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('НЬЮ ЙОРК', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ДЕРЖАВНИЙ ЗАКЛАД ОСВІТИ СЕРЕДНЯ ШКОЛА', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ДЗО СЕРЕДНЯ ШКОЛА', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ДЕРЖАВНА УСТАНОВА ОСВІТИ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ДУШАНБЕ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('КРАСНОЯРСЬК', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('БУСТОН', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ВЕЛАЯТ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ХИРДАЛАН', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('МАРИ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('МІНСЬК', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('БРЯНКИЛУГАНСЬКОЇ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('БАЛКАНСЬКОЇ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('БРЕСТ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ГОМЕЛ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ТУРКМЕНАБАТ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('МАГДАНЛИ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ЕТРАПУ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('УЛААНБААТАР', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('АШХАБАД', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('СЕЙДІ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('САБІРАБАД', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('КЮРДАМИР', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ДЖАЛІЛАБАДА', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('РОСІЙСЬКА ФЕДЕРАЦІЯ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('АШХАБАД', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('КАРАБАЛТА', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ВОРОШИЛОВГРАД', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('АГДЖАБАД', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('КАРАБАЛТА', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('АНАТОЛІЇВСЬКИЙ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('АНАТОЛІЙСЬКІЙ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('БАКИНСЬКИЙ', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('АХІ ЕВРАН', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ДЕРЖАВНИЙ БЮДЖЕТНИЙ ЗАГАЛЬНООСВІТНІЙ ЗАКЛАД', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('ДЕРЖАВНА БЮДЖЕТНА ЗАГАЛЬНООСВІТНЯ УСТАНОВА', edebo_2018$school))
edebo_2018 <- edebo_2018 %>% filter(is.na(school) | !grepl('СЕОНІ', edebo_2018$school))
# additional 
edebo_2018 <- edebo_2018 %>%  filter(!grepl('СЕРЕДНЯ ШКОЛА АМІН', school)) %>% 
  filter(!grepl('СЕРЕДНЯ ШКОЛА ТУБА', school)) %>% 
  filter(!grepl('СЕРЕДНЯ ШКОЛА ОФОК', school)) %>% 
  filter(!grepl('СЕРЕДНЯ ШКОЛА ЯФІЯ', school))
# cleaning from foreign entrants (obvious errors) -------------------------
#1# 
new_edebo_out <- edebo_2018 %>% 
  filter(str_detect(school, '^[0-9]*\\s') == TRUE) 

#3# 
new_edebo_out <- edebo_2018 %>% 
  filter(str_detect(school, '^№\\w+') == TRUE) 

#5#
new_edebo_out <- edebo_2018 %>% 
  filter(str_detect(school, 'SСНООІ') == TRUE) 

#2,4,6#
edebo_2018 <- edebo_2018 %>% anti_join(new_edebo_out, by = c("school"))

# cleaning from crimea, donetzk and foreign -----------------------------------------------------------------------
new_edebo_out <- edebo_2018 %>% filter(str_detect(school, 'ЯЛТИ') == TRUE|
                                             str_detect(school, 'ЯЛТА') == TRUE|
                                             str_detect(school, 'СОКІЛЬНИКИ') == TRUE|
                                             str_detect(school, 'МІСТА ДОНЕЦЬК') == TRUE|
                                             str_detect(school, 'СЕРДАР') == TRUE|
                                             str_detect(school, 'ЄКАТЕРИНБУРГ') == TRUE|
                                             str_detect(school, 'ЧАОЯН') == TRUE|
                                             str_detect(school, 'АЗЕРБАЙДЖАН') == TRUE|
                                             str_detect(school, 'КІЛАМБА') == TRUE|
                                             str_detect(school, 'КРИМ') == TRUE|
                                             str_detect(school, 'ЧІТИ') == TRUE|
                                             str_detect(school, 'ЧИШМИ') == TRUE|
                                             str_detect(school, 'ЛУГАНСЬКА') ==TRUE |
                                             str_detect(school, 'М ЛУГАНСЬК') ==TRUE |
                                             str_detect(school, 'М ДОНЕЦЬК') ==TRUE |
                                             str_detect(school, 'КАРНАНС') ==TRUE |
                                             str_detect(school, 'НАГАР') ==TRUE |
                                             str_detect(school, 'РИБНИЦЬКА') ==TRUE |
                                             str_detect(school, 'АНАТОЛІЙСЬКИЙ') ==TRUE |
                                             str_detect(school, 'БАХЧИСАРАЙСЬКА') ==TRUE |
                                             str_detect(school, 'ДОНЕЦЬКИЙ УНІВЕРСИТЕТ') ==TRUE |
                                             str_detect(school, 'СКУЛ') ==TRUE |
                                             str_detect(school, 'СЕВАСТОПОЛЬ') ==TRUE |
                                             str_detect(school, 'МУНИЦИП') ==TRUE |
                                             str_detect(school, 'ДОНЕЦЬКЕ') ==TRUE)
new_edebo_out <- edebo_2018 %>% 
  filter(str_detect(school, '^ДОНЕЦЬКИЙ') == TRUE)

new_edebo_out <- edebo_2018 %>% 
  filter(str_detect(school, '^ЛУГАНСЬК') == TRUE)

new_edebo_out <- edebo_2018 %>% 
  filter(str_detect(school, '^ДОНЕЦЬКА') == TRUE)

new_edebo_out <- edebo_2018 %>% filter(str_detect(school, 'МОСКОВСЬКОЇ') == TRUE|
                                             str_detect(school, 'НИЖНЬОГОРОДСЬКОЇ') == TRUE|
                                             str_detect(school, 'КРИМ') == TRUE|
                                             str_detect(school, 'ГРОДНЕНСЬКОЇ') == TRUE|
                                             str_detect(school, 'РОСТОВСЬКОЇ') == TRUE|
                                             str_detect(school, 'ВОРОШИЛОВСЬКОЇ') == TRUE|
                                             str_detect(school, 'САМАРКАНДСЬКОЇ') == TRUE)
# steps 2, 4, 6, 8
edebo_2018 <- edebo_2018 %>% anti_join(new_edebo_out, by = 'school') 


# clean after joined with schools -----------------------------------------

# joined_with_schools - prepared df joined with schools

joined_with_schools <- joined_with_schools %>% mutate(place=ifelse(place == 'КРИВИЙ' & school_raion == 'КРИВИЙ РІГ', school_raion, place))

unique(joined_with_schools$school_oblast)
joined_with_schools$school_oblast <- toupper(joined_with_schools$school_oblast)
joined_with_schools$place <- toupper(joined_with_schools$place)

unique(joined_with_schools$place_type)
joined_with_schools <- joined_with_schools %>% 
  mutate(place_type=ifelse(place_type == 'смт' | place_type == 'селище', 'село', place_type))

sum(is.na(joined_with_schools$place))
sum(is.na(joined_with_schools$place_type))

joined_with_schools <- joined_with_schools %>% 
  mutate(info_add=place %>% str_extract('(?<=РАЙОН[:punct:]М.)\\w+')) %>% 
  mutate(place=ifelse(str_detect(joined_with_schools$place, 'РАЙОН[:punct:]М.') == TRUE, info_add, place)) %>% 
  mutate(place_type = ifelse(is.na(place_type) & str_detect(joined_with_schools$place, 'РАЙОН[:punct:]М.') == TRUE, 'місто', place_type))

joined_with_schools <- joined_with_schools %>% 
  mutate(info_add=place %>% str_extract('(?<=РАЙОН[:punct:]СМТ\\s)\\w+')) %>% 
  mutate(place=ifelse(str_detect(joined_with_schools$place, 'РАЙОН[:punct:]СМТ\\s') == TRUE, info_add, place)) %>%
  mutate(place_type = ifelse(is.na(place_type) & str_detect(joined_with_schools$place, 'РАЙОН[:punct:]СМТ\\s') == TRUE, 'село', place_type))

joined_with_schools <- joined_with_schools[ ,-c(27)] #info_add

joined_with_schools <- joined_with_schools %>% 
  mutate(place_type = ifelse(is.na(place_type) & str_detect(joined_with_schools$place, 'МІСТА') == TRUE, 
                             'місто', place_type)) 

joined_with_schools$place[] <- lapply(joined_with_schools$place, gsub, pattern = '.', replacement = ". ", fixed = TRUE)
joined_with_schools$place <- sapply(joined_with_schools$place, as.vector)          

joined_with_schools <- joined_with_schools %>% 
  mutate(place_type = ifelse(is.na(place_type) & str_detect(joined_with_schools$place, 'С. ') == TRUE, 'село', place_type)) %>% 
  mutate(place_type = ifelse(is.na(place_type) & str_detect(joined_with_schools$place, 'СМТ') == TRUE, 'село', place_type)) %>% 
  mutate(place_type = ifelse(is.na(place_type) & str_detect(joined_with_schools$place, 'М. ') == TRUE, 'місто', place_type))

joined_with_schools$school_raion[] <- lapply(joined_with_schools$school_raion, gsub, pattern = '.', replacement = ". ", fixed = TRUE)
joined_with_schools$school_raion <- sapply(joined_with_schools$school_raion, as.vector)   

joined_with_schools <- joined_with_schools %>% 
  mutate(place_type = ifelse(is.na(place_type) & str_detect(joined_with_schools$school_raion, 'СМТ') == TRUE, 'село', place_type)) %>% 
  mutate(place_type = ifelse(is.na(place_type) & str_detect(joined_with_schools$school_raion, 'М. ') == TRUE, 'місто', place_type)) %>%
  mutate(place = ifelse(is.na(place) & !is.na(school_raion), school_raion, place)) %>% 
  mutate(place_type = ifelse(is.na(place_type) & str_detect(joined_with_schools$place, 'С-ЩЕ') == TRUE, 'село', place_type))

joined_with_schools %>% filter(is.na(place_type)) %>% group_by(place) %>% count() %>% View()

joined_with_schools <- joined_with_schools %>% 
  mutate(place_type = ifelse(is.na(place_type) & place == 'ОБОЛОНСЬКИЙ' |
                               place == "СОЛОМ'ЯНСЬКИЙ" |
                               place == 'ШЕВЧЕНКІВСЬКИЙ' |
                               place == 'ПЕЧЕРСЬКИЙ',  'місто', place_type))

joined_with_schools <- joined_with_schools %>% 
  mutate(place_type = ifelse(is.na(place_type) & place == 'ТЕРНІВКА', 'село', place_type))

joined_with_schools <- joined_with_schools %>% 
  mutate(place_type = ifelse(is.na(place_type) & school_oblast == 'М.КИЇВ', 'місто', place_type)) %>%
  mutate(place=ifelse(is.na(place) & !is.na(school_oblast) & school_oblast == 'М.КИЇВ', school_oblast, place)) %>% 
  mutate(school_oblast=ifelse(school_oblast == 'КИЇВ', 'М.КИЇВ', school_oblast)) %>% 
  mutate(place=ifelse(place == 'М.КИЇВ', 'КИЇВ', place))


joined_with_schools <- joined_with_schools %>% 
  mutate(info_add=place %>% str_extract('(?<=М.\\s)\\w+')) %>% 
  mutate(place=ifelse(!is.na(info_add) & str_detect(place, 'СМТ') == FALSE, info_add, place))

joined_with_schools <- joined_with_schools[ ,-c(27)]

# dealing with 'Р-НУ' cases in place column
joined_with_schools %>% filter(grepl('Р-НУ', place)) %>% View()

# 1
joined_with_schools <- joined_with_schools %>% 
  mutate(place = ifelse(grepl('Р-НУ', place) & !is.na(school_location), school_location,
                        place))
# 2
joined_with_schools %>% filter(grepl('Р-НУ', place) & grepl('/М. ', school_raion)) %>% View()
joined_with_schools <- joined_with_schools %>%
  mutate(place = ifelse(grepl('Р-НУ', place) & grepl('/М. ', school_raion), 
                        str_extract(school_raion, '(?<=/М.\\s)\\w+'), place))
# 3
joined_with_schools %>% filter(grepl('Р-НУ', place) & grepl('/СМТ ', school_raion)) %>% View()
joined_with_schools <- joined_with_schools %>%
  mutate(place = ifelse(grepl('Р-НУ', place) & grepl('/СМТ ', school_raion), 
                        str_extract(school_raion, '(?<=/СМТ\\s)\\w+'), place))


# 1 
joined_with_schools <- joined_with_schools %>% 
  mutate(place_type=ifelse(grepl('СМТ', place), 'село', place_type)) 

joined_with_schools <- joined_with_schools %>% 
  mutate(place=ifelse(grepl('СМТ', place), 
                      str_extract(place, '(?<=СМТ\\s)\\w+'), place))

# 2
joined_with_schools %>% filter(grepl('С. ', place)) %>% 
  View()
# group_by(place_type) %>% count()

joined_with_schools <- joined_with_schools %>% 
  mutate(place_type=ifelse(grepl('С. ', place), 'село', place_type))
joined_with_schools <- joined_with_schools %>% 
  mutate(place=ifelse(grepl('С. ', place), str_extract(place, '(?<=С.\\s)\\w+'), place))


# 3
joined_with_schools <- joined_with_schools %>% 
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ЧЕРНІГІВ', school), 'ЧЕРНІГІВ',
                        place)) %>% 
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('КИЇВСЬКИЙ', school), 'КИЇВ',
                        place)) %>% 
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ЗАПОРІЗ', school), 'ЗАПОРІЖЖЯ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('МАРІУПОЛЬ', school), 'МАРІУПОЛЬ',
                        place))%>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ПОЛТАВ', school), 'ПОЛТАВА',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('МИКОЛАЇВ', school), 'МИКОЛАЇВ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('МИКОЛАЄВ', school), 'МИКОЛАЇВ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('КІРОВОГРАД', school), 'КРОПИВНИЦЬКИЙ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ХАРКІВ', school), 'ХАРКІВ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ХАРКОВ', school), 'ХАРКІВ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ЧЕРКАС', school), 'ЧЕРКАСИ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('КРЕМЕНЧУ', school), 'КРЕМЕНЧУК',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ЖИТОМИР', school), 'ЖИТОМИР',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('СУМ', school), 'СУМИ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('КРИВОРІЗ', school), 'КРИВИЙ РІГ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ЧЕРНІВЦІ', school), 'ЧЕРНІВЦІ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ЧЕРНІВЕЦ', school), 'ЧЕРНІВЦІ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ЛЬВОВА', school), 'ЛЬВІВ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ЛЬВІВ', school), 'ЛЬВІВ',
                        place)) %>%
  mutate(place = ifelse(grepl('РАЙОН МІСТА', place) & grepl('ХЕРСОН', school), 'ХЕРСОН',
                        place)) 

joined_with_schools <- joined_with_schools %>% 
  mutate(place = ifelse(school_oblast == 'М.КИЇВ', 'КИЇВ', place))

joined_with_schools <- joined_with_schools %>% 
  mutate(place = ifelse(place == 'ОБОЛОНСЬКИЙ' |
                          place == "СОЛОМ'ЯНСЬКИЙ" |
                          place == 'ШЕВЧЕНКІВСЬКИЙ' |
                          place == 'ПЕЧЕРСЬКИЙ' |
                          place == 'ДЕСНЯНСЬКИЙ', 'КИЇВ', place))




