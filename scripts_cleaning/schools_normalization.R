library(dplyr)
library(stringr)
library(purrr)


fix_latin <- function(str) {
  res <- str %>% 
    str_replace_all('A', 'А') %>%
    str_replace_all('B', 'В') %>%
    str_replace_all('C', 'С') %>%
    str_replace_all('E', 'Е') %>%
    str_replace_all('H', 'Н') %>%
    str_replace_all('I', 'І') %>% # mega
    str_replace_all('K', 'К') %>%
    str_replace_all('M', 'М') %>%
    str_replace_all('O', 'О') %>%
    str_replace_all('P', 'Р') %>%
    str_replace_all('T', 'Т') %>%
    str_replace_all('X', 'Х') %>%
    str_replace_all('Y', 'У')
  
  return(res)
} 

address <- function (str) {
  res <- str %>% 
    str_to_upper() %>% 
    str_replace_all('[[:punct:]=+|$^]', ' ') %>% 
    str_replace_all('\\s+', ' ')
  
  res <- str_c(' ', res, ' ') %>% 
    fix_latin() %>% 
    str_replace_all('L', 'І') %>% # використовують замість римської одиниці
    str_replace_all('N', '№') %>% # номер
    str_replace_all('№\\s+', '№') %>% # мега (+ ~ 1000)
    str_replace_all('^ ЗАКЛАД ', ' ') %>% 
    str_replace_all('^ ОПОРНИЙ ЗАКЛАД ', ' ') %>% 
    str_replace_all(' СЕЛА ', ' С ') %>% 
    str_replace_all(' ЗОШ ', ' ЗАГАЛЬНООСВІТНЯ ШКОЛА ') %>% 
    str_replace_all(' НВК ', ' НАВЧАЛЬНО ВИХОВНИЙ КОМПЛЕКС ') %>% 
    str_replace_all(' ЗНЗ ', ' ЗАГАЛЬНООСВІТНІЙ НАВЧАЛЬНИЙ ЗАКЛАД ') %>% 
    str_replace_all(' ДНЗ ', ' ДОШКІЛЬНИЙ НАВЧАЛЬНИЙ ЗАКЛАД ') %>% 
    str_replace_all(' КЗ ', ' КОМУНАЛЬНИЙ ЗАКЛАД ') %>% 
    str_replace_all(' РКЗО ', ' РАЙОННИЙ КОМУНАЛЬНИЙ ЗАКЛАД ОСВІТИ ') %>% 
    str_replace_all('^ РАЙОННИЙ КОМУНАЛЬНИЙ ЗАКЛАД ОСВІТИ ', ' ') %>% 
    str_replace_all('^ КОМУНАЛЬНИЙ ЗАКЛАД ОСВІТИ ', ' ') %>% 
    str_replace_all('^ КОМУНАЛЬНИЙ ЗАКЛАД ', ' ') %>%
    str_replace_all(' СЗШ ', ' СЕРЕДНЯ ЗАГАЛЬНООСВІТНЯ ШКОЛА ') %>%
    str_replace_all(' СШ ', ' СПЕЦІАЛІЗОВАНА ШКОЛА ') %>%
    str_replace_all(' СШІ ', ' СПЕЦІАЛІЗОВАНА ШКОЛА ІНТЕРНАТ ') %>% #nothing
    str_replace_all(' ІМ ', ' ІМЕНІ ') %>% # mega
    str_replace_all(' ІСТ ', ' І СТ ') %>%
    str_replace_all(' ІІСТ ', ' ІІ СТ ') %>%
    str_replace_all(' ІІІСТ ', ' ІІІ СТ ') %>%
    str_replace_all(' І ІІ СТ ', ' І ІІ СТУПЕНІВ ') %>%
    str_replace_all(' І ІІІ СТ ', ' І ІІІ СТУПЕНІВ ') %>%
    str_replace_all(' І СТ ', ' І СТУПЕНЯ ') %>%
    str_replace_all(' ІІ СТ ', ' ІІ СТУПЕНЯ ') %>%
    str_replace_all(' ІІІ СТ ', ' ІІІ СТУПЕНЯ ') %>%
    
    str_replace_all(' ІСТУПЕНЯ ', ' І СТУПЕНЯ ') %>%
    str_replace_all(' ІІСТУПЕНЯ ', ' ІІ СТУПЕНЯ ') %>%
    str_replace_all(' ІІІСТУПЕНЯ ', ' ІІІ СТУПЕНЯ ') %>%
    str_replace_all(' І ІІСТУПЕНІВ ', ' І ІІ СТУПЕНІВ ') %>%
    str_replace_all(' І ІІІСТУПЕНІВ ', ' І ІІІ СТУПЕНІВ ') %>%
    
    str_replace_all(' ІСТУПЕНІ ', ' І СТУПЕНЯ ') %>%
    str_replace_all(' ІІСТУПЕНІ ', ' ІІ СТУПЕНЯ ') %>%
    str_replace_all(' ІІІСТУПЕНІ ', ' ІІІ СТУПЕНЯ ') %>%
    str_replace_all(' І ІІСТУПЕНІ ', ' І ІІ СТУПЕНІВ ') %>%
    str_replace_all(' І ІІІСТУПЕНІ ', ' І ІІІ СТУПЕНІВ ') %>%
    
    str_replace_all(' 1 3 СТУПЕНІ ', ' І ІІІ СТУПЕНІВ ') %>%
    str_replace_all(' 1 3 СТУПЕНІВ ', ' І ІІІ СТУПЕНІВ ') %>%
    str_replace_all(' 1 3 СТУПЕНЯ ', ' І ІІІ СТУПЕНІВ ') %>%
    
    str_replace_all(' ОБЛ\\s+$', ' ОБЛАСТІ ') %>% 
    
    #str_replace_all('\\s.\\s', ' ') %>% 
    str_replace_all('\\s+', ' ') %>% 
    str_trim()
  
  return(res)
}

address_hard <- function (str) {
  res <- str# %>% 
  #str_to_upper() %>% 
  #str_replace_all('[[:punct:]=+|$^]', ' ') %>% 
  #str_replace_all('\\s+', ' ')
  
  res <- str_c(' ', res, ' ') %>% 
    str_replace_all(' МІСЬКОЇ РАДИ ', ' РАЙОННОЇ РАДИ ') %>% 
    
    str_replace_all('\\s+', ' ') %>% 
    str_trim()
  
  return(res)
}

school_number <- function(name) {
  num <- name %>% 
    str_extract('№\\d+') %>% 
    str_extract('\\d+')
  
  num <- ifelse(is.na(num), '', num)
  
  return(num)
}
