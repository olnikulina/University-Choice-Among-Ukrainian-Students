library(stringr)

region_reschool_old <- function(schools_parsed_sep_refine_csv) {
  school <- str_c(' ', school, ' ') 
  
  schools_parsed_sep_refine_csv$school <- schools_parsed_sep_refine_csv$school %>% 
    str_replace_all(' ЦЮРУПИНСЬК(?!\\s)', ' ОЛЕШКІВСЬК')
  
  schools_parsed_sep_refine_csv$school <- schools_parsed_sep_refine_csv$school %>% 
    str_replace_all(' ДНІПРОПЕТРОВСЬК(?!\\s)', ' ДНІПРОВСЬК')
  
  schools_parsed_sep_refine_csv$school <- schools_parsed_sep_refine_csv$school %>% 
    str_replace_all(' КРАСНОЛИМАНСЬК(?!\\s)', ' ЛИМАНСЬК')
  
  schools_parsed_sep_refine_csv$school <- schools_parsed_sep_refine_csv$school %>% 
    str_replace_all(' ПЕРШОТРАВНЕВА ', ' МАНГУСЬКА ') %>% 
    str_replace_all(' ПЕРШОТРАВНЕВИЙ ', ' МАНГУШСЬКИЙ ') %>% 
    str_replace_all(' ПЕРШОТРАВНЕВОЇ ', ' МАНГУСЬКОЇ ') %>% 
    str_replace_all(' ПЕРШОТРАВНЕВОГО ', ' МАНГУШСЬКОГО ')
  
  school <- school %>% 
    str_replace_all(' ЧЕРВОНОАРМІЙСЬК(?!\\s)', ' ПУЛИНСЬК')
  
  school <- school %>% 
    str_replace_all(' УЛЬЯНОВСЬК(?!\\s)', ' БЛАГОВІЩЕНСЬК') 
  
  school <- school %>% 
    str_replace_all(' ЖОВТНЕВОЇ ', ' ВІТОВСЬКОЇ ') %>%
    str_replace_all(' ЖОВТНЕВОГО ', ' ВІТОВСЬКОГО ')
  
  school <- school %>% 
    str_replace_all(' КРАСНООКНЯНСЬК(?!\\s)', ' ОКНЯНСЬК')
  
  school <- school %>% 
    str_replace_all(' ФРУНЗІВСЬК(?!\\s)', ' ЗАХАРІВСЬК')
  
  school <- school %>% 
    str_replace_all('\\s+', ' ') %>% 
    str_trim()
  
  return(school)
}

region_reschool_old(schools_parsed_sep_refine_csv)
