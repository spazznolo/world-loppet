
library(tidyverse); library(magrittr)

general_results = read_csv('general_results.csv')
missed_results = read_csv('missed_results.csv')
general_dictionary = read_csv('general_dictionary.csv')

total_results = rbind(general_results,missed_results)
event_information = str_split(general_dictionary$event_date,' ')

general_dictionary %<>%
  mutate(date = sapply(event_information,'[',1),
         distance_km = as.numeric(sapply(event_information,'[',2)),
         type = sapply(event_information,'[',4)) %>%
  select(-event_date)

final_results = total_results %>%
  left_join(general_dictionary,by=c('id'='event_id')) %>%
  mutate(
    tournament_information=case_when(
      distance_km < 15 & type == 'CT' ~ 'CLASSIC - 15Km and under',
      distance_km < 25 & type == 'CT' ~ 'CLASSIC - 25Km and over',
      distance_km < 100 & type == 'CT' ~ 'CLASSIC - 50Km and over',
      distance_km < 15 & type == 'FT' ~ 'FREE - 15Km and under',
      distance_km < 25 & type == 'FT' ~ 'FREE - 25Km and over',
      distance_km < 100 & type == 'FT' ~ 'FREE - 50Km and over',
      ),
    Position=parse_number(Position),
    event_name = iconv(event_name, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
    event_name = gsub("[^[:alnum:][:space:]]", '', event_name),
    event_name = str_to_title(event_name),
    Name = iconv(Name, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
    Name = gsub("[^[:alnum:][:space:]]", '', Name),
    Name = str_to_title(Name),
    Name = paste0(Name,' ',tournament_information)
         ) %>%
  add_count(id,name='participants') %>%
  set_colnames(c('position','name','age_group','flag','time',
                 'points','id','event_name','year','date',
                 'distance_km','type','tournament_information','participants')) %>%
  select(id,event_name,date,distance_km,type,participants,position,name)

write.csv(final_results,'final_results.csv',row.names=FALSE)
