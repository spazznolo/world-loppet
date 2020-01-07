
library(tidyverse); library(magrittr)

general_results = read_csv('general_results.csv')
missed_results = read_csv('missed_results.csv')
general_dictionary = read_csv('general_dictionary.csv')

total_results = rbind(general_results,missed_results)
event_information = str_split(general_dictionary$event_date,' ')

general_dictionary %<>%
  mutate(date = sapply(event_information,'[',1),
         distance_km = sapply(event_information,'[',2),
         type = sapply(event_information,'[',4)) %>%
  select(-event_date)

format_general_results = total_results %>%
  left_join(general_dictionary,by=c('id'='event_id')) %>%
  mutate(Position=parse_number(Position)) %>%
  add_count(id,name='participants') %>%
  mutate(percent_rank = Position/participants) %>%
  set_colnames(c('position','name','age_group','flag','time',
                 'points','id','event_name','year','date',
                 'distance_km','type','participants','percent_rank')) %>%
  select(position,percent_rank,name,age_group,flag,time,
         points,id,event_name,year,date,distance_km,type,participants)

write.csv(format_general_results,'final_results.csv',row.names=FALSE)

