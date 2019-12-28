
library(tidyverse); library(magrittr)

mens_results = read_csv('mens_results.csv')
mens_dictionary = read_csv('mens_dictionary.csv')

event_information = str_split(mens_dictionary$event_date,' ')

mens_dictionary %<>%
  mutate(date = sapply(event_information,'[',1),
         distance_km = sapply(event_information,'[',2),
         type = sapply(event_information,'[',4)) %>%
  select(-event_date)

format_men_results = mens_results %>%
  left_join(mens_dictionary,by=c('id'='event_id')) %>%
  mutate(Position=parse_number(Position)) %>%
  add_count(id,name='participants') %>%
  mutate(percent_rank = Position/participants) %>%
  set_colnames(c('position','name','age_group','flag','time',
                 'points','id','event_name','year','date',
                 'distance_km','type','participants','percent_rank')) %>%
  select(position,percent_rank,name,age_group,flag,time,
         points,id,event_name,year,date,distance_km,type,participants)

write.csv(format_men_results,'men_results_final.csv',row.names=FALSE)

womens_results = read_csv('womens_results.csv')
womens_dictionary = read_csv('womens_dictionary.csv')

event_information = str_split(womens_dictionary$event_date,' ')

womens_dictionary %<>%
  mutate(date = sapply(event_information,'[',1),
         distance_km = sapply(event_information,'[',2),
         type = sapply(event_information,'[',4)) %>%
  select(-event_date)

format_women_results = womens_results %>%
  left_join(womens_dictionary,by=c('id'='event_id')) %>%
  mutate(Position=parse_number(Position)) %>%
  add_count(id,name='participants') %>%
  mutate(percent_rank = Position/participants) %>%
  set_colnames(c('position','name','age_group','flag','time',
                 'points','id','event_name','year','date',
                 'distance_km','type','participants','percent_rank')) %>%
  select(position,percent_rank,name,age_group,flag,time,
         points,id,event_name,year,date,distance_km,type,participants)

write.csv(format_women_results,'women_results_final.csv',row.names=FALSE)



