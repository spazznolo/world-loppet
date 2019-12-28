
# load packages
library(XML); library(rvest); library(tidyverse)

# create shell for historic results set
history_of_results = data.frame(matrix(NA,0,0))
event_dictionary = data.frame(event_id=NA,event_name=NA,event_date=NA)
options(timeout= 10000000000)

mens_results = read_csv('mens_results.csv')

event_ids = unique(mens_results$id)

for (event_id in 1:2000) {
  
  # update progress
  message('Retrieving Event ',event_id)
  
  race_url = paste0('https://www.worldloppet.com/browse/?id=',event_id)
  
  event_info = read_html(race_url) %>% 
    html_nodes('h2') %>%
    .[1] %>%
    gsub('<br>','<br>  ',.) %>%
    gsub("<[^>]+>", "",.) %>%
    str_split(.,'  ') %>%
    unlist()
  
  event_dictionary = rbind(event_dictionary,c(event_id,event_info))
  
  # temporary break 
  Sys.sleep(3)
  
}

list_of_events = event_dictionary %>% 
  filter(nchar(event_name)>0) %>%
  mutate(year=substr(trimws(event_date),1,4)) %>%
  filter(year %in% 2015:2019)


# run get_race_results for desired events
for (event_id in list_of_events$event_id) {
  
  for (page_number in 1:100) {
    
    # update progress
    message('Retrieving Page ',page_number,' of Event ',event_id)
    
    # create url of desired event and page
    race_url = paste0('https://www.worldloppet.com/browse/?id=',event_id,'&pgn=',page_number)
    
    # scrape desired page
    read_html(race_url) %>% 
      html_nodes('table') %>% 
      .[1] %>% 
      html_table(fill=TRUE) %>%
      .[[1]] %>%
      mutate(id=event_id) -> l
    
    # move to next event if table is empty
    if (nrow(l)==0) break
    
    # bind table to master set
    history_of_results = rbind(history_of_results,l)
    
    # temporary break 
    Sys.sleep(3)
    
    
  }
  
}

# save tournament dictionary
write.csv(list_of_events,'mens_dictionary.csv',row.names=FALSE)

# save tournament results
write.csv(history_of_results,'mens_results.csv',row.names=FALSE)


