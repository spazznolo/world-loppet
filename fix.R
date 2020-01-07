
# load packages
library(XML); library(rvest); library(tidyverse)

# create shell for historic results set
missed_results = data.frame(matrix(NA,0,0))
options(timeout= 10000000000)

missing_events = read_csv('missing_events.csv')

# run get_race_results for desired events
for (event_id in missing_events$event_id) {
  
  for (page_number in 1:150) {
    
    # update progress
    message('Retrieving Page ',page_number,' of Event ',event_id)
    
    for (rank in c('m','w')) {
     
      # create url of desired event and page
      race_url = paste0('https://www.worldloppet.com/browse/?rank=',rank,'&id=',event_id,'&pgn=',page_number)
      
      # scrape desired page
      read_html(race_url) %>% 
        html_nodes('table') %>% 
        .[1] %>% 
        html_table(fill=TRUE) %>%
        .[[1]] %>%
        mutate(id=event_id) -> results_table
      
      # move to next event if table is empty
      if (nrow(results_table)==0) break
      
      # bind table to master set
      missed_results = rbind(missed_results,results_table)
      
      # temporary break 
      Sys.sleep(3) 
      
    }

    
    
  }
  
}


# save tournament results
write.csv(missed_results,'missed_results.csv',row.names=FALSE)
