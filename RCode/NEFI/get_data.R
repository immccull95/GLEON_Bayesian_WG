# function for pulling data and getting it into a usable format 

require(dplyr)

get_data <- function(cal_time_start, cal_time_end, forecast_time_end, sites){
  
  data <- read.csv('Datasets/Sunapee/R Work/Level 1/All_Sites_Gloeo.csv', stringsAsFactors = F) %>%
    as_data_frame() %>% 
    mutate(date = lapply(date, function(date){as.Date(strsplit(date, 'T')[[1]][1]) %>% as_data_frame()}) %>%  # changing date format to date 
    bind_rows() %>%
    pull(value)) %>%
    dplyr::filter(site %in% sites) 
  
  cal <- data %>%
    dplyr::filter(date > as.Date(cal_time_start), date < as.Date(cal_time_end)) 

}
