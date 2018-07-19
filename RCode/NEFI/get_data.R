# function for pulling data and getting it into a usable format 

require(dplyr)

get_data <- function(cal_time_start, cal_time_end, forecast_time_end, sites){
  
  gleo <- read.csv('Datasets/Sunapee/R Work/Level 1/All_Sites_Gloeo.csv', stringsAsFactors = F) %>%
    as_data_frame() %>% 
    mutate(date = lapply(date, function(date){as.Date(strsplit(date, 'T')[[1]][1]) %>% as_data_frame()}) %>%  # changing date format to date 
    bind_rows() %>%
    pull(value)) %>%
    mutate(week = lubridate::week(date),
           site = tolower(site)) %>%
    dplyr::filter(site %in% sites) 

  
  # temperature data 
  wtr <- read.csv('Datasets/Sunapee/R Work/Level 1/wtr_temp_long_weekly_summary.csv', stringsAsFactors = F) %>%
    select(-X) %>%
    mutate(site = tolower(site)) 
  
  data <- left_join(gleo, wtr, by = c('week', 'year', 'site')) 
  
  cal <- data %>%
    dplyr::filter(date > as.Date(cal_time_start), date < as.Date(cal_time_end)) 
  
  forecast <- data %>%
    dplyr::filter(date > as.Date(cal_time_end), date < as.Date(forecast_time_end)) 
  
  return(list(cal, forecast)) 
}
