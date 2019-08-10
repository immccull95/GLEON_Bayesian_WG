# function for pulling data and getting it into a usable format 

require(dplyr)
require(rlang)

get_data <- function(cal_time_start, cal_time_end, fill_dates = TRUE, forecast = FALSE, forecast_time_end, model_timestep = 1, sites){
  
  gleo <- read.csv('Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp_airtemp-Final_02JUL19.csv', stringsAsFactors = F) %>%
    as_data_frame() %>% 
    mutate(date = lapply(date, function(date){as.Date(strsplit(date, 'T')[[1]][1]) %>% as_data_frame()}) %>%  # changing date format to date 
    bind_rows() %>%
    pull(value)) %>%
    mutate(week = lubridate::week(date),
           site = tolower(site)) %>%
    dplyr::filter(site %in% tolower(sites))

  
  # # temperature data 
  # wtr <- read.csv('Datasets/Sunapee/Level1/wtr_temp_long_weekly_summary.csv', stringsAsFactors = F) %>%
  #   select(-X) %>%
  #   mutate(site = tolower(site)) 
  # 
  # data <- left_join(gleo, wtr, by = c('week', 'year', 'site')) 
  
  data <- gleo
  
  if(fill_dates){
    add_cols = colnames(select(data, -date, -site))
    
    #filling in rows to make it daily time step ; or specified model timestep 
    all_dates <- data_frame(date = rep(seq(min(data$date), max(data$date), by= paste(model_timestep, 'day')), each = length(sites))) %>% 
      mutate(site = rep(sites, nrow(.)/length(sites)))
    
    other_cols = lapply(add_cols, function(col){
      mutate(all_dates, !!col := NA)
    }) %>% bind_cols() %>% select(-contains('date'),-contains('site'))
    
    all_dates <- bind_cols(all_dates, other_cols) %>%
      select(colnames(data))
    
    data <- bind_rows(data, all_dates) %>%
      dplyr::filter(!duplicated(paste0(date,site))) %>%
      arrange(date) %>% 
      mutate(year = lubridate::year(date)) 
  }
  
  cal <- data %>%
    dplyr::filter(date > as.Date(cal_time_start), date < as.Date(cal_time_end)) 
  
  if(forecast){
    forecast <- data %>%
      dplyr::filter(date > as.Date(cal_time_end), date < as.Date(forecast_time_end))
    
    return(list(cal = cal, forecast = forecast))
  }else{
    return(cal)
  }

}
