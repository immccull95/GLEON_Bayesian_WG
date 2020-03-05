#Title: Reading in appropriate data files for hindcasts for each model
#Author: Mary Lofton
#Date: 03MAR20

get_forecast_data <- function(model_name){
 
  if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error" | model_name = "Seasonal_AR"){
    y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_22JUL19.csv"))+0.003)
    forecast_y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
    forecast_y <- forecast_y[7:8,]
  } 
  
}