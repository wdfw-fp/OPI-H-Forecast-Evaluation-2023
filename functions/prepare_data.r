#function to summarize data for forecasting into period sums
#dates between the last observed date of the last year and the beginning of the second period are automatically deleted so period 1 counts are comparable among years
prepare_data<-function(series,
                       date_start_analysis, #inclusive
                       date_end_analysis, #inclusive
                       p_2_start_m, #inclusive
                       p_2_start_d, #inclusive
                       covariates
){
  p_2_end_m<-month(date_end_analysis)
  p_2_end_d<-mday(date_end_analysis)
  
  series<-series%>%
    filter(date >= date_start_analysis & date <= date_end_analysis)%>%
    filter(month(date) > month(date_start_analysis) | month(date) == month(date_start_analysis) & mday(date) >= mday(date_start_analysis)) %>%
    filter(month(date) < month(date_end_analysis) | month(date) == month(date_end_analysis) & mday(date) <= mday(date_end_analysis)) %>%
    mutate(period = ifelse(month(date) < p_2_start_m | month(date) == p_2_start_m & mday(date) < p_2_start_d, 1, 2)
    )
  
  last_obs_d<-mday(max(series$date))
  last_obs_m<-month(max(series$date))
  
  if(last_obs_m < p_2_start_m | last_obs_m == p_2_start_m & last_obs_d < p_2_start_d){
    series<-series%>%
      filter(month(date) < month(last_obs_m)  | month(date) > month(p_2_start_m) | month(date) == month(last_obs_m) & mday(date) <= last_obs_d | month(date) == p_2_start_m & mday(date) >= p_2_start_d)
  }
  
  series<-series%>%
    dplyr::group_by_at(c("year","species","period",all_of(covariates)))%>%
    summarize(abundance=sum(abundance),.groups="keep")
}