fit_xgboost<-function(forecasts,years,series){
  stackdat<-forecasts%>%
    filter(year %in% years)%>%
    pivot_wider(names_from = model, values_from = predicted_abundance,id_cols = year)%>%
    left_join(series%>%dplyr::select(year,abundance))
  
  x<-stackdat%>%dplyr::select(!year & !abundance)%>%as.matrix()
  y<-stackdat%>%dplyr::select(abundance)%>%unlist()%>%as.vector()
  x_pred<-forecasts%>%
    filter(year == max(years)+1)%>%
    pivot_wider(names_from = model, values_from = predicted_abundance,id_cols = year)%>%
    left_join(series%>%dplyr::select(year,abundance))%>%
    dplyr::select(!year & !abundance)%>%
    as.matrix()
  
  fit<-xgboost(data = x, label = y,booster = "gblinear", 
               eta = 0.1, lambda = 0.01, alpha = 0.01,
               nrounds = 200, objective = "reg:squarederror", verbose =  0
  )
  
  results<-tibble(predicted_abundance=predict(fit, x_pred))%>%
    mutate(year=max(years)+1,
           model="xgboost_rf"
    )
  return(results)
}