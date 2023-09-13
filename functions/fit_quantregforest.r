fit_quantregForest<-function(forecasts,years,series){
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
  fit<-quantregForest(x = x, y = y)
  results<-predict(fit, newdata=x_pred,what=c(0.025,0.25,0.5,0.75,0.975))%>%
    as.data.frame()%>%
    setNames(c("Lo 95","Lo 50","predicted_abundance","Hi 50","Hi 95"))%>%
    mutate(year=max(years)+1,
           model="quantregForest"
    )
  return(results)
}