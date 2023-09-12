evaluate_old_model<-function(data,TY_ensemble,yr_end){
  results_old<-NULL
  for(i in (yr_end-TY_ensemble):c(OPIHData%>%filter(!is.na(abundance))%>%summarise(max(year))%>%unlist()%>%pluck())){
    if(is.null(results_old)){
      results_old<- as_tibble(predict(lm(abundance ~ lagJackOPI + lagSmAdj,data=OPIHData%>%filter(year %in% c(min(year):i))),
                                      newdata=OPIHData%>%filter(year == i+1),
                                      se.fit = T,
                                      interval = "prediction",
                                      level = c(0.95)
      )
      )%>%
        as.matrix()%>%
        as_tibble()%>%
        setNames(c("predicted_abundance","Lo 95", "Hi 95", "se.fit","df","residual.scale"))%>%
        bind_cols(OPIHData%>%filter(year == i+1)%>%dplyr::select(year,abundance))
    }else{
      temp<-as_tibble(predict(lm(abundance ~ lagJackOPI + lagSmAdj,data=OPIHData%>%filter(year %in% c(min(year):i))),
                              newdata=OPIHData%>%filter(year == i+1),
                              se.fit = T,
                              interval = "prediction",
                              level = c(0.95)
      )
      )%>%
        as.matrix()%>%
        as_tibble()%>%
        setNames(c("predicted_abundance","Lo 95", "Hi 95", "se.fit","df","residual.scale"))%>%
        bind_cols(OPIHData%>%filter(year == i+1)%>%dplyr::select(year,abundance))
      
      results_old<- bind_rows(results_old,temp)
    }
  }
  results_old%<>%
    mutate(`Hi 50` = predicted_abundance + qt(0.5/2, df, lower.tail = FALSE) * sqrt(se.fit^2 + residual.scale^2),
           `Lo 50` = predicted_abundance - qt(0.5/2, df, lower.tail = FALSE) * sqrt(se.fit^2 + residual.scale^2),
           model = "abundance ~ lagJackOPI + lagSmAdj [current OPI model; linear regression]"
    )%>%
    dplyr::select(-c("se.fit","df","residual.scale"))
  return(
    list(
      forecasts=results_old,
      forecast_skill=evaluate_forecasts2(results_old,data)
    )
    
  )
}
