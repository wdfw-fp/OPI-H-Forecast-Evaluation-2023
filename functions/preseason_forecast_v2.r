#function to evaluate performance of ARIMA model (produces season total forecasts only)
preseason_forecast_v2<-function(series,
                            leave_yrs,
                            TY_ensemble,
                            covariates,
                            first_forecast_period,
                            plot_results,
                            write_model_summaries,
                            forecast_period_start_m, #inclusive
                            forecast_period_start_d, #inclusive
                            obs_period_2,
                            p1_covariates_only,
                            stack_metric,
                            k
                            ){
  if(write_model_summaries ==T){
    write.table(NULL,"summary.txt")
  }
  
  series<-series%>%
    ungroup()%>%
    dplyr::select(year,species,period,abundance,all_of(unique(unlist(covariates))))%>%
    filter(
      across(
        .cols = all_of(unique(unlist(covariates))),
        .fns = ~ !is.na(.x)
      )
    )
  exists1 =ifelse(is.na(series%>%dplyr::select(abundance)%>%tail(n=1)%>%pull()),1,0)
  for(c in 1:length(covariates)){
    for(i in 1:leave_yrs){
      last_train_yr = max(series$year) - (leave_yrs-i+exists1)
      tdat<-series%>%
        filter(year <= (last_train_yr + 1))%>%
        mutate(train_test = ifelse(year > last_train_yr & period >= first_forecast_period, 1, 0),
        )

      xreg<-tdat%>%
        filter(train_test==0)%>%
        ungroup%>%
        dplyr::select(all_of(covariates[[c]]))%>%
        as.matrix()
      
      xreg_pred<-tdat%>%
        filter(train_test==1)%>%
        ungroup%>%
        dplyr::select(all_of(covariates[[c]]))%>%
        as.matrix()
  
      if(ncol(xreg)>0){
        m1<-tdat%>%
          filter(train_test==0)%>%
          ungroup()%>%
          dplyr::select(abundance)%>%
          unlist()%>%
          ts(frequency = 2)%>%
          auto.arima(lambda=0,seasonal = T, xreg = xreg)
        
        pred<-c(m1$fitted,forecast::forecast(m1,lambda=0,h=(1/first_forecast_period)*2,
                                             xreg = xreg_pred
        )$mean)
        CI<-forecast::forecast(m1,lambda=0,h=(1/first_forecast_period)*2, level = c(50, 95),
                               xreg = xreg_pred
        )%>%
          as_tibble()%>%
          dplyr::select(!`Point Forecast`)%>%
          mutate(year = last_train_yr+1, period = ifelse(first_forecast_period == 2,2,c(1:2)))
      }else{
        m1<-tdat%>%
          filter(train_test==0)%>%
          ungroup()%>%
          dplyr::select(abundance)%>%
          unlist()%>%
          ts(frequency = 2)%>%
          auto.arima(lambda=0,seasonal = T, xreg = NULL)
        
        pred<-c(m1$fitted,forecast::forecast(m1,lambda=0,h=1,
                                             xreg = NULL
        )$mean)
        CI<-forecast::forecast(m1,lambda=0,h=1, level = c(50, 95),
                               xreg = NULL
        )%>%
          as_tibble()%>%
          dplyr::select(!`Point Forecast`)%>%
          mutate(year = last_train_yr+1, period = 1)
      }
      
      if(write_model_summaries ==T){
        sink("summary.txt",append=T)
        print(summary(m1))
        sink()
      }
      
      tdat<-tdat%>%
        bind_cols(pred=pred)%>%
        left_join(CI, by = c("year","period"))%>%
        dplyr::rename(predicted_abundance = pred)%>%
        filter(train_test==1)
      
      if(i==1){forecasts = tdat
      }else{forecasts = forecasts %>% bind_rows(tdat)}
    }
    forecasts<-forecasts%>%
      mutate(error = predicted_abundance-abundance,
             pct_error=scales::percent(error/abundance),
             model = as.character(c)
      )
    if(c==1){
      tdat2 <- forecasts
    }else{
      tdat2 <- tdat2%>%
        bind_rows(forecasts)
    }
  }
  forecasts<-tdat2
  
  #do model averaging and stacking and calculate performance metrics
  forecast_eval<-evaluate_forecasts_with_ensembles2(forecasts=forecasts,series=series,TY_ensemble=TY_ensemble,k=k,leave_yrs=leave_yrs)
  
  return(forecast_eval)
}

