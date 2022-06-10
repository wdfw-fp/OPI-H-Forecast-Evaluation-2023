#function to evaluate performance of SARIMA model (produces season total forecasts only)
inseason_forecast<-function(series,
                            leave_yrs,
                            covariates,
                            first_forecast_period,
                            plot_results,
                            write_model_summaries,
                            forecast_period_start_m, #inclusive
                            forecast_period_start_d, #inclusive
                            obs_period_2
                            ){
  if(write_model_summaries ==T){
    write.table(NULL,"summary.txt")
  }
  
  for(i in 1:leave_yrs){
    last_train_yr = max(series$year) - (leave_yrs-i+1)
    tdat<-series%>%
      filter(year <= (last_train_yr + 1))%>%
      mutate(train_test = ifelse(year > last_train_yr & period >= first_forecast_period, 1, 0),
      )
    
    exists1<-tdat%>%filter(year==(last_train_yr+1) & period == 1)%>%ungroup%>%dplyr::select(abundance)%>%nrow()
    if(exists1==0){
      adddat<-tdat%>%tail(1)
      adddat$period<-1
      adddat$abundance<-NA
      adddat$train_test<-1
      tdat<-tdat%>%
        bind_rows(adddat)
    }
    exists2<-tdat%>%filter(year==(last_train_yr+1) & period == 2)%>%ungroup%>%dplyr::select(abundance)%>%nrow()
    if(exists2==0){
      adddat<-tdat%>%tail(1)
      adddat$period<-2
      adddat$abundance<-NA
      adddat$train_test<-1
      tdat<-tdat%>%
        bind_rows(adddat)
    }
    
    xreg<-tdat%>%
      filter(train_test==0)%>%
      ungroup%>%
      dplyr::select(all_of(covariates))%>%
      as.matrix()
    
    xreg_pred<-tdat%>%
      filter(train_test==1)%>%
      ungroup%>%
      dplyr::select(all_of(covariates))%>%
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
      
      pred<-c(m1$fitted,forecast::forecast(m1,lambda=0,h=(1/first_forecast_period)*2,
                                           xreg = NULL
      )$mean)
      CI<-forecast::forecast(m1,lambda=0,h=(1/first_forecast_period)*2, level = c(50, 95),
                             xreg = NULL
      )%>%
        as_tibble()%>%
        dplyr::select(!`Point Forecast`)%>%
        mutate(year = last_train_yr+1, period = ifelse(first_forecast_period == 2,2,c(1:2)))
    }
    
    if(write_model_summaries ==T){
      sink("summary.txt",append=T)
      print(summary(m1))
      sink()
    }
    
    tdat<-tdat%>%
      bind_cols(pred=pred)%>%
      left_join(CI, by = c("year","period"))
    
    if(plot_results == T){
      p<-ggplot(data=tdat,aes(x=period,y=abundance, group=year,color=factor(train_test)))+
        facet_wrap(~year,scales = "free_x")+
        geom_errorbar(aes(ymin=`Lo 95`, ymax=`Hi 95`), width=.2, color="blue",position=position_dodge(.9))+
        geom_rect(aes(xmin=period-0.25,xmax=period+0.25, ymin=`Lo 50`, ymax=`Hi 50`),fill="white", colour="blue", size=0.5)+ 
        geom_point(mapping = aes(x=period,y=pred),color="blue",shape=3)+
        geom_point(size=2)+
        scale_x_continuous(breaks = c(1,2))+
        theme_bw()+
        ylim(0,NA)
      
      print(p)
    }
     
    tdat<-tdat%>%
      dplyr::rename(predicted_abundance = pred)%>%
      filter(train_test==1)
    
    if(i==1){results = tdat
    }else{results = results %>% bind_rows(tdat)}
  }
  
  if(nrow(obs_period_2)>0){
    results<-results%>%
      left_join(obs_period_2)%>%
      mutate(
        abundance = abundance + + obs_abundance,
        predicted_abundance = predicted_abundance + obs_abundance,
        `Lo 50` = `Lo 50` + obs_abundance,
        `Lo 95` = `Lo 95` + obs_abundance,
        `Hi 50` = `Hi 50` + obs_abundance,
        `Hi 95` = `Hi 95` + obs_abundance,
        )
  }
  results<-results%>%
    mutate(error = predicted_abundance-abundance,
           pct_error=scales::percent(error/abundance)
    )
  return(results)
}

