#function to evaluate performance of ARIMA model (produces season total forecasts only)
preseason_forecast_v3<-function(series,
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
      
      temp<-NULL
      temp<-arima_forecast(tdat,xreg,xreg_pred,last_train_yr)
      pred<-temp$pred
      CI<-temp$CI

      tdat<-tdat%>%
        bind_cols(pred=pred)%>%
        left_join(CI, by = c("year","period"))%>%
        dplyr::rename(predicted_abundance = pred)%>%
        filter(train_test==1)
      
      if(i==1){forecasts = tdat
      }else{forecasts = forecasts %>% bind_rows(tdat)}
    }#years
    forecasts<-forecasts%>%
      mutate(error = predicted_abundance-abundance,
             pct_error=scales::percent(error/abundance),
             model = as.character(c)
      )
    if(c==1){
      tdat2 <- forecasts
    }else{
      if(sum(is.na(forecasts$predicted_abundance))==0){
        tdat2 <- tdat2%>%
          bind_rows(forecasts)
      }
    }
  }#covariates
  forecasts<-tdat2
  
  results2<-NULL
  for(i in 1:leave_yrs){
    last_train_yr = max(series$year) - (leave_yrs-i+exists1)
    tdat<-series%>%
      filter(year <= (last_train_yr + 1))%>%
      mutate(train_test = ifelse(year > last_train_yr & period >= first_forecast_period, 1, 0),
             log_abundance = log(abundance)
      )
    formula<-as.formula(paste0("log_abundance ~ ",paste0(paste(paste("s(",covariates,",bs='re')",sep=""),collapse="+"))))
    
    fit<-gam(formula=formula,data=tdat%>%filter(train_test==0),family="gaussian",link=identity)
    #predict(fit,newdata=tdat%>%filter(train_test==1))
    
    results<-data.frame(predict(fit,tdat%>%filter(train_test==1),type="response",se.fit=T))%>%
      cbind(data.frame(gam_pred_ints(fit,tdat%>%filter(train_test==1),c(0.025,0.25,0.75,0.975))))%>%
      data.frame()%>%
      rename(predicted_abundance=fit,`Lo 95`= X0.025,`Hi 95`=X0.975, `Lo 50` =X0.25,`Hi 50` = X0.75) %>%
      mutate(across(everything(),~exp(.)))%>%
      bind_cols(data.frame(year=tdat%>%filter(train_test==1)%>%dplyr::select(year)%>%unlist()%>%pluck()))%>%
      dplyr::select(!se.fit)
    
    results<-tdat%>%
      filter(train_test==1)%>%
      left_join(results)%>%
      dplyr::select(-log_abundance)
      
    
    if(i==1){
      results2<-results
    }else{
      results2%<>%
        bind_rows(results)
    }
  }
  forecasts%<>%
    bind_rows(results2%>%
                mutate(model="ridge")
              )
  
  #do model averaging and stacking and calculate performance metrics
  forecast_eval<-evaluate_forecasts_with_ensembles3(forecasts=forecasts,series=series,TY_ensemble=TY_ensemble,k=k,leave_yrs=leave_yrs)
  
  return(forecast_eval)
}

