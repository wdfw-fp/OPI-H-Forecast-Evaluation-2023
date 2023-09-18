test<-forecasts  %>% 
  mutate(error=abundance-predicted_abundance,
         APE=abs(error/abundance))%>%
  arrange(model,year) %>% 
  group_by(model) %>%
  mutate(MAPE= lag( 100*zoo::rollmean(APE, k = 15, fill = NA, align = "right"))) %>% 
  group_by(year) %>% 
  filter(MAPE==min(MAPE)) %>% 
  filter(between(year,2023-15,2023)) %>% arrange(year) 

test %>% left_join(model_list) %>% dplyr::select(year,MAPE,model_name)%>% View

test%>% 
  ungroup %>% 
  mutate(mean(APE,na.rm=T)) %>% View
