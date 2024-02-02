#get NOAA SST Buoy Data
getbuoydata<-function(buoylist,years){
  df<-data.frame(matrix(NA,ncol = 5))
  colnames(df)<-c("buoyid","year","month","yday","meanSST")
  for(i in 1:length(buoylist)){
    for(j in 1:length(years)){
      try(
        df<-df%>%
          bind_rows(buoy(dataset = "stdmet",buoyid = buoylist[i],year=years[j])$data%>%
                      dplyr::select(sea_surface_temperature,time)%>%
                      mutate(buoyid=buoylist[i]
                             ,year = year(time)
                             ,month = month(time)
                             ,yday=yday(time)
                      )%>%
                      group_by(buoyid, year, yday) %>%
                      summarise(meanSST = mean(sea_surface_temperature, na.rm = TRUE))%>%
                      ungroup()
          )
        ,silent = T
      )
    }
    if(max(years)-year(Sys.Date())==0){
      buoy_data <- read_table(
        paste0("https://www.ndbc.noaa.gov/data/realtime2/", buoylist[i], ".txt"),
        skip = 2,
        col_types = cols(
          X6 = col_character(),
          X7 = col_character(),
          X8 = col_character(),
          X14 = col_character()
        )
      ) %>%
        set_names(names(read_table(paste0("https://www.ndbc.noaa.gov/data/realtime2/", buoylist[i], ".txt")))) %>%
        dplyr::select(year=`#YY`,month= MM, day=DD,SST=WTMP) %>%
        mutate(
          buoyid = buoylist[i],
          year=as.numeric(year),
          day=as.numeric(day),
          month=as.numeric(month),
          date = date(paste(year,month,day,sep="-")),
          yday = yday(date),
          SST=as.numeric(SST)
        ) %>%
        group_by(buoyid, year, yday) %>%
        summarise(meanSST = mean(SST, na.rm = TRUE))%>%
        ungroup()
      
      df<-df%>%
        bind_rows(buoy_data)%>%
        group_by(buoyid,year,yday)%>%
        summarise(meanSST = mean(meanSST,na.rm=T))
    }
  }
  df<-df%>%
    filter(!is.na(buoyid))
  return(df)
}
