pacman::p_load(tidyverse,reshape2,dataRetrieval,lubridate,parallel,foreach,doParallel,rnoaa,MARSS)
source(here::here("functions/get_ersst_v5_new.r"))
source(here::here("functions/getbuoydata.r"))





years<-c(2010:2024)
buoylist<-c("46050","46041","46211","46087","46206")

if(!file.exists(here::here("data/buoy_data.csv"))){
  buoy_data <- getbuoydata(buoylist = buoylist,years=years)
  write.csv(buoy_data,here::here("data/buoy_data.csv"),row.names=F)
}else{
  buoy_data<-read_csv(here::here("data/buoy_data.csv"))
}

nmax<-buoy_data%>%
  group_by(buoyid)%>%
  summarise(n=n())%>%
  filter(n==max(n))%>%
  pull(n)

#length(years)*365

keep<-buoy_data%>%
  filter(!is.na(meanSST))%>%
  group_by(buoyid)%>%
  summarise(n=n())%>%
  filter(n>0.5*nmax)%>%
  pull(buoyid)


marss_dat<-buoy_data%>%
  filter(buoyid%in%keep)%>%
  filter(!is.na(meanSST))%>%
  mutate(date=(make_date(year, 1, 1) + days(yday - 1)))%>%
  pivot_wider(names_from = buoyid,values_from = meanSST,id_cols=date)%>%
  mutate( jdate=julian(date))

all_dates<-tibble(jdate=julian(min(marss_dat$date)):julian(max(marss_dat$date)))

marss_dat<-all_dates%>%
  left_join(marss_dat)%>%
  arrange(jdate)

marss_mat<-marss_dat%>%
  dplyr::select(-c(date,jdate))%>%
  as.matrix()%>%
  t()

model=list(
  Q= "equalvarcov",#"unconstrained",
  R= diag(rep(0,nrow(marss_mat))),#"diagonal and equal","diagonal and unequal",
  U= matrix(rep(0,nrow(marss_mat)),nrow=nrow(marss_mat),1)
)
fit=MARSS(marss_mat, model=model,control=list(maxit=200,allow.degen=T))


fitted<-t(fit$states)
colnames(fitted)<-gsub("X.","mle_",colnames(fitted))

marss_dat<-marss_dat%>%
  dplyr::select(-jdate)%>%
  bind_cols(fitted)%>%
  pivot_longer(cols=c(everything(),-date),names_to = "buoyid")%>%
  mutate(type=ifelse(grepl("mle_",buoyid),"mle","obs"))%>%
  mutate(buoyid=gsub("mle_","",buoyid))%>%
  filter(type=="mle")%>%
  mutate(month=month(date),
         year=year(date)
         )%>%
  group_by(year,month,buoyid)%>%
  summarise(meanSST=mean(value))


SST1<-get_ersst_v5_new(years=c(2010,2024),
                           data.dir=here::here("data"),
                           ncfilename="sst.mnmean_1_23_2024.nc",
                           latrange=c(44,50),
                           lonrange=c(-125,-120)
)%>%
  filter(!is.na(sst))%>%
  group_by(year,month)%>%
  summarize(meanSST=mean(sst,na.rm=T))%>%
  mutate(buoyid="zone",
         year=as.numeric(year),
         month=as.numeric(month)
         )

marss_dat2<-marss_dat%>%
  bind_rows(SST1)%>%
  filter(year < 2024 | month <2)%>%
  pivot_wider(names_from = buoyid,values_from = meanSST,id_cols=c("year","month"))

  
marss_mat2<-marss_dat2%>%
  arrange(year,month)%>%
  ungroup()%>%
  dplyr::select(-c("year","month"))%>%
  as.matrix()%>%
  t()

model=list(
  Q="unconstrained",# "equalvarcov"
  R= diag(rep(0,nrow(marss_mat2))),#"diagonal and equal","diagonal and unequal",
  U= matrix(rep(0,nrow(marss_mat2)),nrow=nrow(marss_mat2),1)
)
fit=MARSS(marss_mat2, model=model,control=list(maxit=2000,allow.degen=T))


fitted<-t(fit$states)
colnames(fitted)<-gsub("X.","mle_",colnames(fitted))

results<-marss_dat2%>%
  bind_cols(fitted)%>%
  pivot_longer(cols=c(everything(),-year,-month),names_to = "buoyid")%>%
  mutate(type=ifelse(grepl("mle_",buoyid),"mle","obs"))%>%
  mutate(buoyid=gsub("mle_","",buoyid))%>%
  filter(buoyid=="zone")%>%
  pivot_wider(names_from = type,values_from = value)%>%
  mutate(SST=ifelse(!is.na(obs),obs,mle))


ggplot(marss_dat2%>%
         filter(month==1)%>%
         pivot_longer(cols=c(everything(),-year,-month),names_to = "buoyid",values_to = "meanSST")
       ,aes(x=as.numeric(year+month/12),y=meanSST,color=buoyid))+
  geom_line()
            
ggplot(results%>%
         filter(month==1)
       ,aes(x=as.numeric(year+month/12),y=`mle`))+
  geom_line(color="blue")+
  geom_line(aes(y=`obs`),color="red",linetype="dashed")

            
write.csv(results%>%
            filter(year==year(Sys.Date())),
          here::here("data/January_ERSST_v5.csv"),row.names = F
          )

         