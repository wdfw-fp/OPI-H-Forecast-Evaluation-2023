

install_or_load_pack <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE,repos = "http://cran.us.r-project.org")
  sapply(pack, require, character.only = TRUE)
}
packages_list<-c("tidyverse"
                 ,"forecast"
                 ,"mgcv"
                 ,"ggplot2"
                 ,"MASS"
                 ,"RColorBrewer"
                 ,"kableExtra"
                 ,"lubridate"
                 ,"modelr"
                 ,"kableExtra"
                 ,"reshape2"
                 ,"ggfortify"
                 ,"clock"
                 ,"smooth"
                 ,"scales"
                 ,"gtools"
                 ,"here"
                 ,"MuMIn"
                 ,"janitor"
                 ,"rvest"
)
install_or_load_pack(pack = packages_list)

#=========================================================
#get PDO data
#=========================================================
PDO<-read_table("https://psl.noaa.gov/pdo/data/pdo.timeseries.ersstv5.csv",skip=1,col_names=F,comment="#")%>%
  dplyr::rename(Date=X1,PDO=X2)%>%
  filter(!PDO < -99)%>%
  mutate(Date=as.Date(Date),Month=month(Date),Year=year(Date))%>%
  group_by(Year)%>%
  add_tally()%>%
  #filter(!Month>6)%>% #use only spring (Jan-June) NPGO
  #filter(!n < 12)%>% #use only complete years
  group_by(Year)%>%
  dplyr::summarise(PDO=mean(PDO))%>%
  mutate(lag2_PDO = lag(PDO,2), lag1_PDO = lag(PDO,1))%>%
  dplyr::select(year=Year,lag2_PDO, lag1_PDO)
#=========================================================
#get NPGO data
#=========================================================
NPGO<-read_table("http://www.o3d.org/npgo/npgo.php",skip=29,col_names=F,comment="#")%>%
  filter(!is.na(X2))%>%
  dplyr::rename(Year=X1,Month=X2,NPGO=X3)%>%
  mutate(Year=as.numeric(Year))%>%
  group_by(Year)%>%
  add_tally()%>%
  #filter(!Month>6)%>% #use only spring (Jan-June) NPGO
  filter(!n < 12)%>% #use only complete years
  group_by(Year)%>%
  dplyr::summarise(NPGO=mean(NPGO))%>%
  mutate(Year=Year+1, lag1_NPGO = NPGO,lag2_NPGO = lag(NPGO))%>%
  dplyr::select(year=Year,lag1_NPGO,lag2_NPGO)
#=========================================================
#get PIT tag survival/age data
#=========================================================
PIT<-read_html("https://www.cbr.washington.edu/dart/wrapper?type=php&fname=pitsaresu_1670285794_819.csv")%>%
  html_nodes("body")%>%
  html_text()%>%
  read_lines(skip_empty_rows = T)%>%
  tibble()%>%
  setNames("lines")%>%
  filter(grepl("year",lines) | grepl("20",lines) & !grepl("Generated",lines) & !grepl("DART Data Citation",lines))%>%
  as_vector()%>%
  str_split(",")%>%
  as.data.frame()%>%
  as.matrix()%>%
  t()%>%
  as_tibble()%>%
  row_to_names(row_number = 1)%>%
  type.convert()%>%
  filter(year!=year(Sys.Date()))%>%
  mutate(OutmigrationYear=year,Year=OutmigrationYear+2)

PIT<-PIT%>%bind_cols(data.frame(SAR1=gam(cbind(ocean1Count,juvCount-ocean1Count)~s(OutmigrationYear,k=(dim(PIT)[1]),m=1,bs="ps"),family=binomial,data=PIT)$fitted))%>%
  bind_cols(data.frame(SAR2=c(gam(cbind(ocean2Count,juvCount-ocean2Count)~s(OutmigrationYear,k=(dim(PIT)[1]-1),m=1,bs="ps"),family=binomial,data=PIT)$fitted,NA)))%>%
  mutate(lag1_log_SAR1 = log(SAR1),lag1_log_SAR2=lag(log(SAR2),1))%>%
  dplyr::select(year=Year,lag1_log_SAR1,lag1_log_SAR2)
#=========================================================
#get flow data (might affect timing)
#=========================================================
flow_site<-14128870
flow_url <- paste0("https://waterdata.usgs.gov/nwis/dv?cb_00065=on&format=rdb&site_no=",flow_site,
                   "&referred_module=sw&period=&begin_date=",yr_start,"-01-01",
                   "&end_date=",yr_end,"-12-31")
flow<-readr::read_delim(flow_url,comment = '#')%>%
  filter(agency_cd=="USGS")%>%
  dplyr::rename(date=datetime,stage_height=`113489_00065_00003`)%>%
  dplyr::select(date,stage_height)%>%
  mutate(stage_height = as.numeric(stage_height))

flow<-flow%>%
  mutate(year=year(date),month=month(date),yday=yday(date))%>%
  filter(yday <= yday(max(dat$date)) & yday >= yday(max(dat$date)-6))%>%
  group_by(year)%>%
  dplyr::summarise(zl_flow=mean(stage_height,na.rm=T), .groups = "keep")%>%
  ungroup()%>%
  mutate(zl_flow=as.vector(scale(zl_flow)))

#================================================================
dat<-PDO%>%
  left_join(NPGO)%>%
  left_join(PIT)%>%
  left_join(flow)%>%
  mutate(pink_ind = ifelse(year>1999 & year%%2==0,0,1))%>%
  ungroup()%>%
  filter(
    across(
      .cols = everything(),
      .fns = ~ !is.na(.x)
    )
  )

