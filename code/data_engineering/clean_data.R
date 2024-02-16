library("tidyverse")
library("lubridate")
library("zoo")

train_path="/train_delays/data/raw/pt_results.csv"
df<-read_csv(paste0(getwd(), train_path))

#### 0.Type Conversions and Wrong Encoding ####
df$HALTESTELLEN_NAME<-gsub("Ã¼", "ü", df$HALTESTELLEN_NAME)
df$BETRIEBSTAG<-dmy(df$BETRIEBSTAG)
df$ANKUNFTSZEIT<-dmy_hm(df$ANKUNFTSZEIT)
df$ABFAHRTSZEIT<-dmy_hm(df$ABFAHRTSZEIT)

df$AN_PROGNOSE<-dmy_hms(df$AN_PROGNOSE)
df$AB_PROGNOSE<-dmy_hms(df$AB_PROGNOSE)

df$FAELLT_AUS_TF<-if_else(is.na(df$AN_PROGNOSE), TRUE, df$FAELLT_AUS_TF) # set all trains without data to cancelled
df$delay<-as.numeric(df$AN_PROGNOSE-df$ANKUNFTSZEIT)/60

# accound for shadow train-IDs
df$trainNr<-if_else(df$LINIEN_ID>10000, df$LINIEN_ID-10000, df$LINIEN_ID)

high_train_nr<-df%>%group_by(HALTESTELLEN_NAME, BETRIEBSTAG, trainNr)%>%summarise(count=n())
df<-left_join(df, high_train_nr)
df<-df%>%filter(!(LINIEN_ID<10000 & count>1))%>%select(-"LINIEN_ID")
df$trainNr<-as.factor(df$trainNr)

cancelled_SMA<-df%>%
  filter(HALTESTELLEN_NAME %in% c("St. Margrethen", "St. Margrethen SG"))%>%
  filter(FAELLT_AUS_TF==TRUE)%>%
  select(c("BETRIEBSTAG", "trainNr"))%>%
  mutate(cancelled_SMA=TRUE)

df<-df%>%left_join(cancelled_SMA)

df$cancelled_SMA[is.na(df$cancelled_SMA)] <- FALSE

delay_SMA<-df%>%
  filter(HALTESTELLEN_NAME %in% c("St. Margrethen", "St. Margrethen SG"))%>%
  select(c("BETRIEBSTAG", "trainNr", "delay"))%>%
  rename("delay_SMA"="delay")

df<-df%>%left_join(delay_SMA)


#### 1.Create New Variables & Filter ####
#df$delay<-as.numeric(df$AN_PROGNOSE-df$ANKUNFTSZEIT)/60
df$yearmon<-as.yearmon(df$BETRIEBSTAG)

df<-df%>%filter(BETRIEBSTAG>='2020-12-15') # no data before time table change required
df<-df%>%filter(BETRIEBSTAG<floor_date(max(df$BETRIEBSTAG),'month')) # keep only data for complete months (for the monthly chart)

df<-df%>%filter(HALTESTELLEN_NAME=="Zürich HB")
# to do
# - download weather data
# - join with weather data
# - join with the holidays dataset
# - save the cleaned dataset

holiday_path <- "/train_delays/data/raw/holidays.csv"
df_holidays<-read_csv(paste0(getwd(), holiday_path))

#combine with holiday dataset
df<-df%>%
  select(c(HALTESTELLEN_NAME,BETRIEBSTAG, ANKUNFTSZEIT, AN_PROGNOSE, delay, trainNr, cancelled_SMA, yearmon))%>%
  left_join(df_holidays, by=c("BETRIEBSTAG"="day"))

#combine with addtional dataset
additional_path <- "/train_delays/data/raw/additional_data.csv"
df_additional<-read_csv2(paste0(getwd(), holiday_path))

df_additional[1:2,]%>%transmute(day = map2(start, end, seq, by = "1 day"))%>%unnest%>%distinct

df_additional$start<-dmy(df_additional$start)
df_additional$end<-dmy(df_additional$end)


zh_school_h<-df_additional%>%filter(city=="Zurich")%>%filter(event=="school holidays")%>%
  transmute(day = map2(start, end, seq, by = "1 day"))%>%unnest%>%distinct
zh_school_h$zh_school_h<-TRUE

mc_school_h<-df_additional%>%filter(city=="Munich")%>%filter(event=="school holidays")%>%
  transmute(day = map2(start, end, seq, by = "1 day"))%>%unnest%>%distinct
mc_school_h$mc_school_h<-TRUE

oktoberfest<-df_additional%>%filter(event=="oktoberfest")%>%
  transmute(day = map2(start, end, seq, by = "1 day"))%>%unnest%>%distinct
oktoberfest$oktoberfest<-TRUE

df%>%
  left_join(zh_school_h,  by=c("BETRIEBSTAG"="day"))%>%
  left_join(mc_school_h,  by=c("BETRIEBSTAG"="day"))%>%
  left_join(oktoberfest,  by=c("BETRIEBSTAG"="day"))
