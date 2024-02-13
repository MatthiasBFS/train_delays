library("tidyverse")
library("lubridate")
library("zoo")

df<-read_csv("~/train_delays/data/raw/pt_results.csv")

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
df<-df%>%filter(BETRIEBSTAG<floor_date(max(df$BETRIEBSTAG),'month'))
df_ZH<-df%>%filter(HALTESTELLEN_NAME=="Zürich HB")


#### 2.Quality Checks ####
days_data<-data.frame(day=unique(df_ZH$BETRIEBSTAG))
days_sequ<-data.frame(day=seq(min(df_ZH$BETRIEBSTAG), max(df$BETRIEBSTAG), "days"))
setdiff(days_sequ, days_data)

#### 3.Create Final Dataset ####
avg_delay<-df_ZH%>%group_by(BETRIEBSTAG)%>%summarise(avg_delay=mean(delay, na.rm=TRUE), count_trains=n(), cancelled=sum(FAELLT_AUS_TF))
df_daily_avg<-left_join(days_sequ,
                        avg_delay,
                        by=c("day"="BETRIEBSTAG"))%>%
  mutate(wday=wday(day, label = TRUE))


#### 4.Explanatory Analysis ####

### How many trains are running per period?
df_ZH%>%group_by(yearmon)%>%summarise(count=n())
plot(df_ZH%>%group_by(yearmon)%>%summarise(count=n()),type="l",ylim=c(0,255))


### Average delay per period
plot(avg_delay$BETRIEBSTAG,avg_delay$avg_delay, type="l")


plot(df_ZH%>%group_by(yearmon)%>%summarise(count=n()), type="l", ylim=c(0,255))
lines(df_ZH%>%filter(FAELLT_AUS_TF==TRUE)%>%group_by(yearmon)%>%summarise(cancelled=n()), col="red")
lines(df_ZH%>%filter(cancelled_SMA==TRUE)%>%group_by(yearmon)%>%summarise(cancelled=n()), col="green")

df_ZH$no_na<-1
df_completed<-df_ZH%>%complete(BETRIEBSTAG, trainNr)
df_completed$yearmon<-as.yearmon(df_completed$BETRIEBSTAG)
df_completed$no_na[is.na(df_completed$no_na)] <- 0


df_completed%>%
  group_by(yearmon, trainNr)%>%
  summarise(count=sum(no_na))%>%
  ggplot(aes(x=yearmon, y=count, group=trainNr,  color=trainNr))+
  #geom_point(position=position_jitter(h=0.15,w=0.15))#+
  geom_line()+
  geom_point()

cancelled_daily<-left_join(days_sequ,
                           df_ZH%>%filter(FAELLT_AUS_TF==TRUE)%>%group_by(BETRIEBSTAG)%>%summarise(cancelled=n()), 
                           by=c("day"="BETRIEBSTAG"))%>%replace(is.na(.), 0)


plot(cancelled_daily,ylim=c(0,7),type="l")
lines(df_ZH%>%group_by(BETRIEBSTAG)%>%summarise(count=n()), type="l", col="red")
lines(df_ZH%>%group_by(BETRIEBSTAG)%>%summarise(count1=sum(cancelled_SMA)), type="l", col="green")

df_ZH%>%ggplot(aes(x=BETRIEBSTAG, y=delay, group=trainNr, color=trainNr))+
  geom_point(aes(alpha=0.01))

df_ZH%>%group_by(yearmon, trainNr)%>%summarise(avg_delay=mean(delay, na.rm = TRUE))%>%
  ggplot(aes(x=yearmon, y=avg_delay, group=trainNr, color=trainNr))+
  geom_line()+
  geom_point()

df_ZH%>%
  filter(FAELLT_AUS_TF==FALSE)%>%filter(cancelled_SMA==FALSE)%>%
  group_by(yearmon)%>%summarise(avg_delay_ZH=mean(delay, na.rm = TRUE),
                                      avg_delay_SMA=mean(delay_SMA, na.rm=TRUE))%>%
  ggplot()+
  geom_line(aes(x=yearmon, y=avg_delay_ZH),color="red")+
  geom_point(aes(x=yearmon, y=avg_delay_ZH),color="red")+
  geom_line(aes(x=yearmon, y=avg_delay_SMA),color="green")+
  geom_point(aes(x=yearmon, y=avg_delay_SMA),color="green")
  
