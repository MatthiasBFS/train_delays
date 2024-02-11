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

# accound for shadow train-IDs
df$trainNr<-if_else(df$LINIEN_ID>10000, df$LINIEN_ID-10000, df$LINIEN_ID)

high_train_nr<-df%>%group_by(HALTESTELLEN_NAME, BETRIEBSTAG, trainNr)%>%summarise(count=n())
df<-left_join(df, high_train_nr)
df<-df%>%filter(!(LINIEN_ID<10000 & count>1))%>%select(-"LINIEN_ID")
df$trainNr<-as.factor(df$trainNr)


#### 1.Create New Variables & Filter ####
df$delay<-as.numeric(df$AN_PROGNOSE-df$ANKUNFTSZEIT)/60
df$yearmon<-as.yearmon(df$BETRIEBSTAG)

df<-df%>%filter(BETRIEBSTAG>='2020-12-15') # no data before time table change required
df<-df%>%filter(BETRIEBSTAG<floor_date(max(df$BETRIEBSTAG),'month'))
df<-df%>%filter(HALTESTELLEN_NAME=="Zürich HB")


#### 2.Quality Checks ####
days_data<-data.frame(day=unique(df$BETRIEBSTAG))
days_sequ<-data.frame(day=seq(min(df$BETRIEBSTAG), max(df$BETRIEBSTAG), "days"))
setdiff(days_sequ, days_data)

#### 3.Create Final Dataset ####
avg_delay<-df%>%group_by(BETRIEBSTAG)%>%summarise(avg_delay=mean(delay, na.rm=TRUE), count_trains=n(), cancelled=sum(FAELLT_AUS_TF))
df_daily_avg<-left_join(days_sequ,
                        avg_delay,
                        by=c("day"="BETRIEBSTAG"))%>%
  mutate(wday=wday(day, label = TRUE))


#### 4.Explanatory Analysis ####

### How many trains are running per period?
df%>%group_by(yearmon)%>%summarise(count=n())
plot(df%>%group_by(yearmon)%>%summarise(count=n()),type="l",ylim=c(0,255))


### Average delay per period
plot(avg_delay$BETRIEBSTAG,avg_delay$avg_delay, type="l")


plot(df%>%group_by(yearmon)%>%summarise(count=n()), type="l", ylim=c(0,255))
lines(df%>%filter(FAELLT_AUS_TF==TRUE)%>%group_by(yearmon)%>%summarise(cancelled=n()), col="red")

df$no_na<-1
df_completed<-df%>%complete(BETRIEBSTAG, trainNr)
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
                           df%>%filter(FAELLT_AUS_TF==TRUE)%>%group_by(BETRIEBSTAG)%>%summarise(cancelled=n()), 
                           by=c("day"="BETRIEBSTAG"))%>%replace(is.na(.), 0)


plot(cancelled_daily,ylim=c(0,7),type="l")
lines(df%>%group_by(BETRIEBSTAG)%>%summarise(count=n()), type="l", col="red")

df%>%ggplot(aes(x=BETRIEBSTAG, y=delay, group=trainNr, color=trainNr))+
  geom_point(aes(alpha=0.01))

df%>%group_by(yearmon, trainNr)%>%summarise(avg_delay=mean(delay, na.rm = TRUE))%>%
  ggplot(aes(x=yearmon, y=avg_delay, group=trainNr, color=trainNr))+
  geom_line()+
  geom_point()

df%>%group_by(yearmon)%>%summarise(avg_delay=mean(delay, na.rm = TRUE))%>%
  ggplot(aes(x=yearmon, y=avg_delay))+
  geom_line()+
  geom_point()
