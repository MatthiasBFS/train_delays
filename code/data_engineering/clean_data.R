library("tidyverse")
library("lubridate")
library("zoo")

df<-read_csv("train_delays/data/raw/pt_results.csv")

# Type Conversions and Wrong Encoding
df$HALTESTELLEN_NAME<-gsub("Ã¼", "ü", df$HALTESTELLEN_NAME)
df$BETRIEBSTAG<-dmy(df$BETRIEBSTAG)
df$ANKUNFTSZEIT<-dmy_hm(df$ANKUNFTSZEIT)
df$ABFAHRTSZEIT<-dmy_hm(df$ABFAHRTSZEIT)

df$AN_PROGNOSE<-dmy_hms(df$AN_PROGNOSE)
df$AB_PROGNOSE<-dmy_hms(df$AB_PROGNOSE)

df$FAELLT_AUS_TF<-if_else(is.na(df$AN_PROGNOSE), TRUE, df$FAELLT_AUS_TF) # set all trains without data to cancelled


# Create New Variables & Filter
df$delay<-as.numeric(df$AN_PROGNOSE-df$ANKUNFTSZEIT)/60
df$yearmon<-as.yearmon(df$BETRIEBSTAG)

df<-df%>%filter(BETRIEBSTAG>='2020-12-15') # no data before time table change required
df<-df%>%filter(HALTESTELLEN_NAME=="Zürich HB")


# Quality Checks
days_data<-data.frame(day=unique(df$BETRIEBSTAG))
days_sequ<-data.frame(day=seq(min(df$BETRIEBSTAG), max(df$BETRIEBSTAG), "days"))
setdiff(days_sequ, days_data)

# Explanatory Analysis

plot(df%>%group_by(yearmon)%>%summarise(count=n()), type="l")

df$delay<-as.numeric(df$AN_PROGNOSE-df$ANKUNFTSZEIT)/60
df$yearmon<-as.yearmon(df$BETRIEBSTAG)

plot(df%>%group_by(yearmon)%>%summarise(avg_delay=mean(delay, na.rm=TRUE)), type="l")

df$AN_PROGNOSE



plot(df%>%group_by(yearmon)%>%summarise(count=n()), type="l", ylim=c(0,255))
lines(df%>%filter(FAELLT_AUS_TF==TRUE)%>%group_by(yearmon)%>%summarise(cancelled=n()))

plot(df%>%filter(FAELLT_AUS_TF==TRUE)%>%group_by(yearmon)%>%summarise(cancelled=n()))
