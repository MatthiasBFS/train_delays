library(tidyverse)
library(mgcv)

load("~/work/train_delays/data/clean/df_clean.RData")

# Let's compute the most delayed train
df%>%group_by(trainNr)%>%summarise(sum_delay=sum(delay, na.rm=TRUE))

df_190<-df%>%filter(trainNr==190)
df_190$t <- seq.int(nrow(df_190))

plot(df_190$BETRIEBSTAG, df_190$delay)

df_190$yday<-yday(df_190$BETRIEBSTAG)
df_190$month<-month(df_190$BETRIEBSTAG)
df_190$year<-year(df_190$BETRIEBSTAG)
df_190$wday<-wday(df_190$BETRIEBSTAG)

df_190<-df_190%>%
  left_join(
    df_nbr_trains<-df%>%
      group_by(BETRIEBSTAG)%>%
      summarise(number_trains=n()))

#### M1 Based On Index Only ####

mod1 <- gam(delay ~ s(t,bs = "ts",fx=FALSE,k=40), family=gaussian, data=df_190, na.action=na.omit)

summary(mod1)
plot(mod1,scale=0)
plot(resid(mod1))

predict1<-predict(mod1, df_190)

plot(df_190$BETRIEBSTAG, df_190$delay, col="red")
lines(df_190$BETRIEBSTAG, predict1)


#### M2 Add Year ####

mod2 <- gam(delay ~ s(t, bs = "ts", fx=FALSE, k=40) + s(yday, bs = "ts", fx=FALSE) + as.factor(month) + as.factor(wday)+number_trains, family=gaussian, data=df_190, na.action=na.omit)

summary(mod2)
plot(mod2,scale=0)
plot(resid(mod2))

predict2<-predict(mod2, df_190)

plot(df_190$BETRIEBSTAG, df_190$delay, col="red")
lines(df_190$BETRIEBSTAG, predict2)