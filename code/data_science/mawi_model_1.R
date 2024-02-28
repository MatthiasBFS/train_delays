library(tidyverse)
library(mgcv)

load("~/work/train_delays/data/clean/df_clean.RData")

# Let's compute the most delayed train
df%>%group_by(trainNr)%>%summarise(sum_delay=sum(delay, na.rm=TRUE))


df_190<-df%>%filter(trainNr==190)

plot(df_190$BETRIEBSTAG, df_190$delay)

df_190$yday<-yday(df_190$BETRIEBSTAG)
df_190$month<-month(df_190$BETRIEBSTAG)
df_190$year<-year(df_190$BETRIEBSTAG)
df_190$wday<-wday(df_190$BETRIEBSTAG)

#### M1 Based on Day of Year Only ####

mod1 <- gam(delay ~ s(yday,bs = "cr",fx=FALSE,k=40), family=gaussian, data=df_190, na.action=na.omit)

summary(mod1)
plot(mod1,scale=0)
plot(resid(mod1))

#### M2 Add Year ####

mod2 <- gam(delay ~ s(yday,bs = "cr",fx=FALSE,k=4, by=as.factor(year)), family=gaussian, data=df_190, na.action=na.omit)

summary(mod2)
plot(mod2,scale=0)
plot(resid(mod2))

mod3 <- gam(delay ~ s(yday, bs = "cr", fx=FALSE) + as.factor(month) + as.factor(wday), family=gaussian, data=df_190, na.action=na.omit)

summary(mod2)
plot(mod2,scale=0)
plot(resid(mod2))


