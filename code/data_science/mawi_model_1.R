library(tidyverse)
library(mgcv)
library(broom)

load("~/work/train_delays/data/clean/df_clean.RData")

# Some Data Engineering
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

df_190<-df_190%>%arrange(BETRIEBSTAG)

# Check Data

hist(df_190$delay, breaks=10)

with(df_190%>%filter(delay<60), hist(delay))

summary(df_190$delay)
sd(df_190$delay, na.rm=TRUE)

qqnorm(df_190$delay)

#### Auxiliary Functions ####
f_collect_results <- function(df, mod){
  output<-list(
    deparse(substitute(mod)),
    summary(mod)$dev.expl, 
    summary(mod)$r.sq)
  
  df_results<-data.frame(output)
  colnames(df_results)<-c("model", "dev.epxl","rsq")
  
  
  pred <- predict(mod, df)
  pred <- data.frame(pred)
  pred$mod <-deparse(substitute(mod))
  
  return(list(df_results, pred))
  
}

p<-function(df, res){
  plot(df$BETRIEBSTAG, df$delay, col="red")
  lines(df$BETRIEBSTAG, res[[2]]$pred)
}

#### M1.1 Based On Time Index Only ####

mod1.1 <- gam(delay ~ s(t,fx=FALSE, k=40), family=gaussian, data=df_190, na.action=na.omit)

res1.1<-f_collect_results(df_190,mod1.1)

summary(mod1.1)
plot(mod1.1,scale=0)
plot(resid(mod1.1))

p(df_190, res1.1)


#### M1.2 Based On Time Only ####

mod1.2 <- gam(delay ~ s(t, fx=FALSE, k=40) + s(yday, fx=FALSE) + as.factor(month) + as.factor(wday), family=gaussian, data=df_190, na.action=na.omit)

res1.2<-f_collect_results(df_190,mod1.2)

summary(mod1.2)
plot(mod1.2,scale=0)
plot(resid(mod1.2))

p(df_190, res1.2)


#### M1.3 Based On Additional Input ####

mod1.3 <- gam(delay ~ s(t, fx=FALSE, k=40) + s(yday, fx=FALSE) + as.factor(month) + as.factor(wday)+number_trains+holiday_zurich+holiday_munich+zh_school_h+mc_school_h+temp_max+temp_min, family=gaussian, data=df_190, na.action=na.omit)

res1.3<-f_collect_results(df_190,mod1.3)

summary(mod1.3)
plot(mod1.2,scal3=0)
plot(resid(mod1.2))

p(df_190, res1.3)


#### M1 Let's look at results ####

results<-rbind(res1.1[[1]], res1.2[[1]], res1.3[[1]])

plot(factor(results$model),results$rsq)
title("Rsquared")

plot(factor(results$model),results$dev.epxl)
title("Deviance Explained")
