
library(readr)
library(dplyr)


hosp=read_csv("https://raw.githubusercontent.com/asalazaradams/data/main/hospitals_metro.csv")

hosp$meds=hosp$persmednom+ hosp$persmedac
hosp$staff=hosp$pers_para+hosp$pers_otro
hosp$logpob=log(hosp$POBTOT)
hosp$loginc=log(hosp$ing_pc)

#hosp21=hosp%>%filter(SHPANIO==2021)

names(hosp)
#hosp=na.omit(hosp)

hosp=hosp%>%filter(quiro>0|procedq>0)


hosp$central=hosp$metro+hosp$centro
hosp$north=hosp$norte+hosp$noroeste
hosp$medsac=hosp$persmedac/hosp$meds

hosp%>%filter(SHPANIO==2022)
hosp$big=0
hosp$big[which(hosp$camas_c>24)]=1
hosp$big

hosp$small=0
hosp$small[which(hosp$camas_c<8)]=1
summary(hosp$big)
summary(hosp$small)

library(rDEA)

h22=hosp%>%filter(SHPANIO==2022)

rsu.input=hosp%>% select (meds,staff,camas_c,quiro)
rsu.output=hosp%>% select (procedq,consulta,dias)



rsu.env=hosp%>% select(SHPANIO,
                        loginc,
                        P60,
                        urb,
                        hhi,
                        spec,
                        medsac,
                        central,
                        north,
                        west,
                        big,small)

#summary(hosp$camas_c)
cor(rsu.env)

set.seed(12345)
start_time <- Sys.time()
start_time
hosp.boot.1=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="constant",
                             L1=100, L2=1000, alpha=0.05)
end_time <- Sys.time()
end_time-start_time

summary(hosp.boot.1$delta_hat_hat)
summary(1/hosp.boot.1$delta_hat_hat)


hosp.boot.1$beta_ci

hosp.boot.1$beta_hat_hat

summary(hosp$hhi)




set.seed(123)
start_time <- Sys.time()
start_time
hosp.boot.05=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="variable",
                          L1=100, L2=1000, alpha=0.05)
end_time <- Sys.time()
end_time-start_time

hosp.boot.05$beta_ci
hosp.boot.05$beta_hat_hat
hosp.boot.05$sigma_hat_hat


start_time <- Sys.time()
start_time
hosp.boot.01=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="variable",
                             L1=100, L2=1000, alpha=0.01)
end_time <- Sys.time()
end_time-start_time

hosp.boot.01$beta_ci
hosp.boot.01$beta_hat_hat
hosp.boot.01$sigma_hat_hat

save.image(file="hosp_FEB20.RData")

load



#ESTAD DESCRIPTIVA
summary(hosp$meds)
summary(hosp$staff)
summary(hosp$camas_c)
summary(hosp$quiro)
summary(hosp$procedq)
summary(hosp$consulta)
summary(hosp$dias)
summary(hosp$POBTOT)
summary(hosp$ing_pc)
summary(hosp$P60)
summary(hosp$urb)
summary(hosp$hhi)
summary(hosp$spec)
summary(hosp$small)
summary(hosp$big)
summary(hosp$central)
summary(hosp$north)
summary(hosp$west)

library(dplyr)
hosp$ef_un=1/hosp.boot.1$delta_hat_hat
hosp$ef_bi=1/hosp.boot.1$delta_hat
summary(hosp$ef_un)
summary(hosp$ef_bi)

hosp%>%group_by(spec)%>%
  summarise(n=n(),unbiases=mean(ef_un),biased=mean(ef_bi))

hosp%>%group_by(central,spec)%>%
  summarise(n=n(),mean=mean(ef_un))

hosp%>%group_by(central)%>%
  summarise(n=n(),mean=mean(ef_un))

hosp%>%group_by(region)%>%
  summarise(n=n(),mean=mean(ef_un))

hosp%>%group_by(size)%>%
  summarise(n=n(),mean=mean(ef_un))

eff_chars=hosp%>%group_by(spec,size,region)%>%
  summarise(mean=mean(ef_un))


hosp%>%group_by(size)%>%
  summarise(mean=mean(procedq))

hosp=hosp %>%
  mutate(region = case_when(
    north == 1 & central == 0 & west ==0 ~ "north",
    north == 0 & central == 1 & west ==0 ~ "central",
    north == 0 & central == 0 & west ==1 ~ "west",
    north == 0 & central == 0 & west ==0 ~ "south",
  ))

hosp=hosp %>%
  mutate(size = case_when(
    big == 1 & small == 0 ~ "big",
    big== 0 & small == 1 ~ "small",
    big == 0 & small == 0 ~ "med",
  ))




#BOOT SURG
hosp%>%group_by(size)%>%
  summarise(consulta=mean(consulta),
            procedq=mean(procedq),
            dias=mean(dias),
            )

hosp%>%group_by(size,spec)%>%
  summarise(meds=mean(meds),
            staff=mean(staff),
            camas=mean(camas_c),
  )

hosp%>%group_by(region,SHPANIO)%>%
  summarise(hhi=mean(hhi),
            camas=mean(camas_c))

hosp%>%group_by(size)%>%
  summarise(n=n(),mean=mean(ef_un))


table(hosp$camas_c>100)
