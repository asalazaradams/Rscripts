setwd("C:/Users/asalazar/Desktop/Rcode")
library(readr)
library(dplyr)


hosp<- read_csv("https://raw.githubusercontent.com/asalazaradams/data/main/hospitals.csv")

supmuni<- read_csv("https://raw.githubusercontent.com/asalazaradams/data/main/supmuni.csv")
supmuni$CVE_GE
class(supmuni$AREA)
supmuni$clave_mun=as.numeric(as.character(supmuni$CVEGEO))
supmuni=supmuni%>%select(clave_mun,AREA)

hosp=merge(hosp,supmuni,by="clave_mun")

metros<- read_csv("https://raw.githubusercontent.com/asalazaradams/data/main/ZM_2015.csv")
metros$clave_mun=metros$CVE_MUN
metros=metros%>%select(clave_mun,CVE_ZM,NOM_ZM)
#metros=metros%>%filter(AREAM!="00")
hosp=merge(hosp,metros, by="clave_mun", all.x =T)
is.na(hosp$CVE_ZM)

hosp$loc=hosp$CVE_ZM

hosp=hosp %>% 
  mutate(loc = coalesce(CVE_ZM,clave_mun))


hosp$servs=hosp$consulta+hosp$procedq

hosp%>%group_by(loc)%>%
  summarise(tot_servs=sum(servs))

smuni=hosp%>%group_by(loc)%>%
  summarise(tot_servs=sum(servs))

hosp=merge(hosp,smuni, all=T)
hosp$mshare=hosp$servs/hosp$tot_servs*100
hosp$mshare2=hosp$mshare**2
hh=hosp%>%group_by(loc)%>%
  summarise(hhi=sum(mshare2))
hosp=merge(hosp,hh,by="loc",all=T)

hhest=hosp%>%group_by(SREENTIDAD)%>%
  summarise(mean(hhi))

hosp%>%group_by(grande)%>%
  summarise(mean(hhi))

hosp%>%group_by(metro)%>%
  summarise(mean(hhi))


summary(hosp$hhi)

hosp$logarea=log(hosp$AREA)

hosp$meds=hosp$persmednom+ hosp$persmedac
hosp$staff=hosp$pers_para+hosp$pers_otro
hosp$logpob=log(hosp$POBTOT)
hosp$loginc=log(hosp$ing_pc)

hosp21=hosp%>%filter(SHPANIO==2021)

names(hosp)
hosp=na.omit(hosp)

hosp=hosp%>%filter(quiro>0|procedq>0)



library(rDEA)


rsu.input=hosp%>% select (meds,staff,camas_c,quiro)
rsu.output=hosp%>% select (procedq,consulta,dias)

rsu.env=hosp%>% select( POBTOT,
                        ing_pc,
                        P60,
                        urb,
                        hhi,
                        gral,
                        chico,
                        grande,
                        metro,
                        centro,
                        noroeste,
                        norte,
                        occidente)




set.seed(123)
start_time <- Sys.time()
hosp.boot.10=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="variable",
                          L1=100, L2=1000, alpha=0.05)
end_time <- Sys.time()
end_time-start_time

summary(hosp.boot.10$delta_hat_hat)
summary(1/hosp.boot.10$delta_hat_hat)


hosp.boot.10$beta_ci

hosp.boot.10$


save.image(file="hosp_05.RData")

#load("hosp2.RData")
