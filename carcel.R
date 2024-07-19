library(dplyr)

carcel_cont=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/infraestruc_peniten_cnsipee2021_csv/conjunto_de_datos/m1s1p2_cnsipee2021.csv")
carcel_cap=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/infraestruc_peniten_cnsipee2021_csv/conjunto_de_datos/m1s1p4_cnsipee2021.csv")
carcel_pob=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/pob_privad_libert_cnsipee2021_csv/conjunto_de_datos/m1s2p32_cnsipee2021.csv")
sum(carcel_pob$sexostt)
sum(carcel_pob$sexos1,na.rm = T)
sum(carcel_pob$sexos2,na.rm = T)

carcel_staff=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/rec_huma_cnsipee2021_csv (1)/conjunto_de_datos/m1s1p16_cnsipee2021.csv")
carcel_staff2=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/rec_huma_cnsipee2021_csv (1)/conjunto_de_datos/m1s1p24_cnsipee2021.csv")

carcel_rein=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/act_ori_reinser_soc_cnsipee2021_csv/conjunto_de_datos/m1s3p12_cnsipee2021.csv")
carcel_prod=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/act_ori_reinser_soc_cnsipee2021_csv/conjunto_de_datos/m1s3p17_cnsipee2021.csv")
carcel_prod2=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/act_ori_reinser_soc_cnsipee2021_csv/conjunto_de_datos/m1s3p18_cnsipee2021.csv")
carcel_tec=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/infraestruc_peniten_cnsipee2021_csv/conjunto_de_datos/m1s1p9_cnsipee2021.csv")
carcel_cert=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/infraestruc_peniten_cnsipee2021_csv/conjunto_de_datos/m1s1p3_cnsipee2021.csv")
carcel_bio=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/infraestruc_peniten_cnsipee2021_csv/conjunto_de_datos/m1s1p10_cnsipee2021.csv")
carcel_tipo=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/infraestruc_peniten_cnsipee2021_csv/conjunto_de_datos/m1s1p1a_cnsipee2021.csv")
carcel_sentencia=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/pob_privad_libert_cnsipee2021_csv/conjunto_de_datos/m1s2p34_cnsipee2021.csv")
carcel_presup=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/rec_presupue_cnsipee2021_csv/conjunto_de_datos/m1s1p55_cnsipee2021.csv")
carcel_incid=read.csv("~/Documents/ESTADISTICAS MEXICO/Carcel/incide_cent_penit_cnsipee2021_csv/conjunto_de_datos/m1s3p1_cnsipee2021.csv")



carcel_cap$id=carcel_cap$entidad_a*1000+carcel_cap$numeral
carcel_pob$id=carcel_pob$entidad_a*1000+carcel_pob$numeral
carcel_staff$id=carcel_staff$entidad_a*1000+carcel_staff$numeral
carcel_staff2$id=carcel_staff2$entidad_a*1000+carcel_staff2$numeral
carcel_cont$id=carcel_cont$entidad_a*1000+carcel_cont$numeral
carcel_rein$id=carcel_rein$entidad_a*1000+carcel_rein$numeral
carcel_prod$id=carcel_prod$entidad_a*1000+carcel_prod$numeral
carcel_tipo$id=carcel_tipo$entidad_a*1000+carcel_tipo$numeral
carcel_sentencia$id=carcel_sentencia$entidad_a*1000+carcel_tipo$numeral
carcel_tec$id=carcel_tec$entidad_a*1000+carcel_tec$numeral
carcel_bio$id=carcel_bio$entidad_a*1000+carcel_bio$numeral
carcel_cert$id=carcel_cert$entidad_a*1000+carcel_cert$numeral
carcel_incid$id=carcel_incid$entidad_a*1000+carcel_incid$numeral

carcel_cap$capinstt/carcel_pob$sexostt

carcel_cap=carcel_cap%>%select(id,entidad_a,capinstt)
carcel_pob$pobtot=carcel_pob$sexostt
carcel_pob$pobh=carcel_pob$sexos1
carcel_pob$pobm=carcel_pob$sexos2
carcel_pob$pobh[is.na(carcel_pob$pobh)]=0
carcel_pob$pobm[is.na(carcel_pob$pobm)]=0
carcel_pob%>%select(id,pobtot,pobh,pobm)


carcel_staff$staff=carcel_staff$sexostt
carcel_staff=carcel_staff%>%select(id,staff)


carcel_staff2$guardias=as.numeric(as.character(carcel_staff2$carsex37))+
  as.numeric(as.character(carcel_staff2$carsex38))+
  as.numeric(as.character(carcel_staff2$carsex39))+
  as.numeric(as.character(carcel_staff2$carsex40))+
  as.numeric(as.character(carcel_staff2$carsex41))+
  as.numeric(as.character(carcel_staff2$carsex42))

carcel_guard=carcel_staff2%>%select(id,guardias)

carcel_cont$contrato=carcel_cont$rensnn1
carcel_cont=carcel_cont%>%select(id,contrato)
#carcel_sentencia$pobsinsent= carcel_sentencia$esjucss1
#carcel_sentencia=carcel_sentencia%>%select(id,pobsinsent)
carcel_rein$estud=carcel_rein$activr1
carcel_rein$capacit=carcel_rein$activr3
carcel_rein=carcel_rein%>%select(id,estud,capacit)

carcel_tec$survtec=carcel_tec$rensnn1
carcel_tec=carcel_tec%>%select(id,survtec)
carcel_bio$biometr=carcel_bio$rensnn1
carcel_bio=carcel_bio%>%select(id,biometr)
class(carcel_bio$biometr)
carcel_incid$persincid=carcel_incid$totalca1
carcel_incid=carcel_incid%>%select(id,persincid)
carcel_bio$biometr=recode(carcel_bio$biometr, `2` = 0L)
carcel_tec$survtec=recode(carcel_tec$survtec, `2` = 0L)
carcel_cont$contrato=recode(carcel_cont$contrato, `2` = 0L)
carcel_tipo$tipo=recode(carcel_tipo$centpeni_a,
                        `1` = "men",
                        `2` = "women",
                        `3` = "mixed",
                        `4` = "men",
                        `5` = "women",
                        `6` = "mixed",
                        `7` = "men",
                        `8` = "women",
                        `9` = "mixed",
                        `10` = "men",
                        `11` = "women",
                        `12` = "mixed",
                        `13` = "men",
                        `14` = "women",
                        `15` = "mixed",
                        `16` = "men",
                        `17` = "men",
                        `18` = "women",
                        `19` = "mixed")
                       
carcel_cert$certif=carcel_cert$rensnn1
carcel_cert$certif=recode(carcel_cert$certif, `2` = 0L)

carcel_cert=carcel_cert%>%select(id,certif)

carcel_sentencia$sinsent=carcel_sentencia$esjucss1
carcel_sentencia=carcel_sentencia%>%select(id,sinsent)

carcel=merge(carcel_cap,carcel_pob,by="id")
carcel=merge(carcel,carcel_staff,by="id")
carcel=merge(carcel,carcel_cont,by="id")
carcel=merge(carcel,carcel_bio,by="id")
carcel=merge(carcel,carcel_tec,by="id")
carcel=merge(carcel,carcel_cert,by="id")
carcel=merge(carcel,carcel_incid,by="id")



#carcel=merge(carcel,carcel_sentencia,by="id")
#carcel$pctss=carcel$pobsinsent/carcel$pobtot

carcel=merge(carcel,carcel_rein,by="id")
carcel=merge(carcel,carcel_sentencia,by="id")

carcel$desorden=carcel$persincid/carcel$capinstt
summary(carcel$desorden)
carcel$orden=1-carcel$desorden
summary(carcel$orden)

#carcel=merge(carcel,carcel_prod,by="id")
carcel$sobre=carcel$pobtot/carcel$capinstt

#plot(carcel$pctss,log(carcel$sobre))


carcel$q=1/carcel$sobre

carcel$estudratio=carcel$estud/carcel$pobtot

carcel$capratio=carcel$capacit/carcel$pobtot

carcel$psratio=carcel$pobtot/carcel$staff


plot(carcel$sobre,carcel$capratio)
plot(carcel$sobre,carcel$estudratio)
carcel=carcel%>%filter(capinstt>0)
carcel=carcel%>%filter(pobtot>0)
library(deaR)
sum(carcel$sinsent)

carcel$sentratio=carcel$sinsent/carcel$pobtot
summary(carcel$sentratio)
plot(carcel$sobre,carcel$sentratio)
carcel2=carcel%>%select(id,capinstt,staff,pobtot,estud,capacit,q,orden)

DEA_car=read_data(carcel2,
                   dmus = 1,
                   inputs= 2:3,
                   outputs= 4:8)

result_vrs=model_basic(DEA_car,
                       orientation = "io",
                       rts="vrs") 
rts=rts(result_vrs)
lambdas(result_vrs)
eff=efficiencies(result_vrs)


carcel$eff=eff

plot(carcel$sobre,carcel$eff)
summary(lm(eff~sobre+contrato+tipo,data=carcel))



result_crs=model_basic(DEA_car,
                       orientation = "io",
                       rts="crs") 
rts=rts(result_crs)
lambdas(result_crs)
eff_crs=efficiencies(result_crs)


carcel2$eff_crs=eff_crs
carcel2$rts=rts$rts

carcel2 %>% 
  group_by(rts) %>%
  summarise(min(capinstt),
            mean(capinstt),
            max(capinstt))
carcel_big=carcel2 %>% filter (capinstt>=292)
summary(carcel_big$rts)
summary(carcel_big$eff)
summary(carcel_big$q)

carcel_small=carcel2 %>% filter (capinstt<292)
summary(carcel_small$rts)
summary(carcel_small$eff)
summary(carcel_small$q)

mean(carcel_big$activr1/carcel_big$sexostt.x)
mean(carcel_small$activr1/carcel_small$sexostt.x)

summary(carcel2$capinstt)
summary(carcel2$sexostt.x)

1/mean(carcel_big$q)
1/mean(carcel_small$q)
1/summary(carcel_big$q)
1/summary(carcel_small$q)


carcel%>% 
  group_by(contrato) %>%
  summarise(mean(eff),
            mean(orden),
            n())
carcel%>% 
  group_by(survtec) %>%
  summarise(mean(eff),
            mean(orden),
            mean(sobre),
            n())
carcel%>% 
  group_by(biometr) %>%
  summarise(mean(eff),
            mean(sobre),
            mean(orden),
            n())

carcel%>% 
  group_by(certif) %>%
  summarise(mean(eff),
            mean(sobre),
            mean(orden),
            n())

boxplot(carcel$eff~carcel$certif)
wilcox.test(carcel$eff~carcel$certif)

boxplot(carcel$eff~carcel$contrato)
wilcox.test(carcel$eff~carcel$contrato)

boxplot(carcel$sobre~carcel$certif)
wilcox.test(carcel$eff~carcel$certif)

boxplot(carcel$orden~carcel$certif)
wilcox.test(carcel$orden~carcel$certif)


carcel_estados=carcel%>% 
  group_by(entidad_a) %>%
  summarise(mean(eff),
            sd(eff),
            n())
mean(carcel$eff/carcel$eff_crs)

carcel=merge(carcel,carcel_tipo,by="id")
carcel%>% 
  group_by(tipo) %>%
  summarise(mean(eff),
            mean(sobre),
            mean(orden),
            n())

boxplot(carcel$eff~carcel$tipo)


carcel$anio.y=as.numeric(as.character(carcel$anio.y))
plot(carcel$sobre~carcel$capinstt)
plot(carcel$sobre~carcel$orden)
                      
summary(carcel$sobre)
quantile(carcel$sobre,c(.5,.8,.85,.9,.92,.95,1))

carcel%>% filter(sobre>1) %>%
  summarise(1-mean(orden),
            n())

carcel%>% filter(sobre<=1) %>%
  summarise(1-mean(orden),
            n())


110/249
84/249

sum(carcel$staff)
summary(carcel$capinstt)
sum(carcel_cap$capinstt)
hist(carcel$capinstt)

carcel%>% filter(contrato==1)%>%
  summarise(mean(capinstt),
            mean(pobtot),
            mean(sobre),
            n())
write.csv(carcel,file="carcel.csv")
