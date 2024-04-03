library(dplyr)
library(readr)
library(readxl)

hosp=read_csv("hosp_panel.csv")
nivel<- read_excel("~/Library/CloudStorage/OneDrive-ElColegiodeSonora/ESTADISTICAS/Salud/Publico/2021/ESTABLECIMIENTO_SALUD_202112.xlsx")
nivel=nivel%>%select(CLUES,`NIVEL ATENCION`)

hosp=merge(hosp,nivel,by="CLUES")

rm(nivel)
  # Create dummy variable
hosp$imss<- ifelse(hosp$Institución == "IMSS", 1, 0)
hosp$issste<- ifelse(hosp$Institución == "ISSSTE", 1, 0)
hosp$third= ifelse(hosp$`NIVEL ATENCION`=="TERCER NIVEL",1,0)

hosp$a13= ifelse(hosp$Año==2013,1,0)
hosp$a14= ifelse(hosp$Año==2014,1,0)
hosp$a15= ifelse(hosp$Año==2015,1,0)
hosp$a16= ifelse(hosp$Año==2016,1,0)
hosp$a17= ifelse(hosp$Año==2017,1,0)
hosp$a18= ifelse(hosp$Año==2018,1,0)
hosp$a19= ifelse(hosp$Año==2019,1,0)
hosp$a20= ifelse(hosp$Año==2020,1,0)
hosp$a21= ifelse(hosp$Año==2021,1,0)

hosp$Q1=ifelse(hosp$`TOTAL CAMAS AREA HOSPITALIZACIÓN`<18,1,0)
hosp$Q4=ifelse(hosp$`TOTAL CAMAS AREA HOSPITALIZACIÓN`>=100,1,0)
hosp$Q2=ifelse(hosp$`TOTAL CAMAS AREA HOSPITALIZACIÓN`>=18&
                 hosp$`TOTAL CAMAS AREA HOSPITALIZACIÓN`<37,1,0)
hosp$Q3=ifelse(hosp$`TOTAL CAMAS AREA HOSPITALIZACIÓN`>=37&
                 hosp$`TOTAL CAMAS AREA HOSPITALIZACIÓN`<100,1,0)

summary(hosp$`TOTAL CAMAS AREA HOSPITALIZACIÓN`)

table(hosp$`TOTAL CAMAS AREA HOSPITALIZACIÓN`<18)

hosp$logpob

#hosp$metro=as.numeric()
hosp$centro=as.numeric(hosp$`Clave Estado`==9|
                          hosp$`Clave Estado`==15|
                         hosp$`Clave Estado`==13|
                           hosp$`Clave Estado`==22|
                           hosp$`Clave Estado`==29 )
hosp$norte=as.numeric(hosp$`Clave Estado`==2|
                             hosp$`Clave Estado`==3|
                             hosp$`Clave Estado`==8|
                             hosp$`Clave Estado`==10|
                             hosp$`Clave Estado`==25|
                             hosp$`Clave Estado`==26|
                         hosp$`Clave Estado`==5|
                          hosp$`Clave Estado`==19|
                          hosp$`Clave Estado`==24|
                          hosp$`Clave Estado`==28|
                          hosp$`Clave Estado`==32)

hosp$occidente=as.numeric(hosp$`Clave Estado`==1|
                              hosp$`Clave Estado`==11|
                              hosp$`Clave Estado`==6|
                              hosp$`Clave Estado`==14|
                              hosp$`Clave Estado`==16|
                              hosp$`Clave Estado`==18)

hosp$sur=as.numeric(hosp$`Clave Estado`==4|
                            hosp$`Clave Estado`==12|
                            hosp$`Clave Estado`==7|
                            hosp$`Clave Estado`==20|
                            hosp$`Clave Estado`==30|
                            hosp$`Clave Estado`==17|
                            hosp$`Clave Estado`==21|
                            hosp$`Clave Estado`==23|
                            hosp$`Clave Estado`==27|
                            hosp$`Clave Estado`==31)



hosp$covid=as.numeric(hosp$Año==2020|hosp$Año==2021)
#hosp$staff_ext=hosp$persmednom+hosp$persmedac+hosp$pers_otro+hosp$pers_para


#hosp_panel$tercerniv=ifelse(hosp_panel$Tipología =="HR/HAE")
hosp$logpob=log(hosp$POBTOT)

hosp$loginc=log(hosp$ing_pc)
hosp$exp_elect=hosp$`¿Cuenta con expediente clínico electrónico?`
hosp$doctors_tot=hosp$`Total personal médico en formación`+ hosp$`Total médicos generales especialistas y odonólogos`
hosp$pct_form=hosp$`Total personal médico en formación`/ hosp$doctors_tot
  
hosp19=hosp%>%filter(Año==2019)
which(is.na(hosp), arr.ind=TRUE)
hosp[1312,]
#CSSSA017731 
hosp=hosp%>%filter(CLUES!="CSSSA017731")
hosp2=na.omit(hosp)

write_csv(hosp,file="hosp_13_21.csv")
library(rDEA)


rsu.input=hosp%>% select (`Total médicos generales especialistas y odonólogos`,
                            `Total enfermeras en contacto con el paciente`,
                            `TOTAL CAMAS AREA HOSPITALIZACIÓN`,
                            Quirófanos)

rsu.input.2=hosp%>% select (doctors_tot,
                            `Total enfermeras en contacto con el paciente`,
                            `TOTAL CAMAS AREA HOSPITALIZACIÓN`,
                            Quirófanos)

rsu.output=hosp%>% select (egresos,dias)




rsu.env=hosp%>% select(logpob,
                       loginc,
                       P60,
                       PURB,
                       third,
                       imss,
                       issste,
                       centro,
                       norte,
                       occidente,
                       a13,a14,a15,a16,a17,a18,a19,a20,a21)


rsu.env.2=hosp%>% select(loginc,
                       P60,
                       PURB,
                       pct_form,
                       third,
                       imss,
                       issste,
                       centro,
                       norte,
                       occidente,
                       a13,a14,a15,a16,a17,a18,a19,a20,a21)







#rsu.env=hosp%>% select(Año,
#                       covid,
#                       loginc,
#                       P60,
#                       PURB,
#                       exp_elect,
#                       third,
#                       imss,
#                       issste)

#rsu.env=hosp%>% select(Año,
#                       third,
#                       imss,
#                       issste)

cor(rsu.env)



set.seed(123)
start_time <- Sys.time()
start_time
hosp.boot=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="output", RTS="variable",
                            L1=100, L2=1000, alpha=0.05)
end_time <- Sys.time()
end_time-start_time

summary(hosp.boot$delta_hat_hat)
summary(1/hosp.boot$delta_hat_hat)

hosp.boot$beta_ci
hosp.boot$beta_hat_hat

hosp$eff=1/hosp.boot$delta_hat_hat



set.seed(123)
start_time <- Sys.time()
start_time
hosp.boot.2=dea.env.robust (X=rsu.input.2, Y=rsu.output,  Z=rsu.env.2, model="output", RTS="variable",
                          L1=100, L2=1000, alpha=0.05)
end_time <- Sys.time()
end_time-start_time
hosp.boot.2$beta_ci
hosp.boot.2$beta_hat_hat

save.image("hosp.RData")




hosp%>%group_by(Institución,`NIVEL ATENCION`)%>%
  summarise(n(),beds=mean(`TOTAL CAMAS AREA HOSPITALIZACIÓN`),
            eff=mean(eff))%>%
  print(n=268)

hosp%>%group_by(Institución,covid)%>%
  summarise(n(),beds=mean(`TOTAL CAMAS AREA HOSPITALIZACIÓN`),
            eff=mean(eff))%>%
  print(n=268)

hosp%>%group_by(Año)%>%
  summarise(n(),beds=mean(`TOTAL CAMAS AREA HOSPITALIZACIÓN`),
            eff=mean(eff))%>%
  print(n=268)


hosp%>%group_by(hosp$`NIVEL ATENCION`,Año)%>%
  summarise(beds=sum(`TOTAL CAMAS AREA HOSPITALIZACIÓN`),
            docs=sum(`Total médicos generales especialistas y odonólogos`),
            nurses=sum(`Total enfermeras en contacto con el paciente`),
            student=sum(`Total personal médico en formación`),
            eff=mean(eff))%>%
  print(n=268)

hosp%>%group_by(hosp$`NIVEL ATENCION`)%>%
  summarise(beds=mean(`TOTAL CAMAS AREA HOSPITALIZACIÓN`),
            docs=mean(`Total médicos generales especialistas y odonólogos`),
            nurses=mean(`Total enfermeras en contacto con el paciente`),
            student=mean(`Total personal médico en formación`),
            stu_doc=docs/student,
            eff=mean(eff))%>%
  print(n=268)

hosp%>%group_by(Año)%>%
  summarise(beds=sum(egresos),docs=sum(dias),
            eff=mean(eff))%>%
  print(n=268)

hosp%>%group_by(hosp$`NIVEL ATENCION`, Año)%>%
  summarise(exp=mean(exp_elect),
            docs=mean(`Total médicos generales especialistas y odonólogos`),
            nurses=mean(`Total enfermeras en contacto con el paciente`),
            egresos=mean(egresos),
            p60=mean(P60),
            eff=mean(eff))%>%
print(n=268)


hosp%>%group_by(Año,Institución,`NIVEL ATENCION`)%>%
  summarise(exp=mean(exp_elect),
            income=mean(ing_pc),
            urb=mean(PURB),
            old=mean(P60),
            eff=mean(eff))%>%
  print(n=268)


hosp%>%group_by(Año)%>%
  summarise(eff=mean(eff))%>%
  print(n=268)

hosp%>%group_by(centro, norte, occidente,sur)%>%
  summarise(eff=mean(eff))%>%
  print(n=268)
