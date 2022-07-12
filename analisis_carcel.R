library(dplyr)
library(readr)
carcel <- read_csv("carcel.csv")

summary(carcel$staff)

sex20=carcel%>%filter(anio==2020)%>%
          group_by(caract=sex)%>%
            summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))

seg20=carcel%>%filter(anio==2020)%>%
  group_by(caract=seg)%>%
  summarise(N=n(),
            size=mean(log),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))


cont20=carcel%>%filter(anio==2020)%>%
  group_by(caract=contrato)%>%
  summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))

cert20=carcel%>%filter(anio==2020)%>%
  group_by(caract=certif)%>%
  summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))

car_sum_20=rbind(cont20,seg20,sex20,cert20)



sex19=carcel%>%filter(anio==2019)%>%
  group_by(caract=sex)%>%
  summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))

seg19=carcel%>%filter(anio==2019)%>%
  group_by(caract=seg)%>%
  summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))


cont19=carcel%>%filter(anio==2019)%>%
  group_by(caract=contrato)%>%
  summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))

cert19=carcel%>%filter(anio==2019)%>%
  group_by(caract=certif)%>%
  summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))

car_sum_19=rbind(cont19,seg19,sex19,cert19)


sex=carcel%>%
  group_by(caract=sex)%>%
  summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))

seg=carcel%>%
  group_by(caract=seg)%>%
  summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))


cont=carcel%>%
  group_by(caract=contrato)%>%
  summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))

cert=carcel%>%
  group_by(caract=certif)%>%
  summarise(N=n(),
            size=mean(capinstt),
            pop=mean(pobtot),
            unsen=mean(psisent),
            crowd=mean(ocup),
            stu=mean(estud/pobtot),
            train=mean(capacit/pobtot),
            staff=mean(staff/pobtot))

car_sum=rbind(cont,seg,sex,cert)





?table
x <- ftable(carcel[c("seg", "contrato")])
x
ftable(x, row.vars = c(2, 4))
table(carcel$certif)
