library(foreign)
catastro=read.dbf("C:/Users/asalazar/Documents/HERMOSILLO2019/HERMOSILLO2019/municipio total/predio.dbf")
library(stringr)
library(dplyr)
catastro$COLONIA=str_remove(catastro$COLONIA, "FRACCIONAMIENTO ")
catastro$COLONIA=str_remove(catastro$COLONIA, "POB ")
catastro$COLONIA=str_remove(catastro$COLONIA, "RESID. ")

str_detect(catastro$COLONIA, "QUINTAS DEL SOL")


catastro=catastro%>%filter(VALOR_CAT>0)
val_colonia=catastro%>%group_by(COLONIA)%>%
  summarise(valor=mean(VALTOT, na.rm = T), 
            valorcat=mean(VALOR_CAT,na.rm = T),
            valorcat_med=median(VALOR_CAT),
            predios=n())

#Agua212021 <- read_excel("Agua212021/Agua212021.xlsx")
library(readxl)
Agua21 <- read_excel("C:/Users/asalazar/Desktop/FreeRiders/FreeRiders2/2021.xlsx")

Agua21$COLONIA=str_remove(Agua21$COLONIA, " (INVASION)")


Agua21$cortado=0
which(Agua21$ESTADO_SUMINISTRO=="Cortado por impago")
Agua21$cortado[which(Agua21$ESTADO_SUMINISTRO=="Cortado por impago")]=1

Agua21$moroso=0
Agua21$moroso[which(Agua21$RECIBOS_VENCIDOS>1)]=1

Agua21$sin_med=0
Agua21$cons_medido=0
Agua21$cons_medido[which(Agua21$TIPO_FACTURA_DIC=="MEDIDO")]=1
summary(Agua21$cons_medido)

table(Agua21$TIPO_FACTURA_ENE)
table(Agua21$TIPO_FACTURA_ABR)
table(Agua21$TIPO_FACTURA_JUL)
table(Agua21$TIPO_FACTURA_SEP)
table(Agua21$TIPO_FACTURA_DIC)

#Agua212021=na.omit(Agua212021)

morosos_colonia=Agua21%>%group_by(COLONIA)%>%
  summarise(p_cortado=mean(cortado,na.rm = T), 
            adeudo=mean(IMPORTE_ADEDUDO,na.rm = T),
            recibos_vencidos=mean(RECIBOS_VENCIDOS,na.rm = T),
            total_tomas=n(),
            medicion_efe=sum(TIPO_FACTURA_ENE=="MEDIDO")/n(),
            pct_morosidad=mean(moroso),
            conumo=mean(M3_ENE))

mor_col=na.omit(morosos_colonia)
plot(mor_col$medicion_efe, mor_col$pct_morosidad)



merge(morosos_colonia,val_colonia, by="COLONIA")
morosos_catastro=merge(morosos_colonia,val_colonia, by="COLONIA" )
morosos_valor=merge(Agua21,val_colonia, by="COLONIA" )

plot(morosos_catastro$valorcat, morosos_catastro$pct_morosidad)
library(dplyr)
morosos2=morosos_catastro
plot(morosos2$valorcat, morosos2$pct_morosidad)
plot(log(morosos2$valorcat), morosos2$pct_morosidad)
plot(log(morosos2$valorcat), morosos2$medicion_efe)
plot(log(morosos2$valorcat), morosos2$recibos_vencidos)

exp(14)

summary(morosos2$valorcat)

Agua21%>%group_by(TARIFA)%>%
  summarise(adeudo=mean(IMPORTE_ADEDUDO),
            adeudo_total=sum(IMPORTE_ADEDUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            rvc=mean(RECIBOS_VENCIDOS*cortado),
            morosidad=mean(moroso),
            n())

Agua21%>%group_by(TIPO_FACTURA_ENE)%>%
  summarise(adeudo=mean(IMPORTE_ADEDUDO),
            adeudo_total=sum(IMPORTE_ADEDUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            rvc=mean(RECIBOS_VENCIDOS*cortado),
            morosidad=mean(moroso),
            n())



Agua21%>%filter(TARIFA=="DOMESTICO")%>%
  group_by(TIPO_FACTURA_ENE)%>%
  summarise(adeudo_medio=mean(IMPORTE_ADEDUDO),
            adeudo_total=sum(IMPORTE_ADEDUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            consumo=mean(M3_ENE),
            morosidad=mean(moroso),
            n())


Agua21%>%filter(TARIFA=="COMERCIAL")%>%
  group_by(TIPO_FACTURA_ENE)%>%
  summarise(adeudo_medio=mean(IMPORTE_ADEDUDO),
            adeudo_total=sum(IMPORTE_ADEDUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            consumo=mean(M3_ENE),
            morosidad=mean(moroso),
            n())


Agua21%>%filter(TARIFA=="GOBIERNO")%>%
  group_by(TIPO_FACTURA_ENE)%>%
  summarise(adeudo_medio=mean(IMPORTE_ADEDUDO),
            adeudo_total=sum(IMPORTE_ADEDUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            consumo=mean(M3_ENE),
            morosidad=mean(moroso),
            n())

morosos_valor$valorcatM=morosos_valor$valorcat/1000000

model1=glm(moroso~TIPO_FACTURA_DIC+TARIFA,family="binomial", data=Agua21)
summary(model1)
mod_nul0 <- glm(moroso ~ 1, family="binomial",data=Agua21)
1-(logLik(model1)/logLik(mod_nul0))

model2=glm(moroso~TIPO_FACTURA_DIC+TARIFA+log(valorcat),family="binomial", data=morosos_valor)
summary(model2)
1-(logLik(model2)/logLik(mod_nul))



model2.1=glm(moroso~TIPO_FACTURA_DIC+TARIFA+log(valorcat_med),family="binomial", data=morosos_valor)
summary(model2.1)
1-(logLik(model2.1)/logLik(mod_nul))

model2.2=glm(moroso~TIPO_FACTURA_DIC+TARIFA,family="binomial", data=morosos_valor)
summary(model2.2)
1-(logLik(model2.2)/logLik(mod_nul))


model3=glm(moroso~cons_medido+TARIFA+log(valorcat),family="binomial", data=morosos_valor)
summary(model3)
mod_nul <- glm(moroso ~ 1, family="binomial",data=morosos_valor)
1-(logLik(model3)/logLik(mod_nul))

model4a=glm(moroso~cons_medido,family="binomial", data=morosos_valor)
summary(model4a)
1-(logLik(model4)/logLik(mod_nul))
exp(-1.67)
exp(-0.42)

model4=glm(moroso~cons_medido+valorcatM,family="binomial", data=morosos_valor)
summary(model4)
1-(logLik(model4)/logLik(mod_nul))
exp(-1.67)
exp(-0.42)


model5=glm(RECIBOS_VENCIDOS~TIPO_FACTURA_DIC+valorcatM,family="poisson", data=morosos_valor)
summary(model5)

model5=glm(RECIBOS_VENCIDOS~cons_medido+TARIFA+valorcat,family="poisson", data=morosos_valor)
summary(model5)
1-(logLik(model6)/logLik(mod_nul))

model6=glm(cortado~RECIBOS_VENCIDOS+TIPO_FACTURA_DIC+TARIFA,family="binomial", data=Agua21)
summary(model6)
1-(logLik(model6)/logLik(mod_nul))

Agua_med=morosos_valor%>%filter(cons_medido==1)
model7=glm(M3_DIC~TARIFA+log(valorcat),family="poisson", data=Agua_med)
summary(model7)
1-(logLik(model6)/logLik(mod_nul))

model8=lm(M3_DIC~TARIFA+log(valorcat), data=Agua_med)
summary(model8)
