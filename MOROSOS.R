library(foreign)
catastro=read.dbf("C:/Users/asalazar/Documents/HERMOSILLO2019/HERMOSILLO2019/municipio total/predio.dbf")
library(stringr)

catastro$COLONIA=str_remove(catastro$COLONIA, "FRACCIONAMIENTO ")
catastro$COLONIA=str_remove(catastro$COLONIA, "POB ")
val_colonia=catastro%>%group_by(COLONIA)%>%
  summarise(valor=mean(VALTOT, na.rm = T), 
            valorcat=mean(VALOR_CAT,na.rm = T),
            predios=n())
Morosos2021 <- read_excel("Morosos2021/Morosos2021.xlsx")
Morosos2021$COLONIA=str_remove(Morosos2021$COLONIA, " (INVASION)")

Morosos2021$cortado=0
which(Morosos2021$ESTADO_SUMINISTRO=="Cortado por impago")
Morosos2021$cortado[which(Morosos2021$ESTADO_SUMINISTRO=="Cortado por impago")]=1

#Morosos2021=na.omit(Morosos2021)

morosos_colonia=Morosos2021%>%group_by(COLONIA)%>%
  summarise(p_cortado=mean(cortado,na.rm = T), 
            adeudo=mean(ADEUDO,na.rm = T),
            recibos_vencidos=mean(RECIBOS_VENCIDOS,na.rm = T),
            morosos=n())

merge(morosos_colonia,val_colonia, by="COLONIA")
morosos_catastro=merge(morosos_colonia,val_colonia, by="COLONIA" )
morosos_valor=merge(Morosos2021,val_colonia, by="COLONIA" )
morosos_catastro$pct_morosos=morosos_catastro$morosos/morosos_catastro$predios
plot(morosos_catastro$valorcat, morosos_catastro$pct_morosos)
library(dplyr)
morosos2=morosos_catastro%>%filter(pct_morosos<1, valorcat<10000000)
plot(morosos2$valorcat, morosos2$pct_morosos)
plot(morosos2$valorcat, morosos2$p_cortado)

Morosos2021%>%group_by(TARIFA)%>%
  summarise(adeudo=mean(ADEUDO),
            adeudo_total=sum(ADEUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            rvc=mean(RECIBOS_VENCIDOS*cortado),
            n())

Morosos2021%>%group_by(TIPO_FACTURA_ENE)%>%
  summarise(adeudo_medio=mean(ADEUDO),
            adeudo_total=sum(ADEUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            rvc=mean(RECIBOS_VENCIDOS*cortado),
            n())

Morosos2021%>%filter(TARIFA=="DOMESTICO")%>%
  group_by(TIPO_FACTURA_ENE)%>%
  summarise(adeudo_medio=mean(ADEUDO),
            adeudo_total=sum(ADEUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            consumo=mean(M3_ENE),
            n())

Morosos2021%>%filter(TARIFA=="GOBIERNO")%>%
  group_by(TIPO_FACTURA_ENE)%>%
  summarise(adeudo_medio=mean(ADEUDO),
            adeudo_total=sum(ADEUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            consumo=mean(M3_ENE),
            n())

Morosos2021%>%filter(TARIFA=="COMERCIAL")%>%
  group_by(TIPO_FACTURA_ENE)%>%
  summarise(adeudo_medio=mean(ADEUDO),
            adeudo_total=sum(ADEUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            consumo=mean(M3_ENE),
            n())

Morosos2021%>%filter(TARIFA=="COMERCIAL")%>%
  group_by(TIPO_FACTURA_ENE)%>%
  summarise(adeudo_medio=mean(ADEUDO),
            adeudo_total=sum(ADEUDO),
            cortado=mean(cortado),
            recibos_v=mean(RECIBOS_VENCIDOS),
            consumo=mean(M3_ENE),
            n())

