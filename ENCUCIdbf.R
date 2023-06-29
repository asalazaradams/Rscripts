
library(readr)
library(foreign)
#RECUROS (INPUTS)
encuci=read.dbf("~/Documents/ESTADISTICAS MEXICO/ENCUCI2020_csv/conjunto_de_datos_ENCUCI_2020_SEC_4_5/conjunto_de_datos/conjunto_de_datos_ENCUCI_2020_SEC_4_5.dbf")
encuci=read.dbf("~/Documents/ESTADISTICAS MEXICO/ENCUCI2020_csv/BD_ENCUCI2020_dbf/ENCUCI_2020_SEC_4_5.dbf")

class(encuci$AP5_1_2)
encuci$AP5_1_2=as.numeric(as.character((encuci$AP5_1_2)))

library(dplyr)
confianza=encuci%>%select(NOM_ENT,AP5_1_2,FAC_SEL)%>%
                    filter(AP5_1_2!=99)
confianza_edo=confianza%>%group_by(NOM_ENT)%>%
              summarise(conf_pond=sum(AP5_1_2*FAC_SEL),
                        pob=sum(FAC_SEL))
confianza_edo$calif=confianza_edo$conf_pond/confianza_edo$pob



encuci$AP5_2_1
encuci$AP5_2_1=as.numeric(encuci$AP5_2_1)

library(dplyr)
confianza_emp_gen=encuci%>%select(NOM_ENT,AP5_2_1,FAC_SEL)%>%
  filter(AP5_2_1!=9,AP5_2_1!=5,)
confianza_emp=confianza_emp_gen%>%group_by(NOM_ENT)%>%
  summarise(conf_pond=sum(AP5_2_1*FAC_SEL),
            pob=sum(FAC_SEL))
confianza_emp$calif=confianza_emp$conf_pond/confianza_emp$pob


library(dplyr)

encuci$AP5_2_5
encuci$AP5_2_5=as.numeric(encuci$AP5_2_5)

confianza_part_gen=encuci%>%select(NOM_ENT,AP5_2_5,FAC_SEL)%>%
  filter(AP5_2_5!=9,AP5_2_5!=5,)
confianza_part=confianza_part_gen%>%group_by(NOM_ENT)%>%
  summarise(conf_pond=sum(AP5_2_5*FAC_SEL),
            pob=sum(FAC_SEL))
confianza_part$calif=confianza_part$conf_pond/confianza_part$pob

encuci$AP4_1
encuci$AP4_1=gsub('\r','',encuci$AP4_1)
encuci$AP4_1=as.numeric(encuci$AP4_1)

org=encuci%>%select(NOM_ENT,AP4_1,FAC_SEL)%>%
  filter(AP4_1!=9,AP4_1!=5,)
org=org%>%group_by(NOM_ENT)%>%
  summarise(orgullo=sum(AP4_1*FAC_SEL),
            pob=sum(FAC_SEL))
org$calif=org$orgullo/org$pob

hist(confianza_edo$calif)
summary(confianza_edo$calif)


encuci$AP5_2_2
encuci$AP5_2_2=gsub('\r','',encuci$AP5_2_2)
encuci$AP5_2_2=as.numeric(encuci$AP5_2_2)

confianza_rel_gen=encuci%>%select(NOM_ENT,AP5_2_2,FAC_SEL)%>%
  filter(AP5_2_2!=9,AP5_2_2!=5,)
confianza_rel=confianza_rel_gen%>%group_by(NOM_ENT)%>%
  summarise(conf_pond=sum(AP5_2_2*FAC_SEL),
            pob=sum(FAC_SEL))
confianza_rel$calif=confianza_rel$conf_pond/confianza_rel$pob


encuci$AP5_9_1
encuci$AP5_9_1=gsub('\r','',encuci$AP5_9_1)
encuci$AP5_9_1=as.numeric(encuci$AP5_9_1)

encuci_678=read.dbf("~/Documents/ESTADISTICAS MEXICO/ENCUCI2020_csv/BD_ENCUCI2020_dbf/ENCUCI_2020_SEC_6_7_8.dbf")

encuci_678$AP6_7
head(encuci_678$AP6_7,10)

respons=encuci_678%>%select(NOM_ENT,AP6_7,FAC_SEL)%>%
  filter(AP6_7!=9,AP6_7!=4,)
respons3=respons%>%filter(AP6_7==2)%>%
  group_by(NOM_ENT)%>%
  summarise(pob=sum(FAC_SEL))
respons4=respons%>%
  group_by(NOM_ENT)%>%
    summarise(pob=sum(FAC_SEL))
  respons4$pctind=respons3$pob/respons4$pob
respons5=respons%>%filter(AP6_7==1)%>%
    group_by(NOM_ENT)%>%
    summarise(pob=sum(FAC_SEL))
respons4$pctgob=respons5$pob/respons4$pob
respons4$pctboth=1-respons4$pctgob-respons4$pctind
encuci_9=read.dbf("~/Documents/ESTADISTICAS MEXICO/ENCUCI2020_csv/BD_ENCUCI2020_dbf/ENCUCI_2020_SEC_9_10.dbf")
table(encuci_9$AP10_12)
table(encuci_9$AP10_13)
table(encuci_9$AP10_15)
table(encuci_9$AP10_16)
library(survey)
