setwd("C:/Users/asalazar/Desktop/Rcode")
library(readr)
library(dplyr)

hosp<- read_csv("https://raw.githubusercontent.com/asalazaradams/data/main/hospitals.csv")



library(rDEA)
rsu.input=munief_D%>% select (staff,presupparcial,pobtot2018)
rsu.output=munief_D%>% select (waste,light,poli)
rsu.env=munief_D%>% select    (indice_marg,
                               dens,
                               P60,
                               h10k,
                               tax,
                               progtot,
                               capreg,
                               compstaff,
                               ptemp,
                               pcollege,
                               logsal,
                               pagemed,
                               pageold,
                               pct_muj,
                               Align,
                               party_1_LEFT,party_1_PAN,party_1_PVEM,party_1_PRI,party_1_SNI,
                               Estado_1,Estado_2,Estado_3,Estado_4,Estado_5,Estado_6,
                               Estado_7,Estado_8,Estado_10,Estado_11,Estado_12,Estado_13,
                               Estado_14,Estado_15,Estado_16,Estado_17,Estado_18,Estado_19,
                               Estado_20,Estado_21,Estado_22,Estado_23,Estado_24,Estado_25,
                               Estado_26,Estado_27,Estado_28,Estado_29,Estado_30,Estado_31)




set.seed(123)
start_time <- Sys.time()
ooa.d.boot.c=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="variable",
                             L1=100, L2=2000, alpha=0.05)
end_time <- Sys.time()
end_time-start_time