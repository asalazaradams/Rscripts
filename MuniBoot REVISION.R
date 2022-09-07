setwd("C:/Users/asalazar/Desktop/Rcode")
library(readr)

munief  <- read_csv("https://raw.githubusercontent.com/asalazaradams/data/main/Munis19.csv")

#MODEL3

munief$logpob=log(munief$pobtot2018)
munief$logsal=log(munief$salmed)
munief$loginc=log(munief$ingpc2015)

library(fastDummies)
munief_D_0=dummy_cols(munief,select_columns = 'party')


munief_D=dummy_cols(munief,select_columns = 'Estado')
library(dplyr)
library(rDEA)
rsu.input=munief_D%>% select (staff,presupparcial)
rsu.output=munief_D%>% select (waste,light,poli)
rsu.env=munief_D%>% select    (loginc,
                               P60,
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
                               rural, semirural, urbsmall,urbmed,
                               party_LEFT,party_PAN,party_PANC,party_PRI,party_SNI,
                               Estado_1,Estado_2,Estado_3,Estado_4,Estado_5,Estado_6,
                               Estado_7,Estado_8,Estado_10,Estado_11,Estado_12,Estado_13,
                               Estado_14,Estado_15,Estado_16,Estado_17,Estado_18,Estado_19,
                               Estado_20,Estado_21,Estado_22,Estado_23,Estado_24,Estado_25,
                               Estado_26,Estado_27,Estado_28,Estado_29,Estado_30,Estado_31)
set.seed(123)
start_time <- Sys.time()
ooa.d.boot=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="variable",
                           L1=100, L2=2000, alpha=0.05)
end_time <- Sys.time()
end_time-start_time


ooa.d.boot$beta_hat
ooa.d.boot$delta_hat_hat
eff_boot=1/ooa.d.boot$delta_hat_hat
ooa.d.boot$beta_hat
ooa.d.boot$beta_hat_hat
ooa.d.boot$beta_hat_hat_star
ooa.d.boot$beta_ci
ooa.d.boot$sigma_ci

summary(eff_boot)

cor(rsu.env)


corrplot::set.seed(123)
start_time <- Sys.time()
ooa.d.boot.c=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="constant",
                           L1=100, L2=2000, alpha=0.1)
end_time <- Sys.time()
end_time-start_time

ooa.d.boot.c$beta_hat
ooa.d.boot.c$delta_hat_hat
eff_boot.c=1/ooa.d.boot.c$delta_hat_hat
ooa.d.boot.c$beta_hat
ooa.d.boot.c$beta_hat_hat
ooa.d.boot.c$beta_hat_hat_star
ooa.d.boot.c$beta_ci
ooa.d.boot.c$sigma_ci

summary(eff_boot.c)

summary(eff_boot)



#CRS MODEL
set.seed(123)
start_time <- Sys.time()
mun.crs=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="constant",
                             L1=100, L2=2000, alpha=0.1)
end_time <- Sys.time()
end_time-start_time

mun.crs$beta_hat
mun.crs$delta_hat_hat
eff_boot.crs=1/mun.crs$delta_hat_hat
mun.crs$beta_hat
mun.crs$beta_hat_hat
mun.crs$beta_hat_hat_star
mun.crs$beta_ci
mun.crs$sigma_ci

summary(eff_boot.crs)



#vRS MODEL (95%)
set.seed(123)
start_time <- Sys.time()
mun.vrs.95=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="variable",
                        L1=100, L2=2000, alpha=0.05)
end_time <- Sys.time()
end_time-start_time

mun.vrs.95$beta_hat
mun.vrs.95$delta_hat_hat
eff_boot.vrs.95=1/mun.vrs.95$delta_hat_hat
mun.vrs.95$beta_hat
mun.vrs.95$beta_hat_hat
mun.vrs.95$beta_hat_hat_star
mun.vrs.95$beta_ci
mun.vrs.95$sigma_ci

summary(eff_boot.vrs.95)

munief%>%group_by(party)%>%
        summarise(mean(pobtot2018))
munief%>%group_by(party)%>%
  summarise(mean(pobtot2018),
            mean(pct_muj))
munief%>%group_by(party)%>%
  summarise(mean(pobtot2018),
            mean(pct_muj),
            mean(staff))

munief%>%group_by(party)%>%
  summarise(mean(pobtot2018),
            median(pobtot2018),
            max(pobtot2018))

summary(munief$pobtot2018)




