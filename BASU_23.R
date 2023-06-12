
library(readr)
basu <- read_csv("https://raw.githubusercontent.com/asalazaradams/data/main/basu_BASE.csv")
supmuni=read_csv("https://raw.githubusercontent.com/asalazaradams/data/main/supmuni.csv")
basu=na.omit(basu)
basu$hotel100=basu$hoteles/basu$POBTOT*100000

basu$old=1-(basu$a_10+basu$a_20)
basu$p_base=basu$base/basu$staff_t
basu$big=(basu$compact+basu$truck)/basu$tot_veh


basu$priv=0
basu$priv[which(basu$reg=="priv")]=1
basu$mixed=0
basu$mixed[which(basu$reg=="mixed")]=1

table(basu$reg)

#table(basu$mixed)

library (dplyr)

supmuni$folio=as.numeric(as.character(supmuni$CVEGEO))

sup=supmuni %>% select(folio,AREA)
basu=merge(basu,sup,by="folio")
basu$dens=basu$POBTOT/basu$AREA
basu$logpob=log(basu$POBTOT)
basu$loginc=log(basu$ingpc2015)
basu$pcompact=basu$compact/basu$tot_veh
basu$ptruck=basu$truck/basu$tot_veh
basu$ppickup=basu$pickup/basu$tot_veh

aps=as.data.frame(table(basu$folio))
aps$folio=aps$Var1
aps=aps%>%select(folio, Freq)
basu=merge(basu,aps, by="folio")

basu2=basu%>%filter(Freq==2)
mean(basu2$mixed)

basu2%>%group_by(anyo,priv)%>%
  summarise(n())


basu2%>%group_by(anyo,mixed)%>%
  summarise(n())


##################################



library(rDEA)

rsu.input=basu%>% select (staff_t,tot_veh)
rsu.output=basu%>% select (can_ton)

rsu.env=basu%>% select(logpob,P60,dens,
                       loginc,hotel100,
                       priv,mixed,p_base,
                       pcompact,a_10)


set.seed(123)
start_time <- Sys.time()
boot=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="variable",
                             L1=100, L2=1000, alpha=0.05)
end_time <- Sys.time()
end_time-start_time

summary(boot$delta_hat_hat)
summary(1/boot$delta_hat_hat)



boot$beta_ci

boot$beta_hat_hat
  
  
  save.image(file="basu_BOOT.RData")







start_time <- Sys.time()
boot5=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="variable",
                     L1=100, L2=1000, alpha=0.05)
end_time <- Sys.time()
end_time-start_time




boot5$beta_ci

boot5$beta_hat_hat
  
  
  save.image(file="basu_BOOT.RData")


  1/78

set.seed(123)
start_time <- Sys.time()
boot=dea.env.robust (X=rsu.input, Y=rsu.output,  Z=rsu.env, model="input", RTS="variable",
                     L1=100, L2=1000, alpha=0.1)
end_time <- Sys.time()
end_time-start_time

summary(boot$delta_hat_hat)
summary(1/boot$delta_hat_hat)


boot$beta_ci

boot$
  
  
  save.image(file="basu_BOOT.RData")