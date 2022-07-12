
options(scipen=999)
library(readr)
set.seed(123)



####################################################
## Step 0 : the FDH model
####################################################
library(glpkAPI)
library(Rglpk)
library(boot)
library(np)
library(MASS)
library(dplyr)
library(ggplot2)



carcel <- read_csv("carcel.csv")


data_file=carcel
y= cbind(data_file$pobtot, data_file$capacit, data_file$estud)
x= cbind(data_file$staff)

n  <- length(x[,1])
i <-1
yk <- y[i,]
xk <- x[i,]
p <- length(x[1,])
q_y <- length(y[1,])
#q_fact <- ncol(fact)

f <- function(theta,x,y,i,mm)                    # define a function, depending on the efficiency score theta
{
  nsum <- 0; dsum <- 0
  for (j in (1:length(x[,1])))
  {
    
    n= (as.numeric(( all(y[j,] >=  y[i,] )) & (x[j,1] <= (x[i,1] * theta ))))       # indicator function to test whehter it is lower or higher than a specific value
    d= (as.numeric(all(y[j,] >=  y[i,])))
    
    nsum <- n+nsum                                        # you sum all these integrals
    dsum <- d+dsum
  }
  if(dsum==0)
  {
    dsum <- 1
  }
  
  return((1-(nsum/dsum))^mm)
}


eff.int <- matrix(nrow=length(x[,1]),ncol=1)              # define a matrix where you put your results
for (i in (1:length(x[,1])))
{
  print(i)
  eff <- integrate(f,0,Inf,x=x,y=y,i=i,mm=150, stop.on.error = FALSE)       # integrate from O to infinity
  eff.int[i] <- eff$value
  write.matrix(eff.int, file = "orderm.txt", sep = " ")
  eff.uncond = eff.int
}


dat=data.frame (ordered(data_file$anio),
                 data_file$contrato,
                 data_file$capinstt,
                 data_file$ocup,
                 data_file$seg,
                 data_file$sex,
                 data_file$psisent,
                 data_file$certif)


bw <- npudensbw(dat=dat)




#MODELO 1
#bw=bw
#dat=dat
f <- function(theta,x,y,i,mm)                    # define a function, depending on the efficiency score theta
{
  tdata <- dat[i,]
  kerz <- npudens(bws=bw,cykertype="epanechnikov",cxkertype="epanechnikov",oxkertype="liracine",tdat=tdata,edat=dat)
  kerz <- kerz$dens
  nsum <- 0; dsum <- 0
  for (j in (1:length(x[,1])))
  {
    n= (as.numeric(( all(y[j,] >=  y[i,] )) & (x[j,1] <= (x[i,1] * theta )))) * kerz[j]       # indicator function to test whehter it is lower or higher than a specific value
    d= (as.numeric(all(y[j,] >=  y[i,]))) * kerz[j]
    nsum <- n+nsum                                        # you sum all these integrals
    dsum <- d+dsum
  }
  if(dsum==0)
  {
    dsum <- 1
  }
  return((1-(nsum/dsum))^mm)
}


eff.int <- matrix(nrow=length(x[,1]),ncol=1)              # define a matrix where you put your results
for (i in (1:length(x[,1])))
{
  print(i)
  eff <-      integrate(f,0,Inf,x=x,y=y,i=i,mm=150,stop.on.error=FALSE)
  if(eff$value<=0.01)
  {
    eff.int[i] <- 1
  } else
  {
    eff.int[i] <- eff$value
  }
}
eff.cond=eff.int
# save the output of the second step
write.matrix(eff.int, file = "conditional_eff_carcel.txt", sep = " ")
#### STEP 2 : Explaining efficiency
effratio<-  eff.cond / eff.uncond

bw2 <- npregbw(ydat=effratio[,1], xdat = dat, bwmethod = "cv.ls", regtype="ll", ckertype="epanechnikov",oxkertype="liracine")

model<- npreg(bws=bw2, gradients=TRUE)

par("mar")
par(mar=c(2,2,1,2))
plot<-plot(model,xq=0.5, common.scale=FALSE)

signif <- npsigtest(bws=bw2, boot.num= 100)
signif
model$R2

save.image('cond_ef_carcel150.RData')
carcel$efcond=eff.cond
carcel$efuncond=eff.uncond

summary(carcel$efcond)
summary(carcel$efuncond)
carcel%>%filter(efcond>=1)%>%
  summarise(n())

carcel%>%filter(efuncond>=1)%>%
  summarise(n())

carcel$ratio=carcel$efcond/carcel$efuncond
#plot(carcel$staff,carcel$ratio)
plot(carcel$capinstt,carcel$ratio)
#boxplot(carcel$ratio~carcel$seg)
