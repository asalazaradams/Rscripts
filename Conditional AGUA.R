library(readr)
   .trPaths <- paste(paste(Sys.getenv('APPDATA'), '\\Tinn-R\\tmp\\',
sep=''), c('', 'search.txt', 'objects.txt', 'file.r', 'selection.r',
'block.r', 'lines.r'), sep='')

Agua_Ef_16 <- read_csv("Agua_Ef_16.csv")
agua=Agua_Ef_16
write.csv(ODEAZERO,file="Agua_Ef_16.csv")

library(dplyr)

          
####################################################
## Step 0 : the FDH model
####################################################
  library(glpkAPI)
  library(Rglpk)
  library(boot)
  library(np)
  library(MASS)


#DATA
#setwd("//econprofiles/profiles1/ndaaf84/Documents/Isabelle")

#data_file=read.csv("data_test.csv",sep=";")

   data_file=agua
   data_file$logpob=log(data_file$pobtot2016.x)
   data_file$loginc=log(data_file$ingpc2015)
#data_file=data_file[1:50,]
#data_file=data.frame(data_file)


y= cbind(data_file$tot_vol , data_file$tot_tomas )
x= cbind(data_file$staff,data_file$gasto_elect_total)

#ord= cbind(data_file$YEAR, data_file$Mediumsize, data_file$Largesize) #ordered variables
#fact= cbind(data_file$update)    #dummy

#= cbind(data_file$logpob,
 #        data_file$progtot,
  #       data_file$pcollege,
   #      data_file$logsal,
    #     data_file$pcsolev)

#continuous variables


   n  <- length(x[,1])
   i <-1
   yk <- y[i,]
   xk <- x[i,]
   p <- length(x[1,])
   q_y <- length(y[1,])
   
   #q_ord <- ncol(ord)
   q_fact <- ncol(fact)



f <- function(theta,x,y,i,mm)  # define a function, depending on the efficiency score theta
{
  nsum <- 0; dsum <- 0
  for (j in (1:length(x[,1])))
  {
     #EQS 3 y 4
     n= (as.numeric(( all(y[j,] >=  y[i,] )) & (x[j,1] <= (x[i,1] * theta )))) # indicator function to test whehter it is lower or higher than a specific value
     d= (as.numeric(all(y[j,] >=  y[i,])))

    nsum <- n+nsum          # you sum all these integrals
    dsum <- d+dsum
  }
  if(dsum==0)
  {
    dsum <- 1
  }

return((1-(nsum/dsum))^mm) #EQ 5
}



#EQ 6

  eff.int <- matrix(nrow=length(x[,1]),ncol=1)  # define a matrix where you put your results
  for (i in (1:length(x[,1])))
  {
    print(i)
    eff <- integrate(f,0,Inf,x=x,y=y,i=i,mm=100, stop.on.error = FALSE) # integrate from O to infinity
    eff.int[i] <- eff$value
    write.matrix(eff.int, file = "orderm.txt", sep = " ")
    eff.uncond = eff.int
  }

  


  ## STEP 2: Estimate the conditional efficiency estimates by the integral


# data_ord <- matrix(nrow=length(ord[,1]))
 # for (z in (1:length(ord[1,])))
#  {
 #   data_or <- data.frame(ordered(ord[,z]))
 #   data_ord <- data.frame(data_ord, data_or)
#  }
 
 ##########################################################3

  data_fact <- matrix(nrow=length(fact),ncol=length(fact))
  data_fact <- matrix(nrow=length(fact))
  for (z in (1:length(fact)))
  {
    data_or <- data.frame(ordered(fact))
    data_fact <- data.frame(data_fact, data_or)
  }


 # dat <- data.frame(data_ord[,2:q_ord+1], data_fact[,2:q_fact+1], Q)
  dat <- data.frame( data_fact[,2:q_fact+1], Q)

#dat=Q
# either compute an overall bw estimator, and use this bw
           bw <- npudensbw(dat=dat, bws=c(0.5, 
                                          0.5,
                                          0.5,
                                          0.5,
                                          0.5,
                                          0.5,0.5), bandwidth.compute=FALSE)
           summary(bw)
# or use the specific bw from previous step

####FUNCTION
 #EQ 10
  f <- function(theta,x,y,i,mm)  # define a function, depending on the efficiency score theta
  {

    tdata <- dat[i,]
    ####### USE   bw_cx[i,]

  #  kerz <- npudens(bws=bw_cx[i,],cykertype="epanechnikov",cxkertype="epanechnikov",oxkertype="liracine",tdat=tdata,edat=dat)
    kerz <- npudens(bws=bw,cykertype="epanechnikov",cxkertype="epanechnikov",oxkertype="liracine",tdat=tdata,edat=dat)


    kerz <- kerz$dens
    nsum <- 0; dsum <- 0
    for (j in (1:length(x[,1])))
    {

     n= (as.numeric(( all(y[j,] >=  y[i,] )) & (x[j,1] <= (x[i,1] * theta )))) * kerz[j]       # indicator function to test whehter it is lower or higher than a specific value
     d= (as.numeric(all(y[j,] >=  y[i,]))) * kerz[j]


# output       n <- (as.numeric( all(x[j,] <= x[i,] )) & (y[j,1] >=  (y[i,1] * theta )) & (y[j,2] >=  (y[i,2] * theta ))) * kerz[j]       # indicator function to test whehter it is lower or higher than a specific value
# output    d <- (as.numeric(all(x[j,] <=  x[i,])))  * kerz[j]

           nsum <- n+nsum                                        # you sum all these integrals
      dsum <- d+dsum
    }
    if(dsum==0)
    {
      dsum <- 1
    }
    return((1-(nsum/dsum))^mm)
  }
####FUNCTION

#EQ11
  eff.int <- matrix(nrow=length(x[,1]),ncol=1)              # define a matrix where you put your results

  
   for (i in (1:length(x[,1]))){
    print(i)
    eff <-      integrate(f,0,Inf,x=x,y=y,i=i,mm=100,stop.on.error=FALSE)
    if(eff$value<=0.01)
      {
      eff.int[i] <- 1
    } else
    {
      eff.int[i] <- eff$value
    }
  }

  

  
 eff.cond= eff.int

  # save the output of the second step
  write.matrix(eff.int, file = "conditional_eff.txt", sep = " ")







###############
#### STEP 2 : Explaining efficiency
##############


  effratio <-  eff.cond / eff.uncond

#   data_ord <- matrix(nrow=length(ord[,1]))
#           for (z in (1:length(ord[1,])))
#            {
#            data_or <- data.frame(ordered(ord[,z]))
#            data_ord <- data.frame(data_ord, data_or)
#            }
   
          data_fact <- matrix(nrow=length(fact[,1]),ncol=length(fact[1,]))
          data_fact <- matrix(nrow=length(fact[,1]))
            for (z in (1:length(fact[1,])))
            {
            data_or <- data.frame(ordered(fact[,z]))
            data_fact <- data.frame(data_fact, data_or)
            }
          dat <- data.frame( data_fact[,2:(length(fact[1,])+1)], Q)


 bw2 <- npregbw(ydat=effratio[,1], xdat = dat, bwmethod = "cv.ls", regtype="ll", ckertype="epanechnikov",oxkertype="liracine")


  model2 <- npreg(bws=bw2, gradients=TRUE)

# plot21<-npplot(bws=bw2,xq=0.25, common.scale=FALSE)
  plot22<-npplot(bws=bw2,xq=0.5, common.scale=FALSE)

  
  
  #fact= cbind(data_file$ppales.si,
  #            data_file$muni)    #dummy
  
  #Q= cbind(data_file$logpob,
           data_file$loginc,
           data_file$pctprop,
           data_file$compstaff,
           data_file$logsal,
           data_file$pcsolev,

 # X1:pctprop
  #x2:logsal,
  #x3:compstaff,
  #x4: pcsolev
  #x5: logpob
  #x6: loginc)
  
  signif <- npsigtest(bws=bw2, boot.num= 100)
  betas2<-model2$grad
  se2<-model2$gerr
  summary(betas2)
  colMeans(betas2)
  summary(se2)
  signif
  model2$R2
  par("mar")


  summary(eff.uncond)
  summary(eff.int)

  par(mar=c(1,1,1,1))
  variables <- cbind(x, y, ord, fact, Q, eff.uncond, eff.int, bw_cx)
  write.matrix(variables, file = "input - output - control variables.txt", sep = " ")

