#-----------------------------------------------------------------------------#
# Program: LinearModel_2Slopes_ByCause_ASDR_40-84.r                           #
# Description: Fit 2-slope linear regression models to age-stardardized death # 
#			         rates by COD (ages 40-84).																      #
# Name: Nadine Ouellette                                                      #
# Date: February 15 2024                                                      #
#-----------------------------------------------------------------------------#

rm(list=ls(all=TRUE))

# Load ASDR array prepared by Magali, using data from the RDC
setwd("~/OneDrive - Universite de Montreal/Projet_USStates_Mortality/USStates_CVRev/ROutputs")
#load(file="1941-2020/asdr_by3cod_4084.RData")
load(file="1959-2020/asdr_by5cod_4084.RData")
ls()
str(asdr.by5cod.4084)
asdr <- asdr.by5cod.4084
rm(asdr.by5cod.4084)
str(asdr)

states <- dimnames(asdr)[[1]]
nb.states <- length(states)
years.tmp <- as.numeric(dimnames(asdr)[[2]])
years <- years.tmp[-length(years.tmp)] ## without COVID-19
sex <- c("m", "f")
nb.sex <- length(sex)
cod <- dimnames(asdr)[[4]]
nb.cod <- length(cod)

Rsquare.max <- array(NA, dim=c(nb.states,nb.sex,nb.cod))
dimnames(Rsquare.max)[[1]] <- states
dimnames(Rsquare.max)[[2]] <- sex
dimnames(Rsquare.max)[[3]] <- cod
break.yr <- Rsquare.max
beta0.opt <- Rsquare.max
beta1.opt <- Rsquare.max
beta2.opt <- Rsquare.max
pvalue.break.opt <- Rsquare.max
pvalue.beta0.opt <- Rsquare.max
pvalue.beta1.opt <- Rsquare.max
BICvalue.opt <- Rsquare.max
AICvalue.opt <- Rsquare.max
for (i in states){
  j <- which(states==i)  
  for (k in 1:nb.sex){  	
    for (l in nb.cod){ ## while CoD categories are problematic for 1941-1958
      #for (l in 1:nb.cod){
      y <- asdr[j,,k,l][-length(years.tmp)]*1000
      #y <- asdr[j,,k,l]*1000 ## with COVID-19
      YRS <- min(years):max(years)
      x1 <- YRS - min(YRS)
      break.pts <- YRS[5:(length(YRS)-3)]
	    Rsquare <- numeric()
      beta0 <- numeric()
      beta1 <- numeric()
      beta2 <- numeric()
      pvalue.break <- numeric()
      pvalue.beta0 <- numeric()
      pvalue.beta1 <- numeric()
      BICvalue <- numeric()
      AICvalue <- numeric()
      for(brk in break.pts) {
        ibreak <- YRS > brk
        x2 <- (YRS - brk)*ibreak
        model.fit <- lm(y ~ x1 + x2)
        Rsquare[brk-min(break.pts)+1] <- summary(model.fit)$r.squared
        beta0[brk-min(break.pts)+1] <- model.fit$coef[1]
        beta1[brk-min(break.pts)+1] <- model.fit$coef[2]
        beta2[brk-min(break.pts)+1] <- model.fit$coef[3]
        pvalue.break[brk-min(break.pts)+1] <-
          ifelse(is.na(model.fit$coef[3]),NA,summary(model.fit)$coef[3,4])
        pvalue.beta0[brk-min(break.pts)+1] <-
          ifelse(is.na(model.fit$coef[2]),NA,summary(model.fit)$coef[2,4])
        pvalue.beta1[brk-min(break.pts)+1] <- ifelse(is.na(model.fit$coef[1]),NA,summary(model.fit)$coef[1,4])
        BICvalue[brk-min(break.pts)+1] <- BIC(model.fit)
        AICvalue[brk-min(break.pts)+1] <- AIC(model.fit)
      }
      Rsquare.max[j,k,l] <- max(Rsquare)
  	  break.yr[j,k,l] <- break.pts[which.max(Rsquare)]
      beta0.opt[j,k,l] <- beta0[which.max(Rsquare)]
      beta1.opt[j,k,l] <- beta1[which.max(Rsquare)]
      beta2.opt[j,k,l] <- beta2[which.max(Rsquare)]
      pvalue.break.opt[j,k,l] <- pvalue.break[which.max(Rsquare)]      
      pvalue.beta0.opt[j,k,l] <- pvalue.beta0[which.max(Rsquare)]      
      pvalue.beta1.opt[j,k,l] <- pvalue.beta1[which.max(Rsquare)]      
      BICvalue.opt[j,k,l] <- BICvalue[which.max(Rsquare)]      
      AICvalue.opt[j,k,l] <- AICvalue[which.max(Rsquare)]      
    }
  }
}

save(list=c("states","years","YRS","asdr","Rsquare.max","break.yr",
            "beta0.opt","beta1.opt","beta2.opt","pvalue.break.opt",
            "pvalue.beta0.opt","pvalue.beta1.opt","BICvalue.opt","AICvalue.opt"),
     file="~/OneDrive - Universite de Montreal/Projet_USStates_Mortality/USStates_CVRev/ROutputs/1941-2020/LinearModel_2Slopes_ByCause_ASDR_40-84.RData")


# P-values
alpha <- 0.01
pvalue.break.opt[ states[which(pvalue.break.opt[,1,1]>alpha)] ,1,1] #Heart dis.: AK (0.191)
pvalue.break.opt[ states[which(pvalue.break.opt[,1,2]>alpha)] ,1,2]
pvalue.break.opt[ states[which(pvalue.break.opt[,1,3]>alpha)] ,1,3]
pvalue.break.opt[ states[which(pvalue.break.opt[,1,4]>alpha)] ,1,4] #Other canc.: HI (0.054)
pvalue.break.opt[ states[which(pvalue.break.opt[,1,6]>alpha)] ,1,6] #All COD: AK (0.084)
pvalue.break.opt[ states[which(pvalue.break.opt[,1,7]>alpha)] ,1,7] #CVDs: AK (0.012)

pvalue.break.opt[ states[which(pvalue.break.opt[,2,1]>alpha)] ,2,1] #Heart dis.: KS (0.035), NE (0.156), SD (0.088), VT (0.019)
pvalue.break.opt[ states[which(pvalue.break.opt[,2,2]>alpha)] ,2,2]
pvalue.break.opt[ states[which(pvalue.break.opt[,2,3]>alpha)] ,2,3]
pvalue.break.opt[ states[which(pvalue.break.opt[,2,4]>alpha)] ,2,4] #Other canc.: AK (0.027), HI (0.025), SD (0.029), WY (0.045)
pvalue.break.opt[ states[which(pvalue.break.opt[,2,6]>alpha)] ,2,6]
pvalue.break.opt[ states[which(pvalue.break.opt[,2,7]>alpha)] ,2,7] #CVDs : OK (0.016)