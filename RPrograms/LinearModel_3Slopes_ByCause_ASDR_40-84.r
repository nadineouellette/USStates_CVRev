#-----------------------------------------------------------------------------#
# Program: LinearModel_3Slopes_ByCause_ASDR_40-84.r                           #
# Description: Fit 3-slope linear regression models to age-stardardized death # 
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
break1.yr <- Rsquare.max
break2.yr <- Rsquare.max
beta0.opt <- Rsquare.max
beta1.opt <- Rsquare.max
beta2.opt <- Rsquare.max
beta3.opt <- Rsquare.max
pvalue.break1.opt <- Rsquare.max
pvalue.break2.opt <- Rsquare.max
pvalue.beta0.opt <- Rsquare.max
pvalue.beta1.opt <- Rsquare.max
BICvalue.3sl.opt <- Rsquare.max
AICvalue.3sl.opt <- Rsquare.max
for (i in states){
  print(i)
  j <- which(states==i)  
  for (k in 1:nb.sex){  	
    for (l in 1:nb.cod){ 
    #for (l in nb.cod){ ## while CoD categories are problematic for 1941-1958
      y <- asdr[j,,k,l][-length(years.tmp)]*1000
      #y <- asdr[j,,k,l]*1000 ## with COVID-19
      YRS <- min(years):max(years)
      x1 <- YRS - min(YRS)
      break1.pts <- YRS[5:(length(YRS)-3)]
	    break2.pts <- numeric()	  
	    Rsquare <- matrix(NA, nrow=(length(break1.pts)-3),ncol=(length(break1.pts)-3))
	    rownames(Rsquare) <- break1.pts[1:(length(break1.pts)-3)]
	    colnames(Rsquare) <- break1.pts[4:length(break1.pts)] #6?
      beta0 <- Rsquare
      beta1 <- Rsquare
      beta2 <- Rsquare
      beta3 <- Rsquare
      pvalue.break1 <- Rsquare
      pvalue.break2 <- Rsquare
      pvalue.beta0 <- Rsquare
	    pvalue.beta1 <- Rsquare
      BICvalue.3sl <- Rsquare
      AICvalue.3sl <- Rsquare
      for(brk1 in break1.pts) {
        ibreak1 <- YRS > brk1
        x2 <- (YRS - brk1)*ibreak1
        break2.pts <- break1.pts[which(break1.pts >= (brk1 + 3))]
        for(brk2 in break2.pts){
          ibreak2 <- YRS > brk2
          x3 <- (YRS - brk2)*ibreak2
          model.fit <- lm(y ~ x1 + x2 + x3)
          Rsquare[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <- 
            summary(model.fit)$r.squared
          beta0[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <-
            model.fit$coef[1]
          beta1[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <-
            model.fit$coef[2]
          beta2[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <-
            model.fit$coef[3]
          beta3[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <-
            model.fit$coef[4]
          pvalue.break1[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <-
            ifelse(is.na(model.fit$coef[3]),NA,summary(model.fit)$coef[3,4])
          pvalue.break2[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <-
            ifelse(is.na(model.fit$coef[3]),NA,summary(model.fit)$coef[4,4])
          pvalue.beta0[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <-
            ifelse(is.na(model.fit$coef[1]),NA,summary(model.fit)$coef[1,4])
          pvalue.beta1[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <-
            ifelse(is.na(model.fit$coef[2]),NA,summary(model.fit)$coef[2,4])
          BICvalue.3sl[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <-
            BIC(model.fit)
          AICvalue.3sl[brk1-min(break1.pts)+1,which(break1.pts==brk2)-3] <-
            AIC(model.fit)
        }
      }
      Rsquare.max[j,k,l] <- max(Rsquare,na.rm=T)
  	  break1.yr[j,k,l] <- break1.pts[which(Rsquare==max(Rsquare,na.rm=T),
  	                                       arr.ind=TRUE)[1]]
  	  break2.yr[j,k,l] <- break1.pts[which(Rsquare==max(Rsquare,na.rm=T),
  	                                       arr.ind=TRUE)[2]+3]
      beta0.opt[j,k,l] <- beta0[which(Rsquare==max(Rsquare,na.rm=T),
                                      arr.ind=TRUE)]
      beta1.opt[j,k,l] <- beta1[which(Rsquare==max(Rsquare,na.rm=T),
                                      arr.ind=TRUE)]
      beta2.opt[j,k,l] <- beta2[which(Rsquare==max(Rsquare,na.rm=T),
                                      arr.ind=TRUE)]
      beta3.opt[j,k,l] <- beta3[which(Rsquare==max(Rsquare,na.rm=T),
                                      arr.ind=TRUE)]
      pvalue.break1.opt[j,k,l] <- pvalue.break1[which(Rsquare==max(Rsquare,na.rm=T),
                                                      arr.ind=TRUE)]      
      pvalue.break2.opt[j,k,l] <- pvalue.break2[which(Rsquare==max(Rsquare,na.rm=T),
                                                      arr.ind=TRUE)]      
      pvalue.beta0.opt[j,k,l] <- pvalue.beta0[which(Rsquare==max(Rsquare,na.rm=T),
                                                    arr.ind=TRUE)]      
      pvalue.beta1.opt[j,k,l] <- pvalue.beta1[which(Rsquare==max(Rsquare,na.rm=T),
                                                    arr.ind=TRUE)]      
      BICvalue.3sl.opt[j,k,l] <- BICvalue.3sl[which(Rsquare==max(Rsquare,na.rm=T),
                                                    arr.ind=TRUE)]
      AICvalue.3sl.opt[j,k,l] <- AICvalue.3sl[which(Rsquare==max(Rsquare,na.rm=T),
                                                    arr.ind=TRUE)]
    }
  }
}

save(list=c("states","years","YRS","asdr","Rsquare.max","break1.yr","break2.yr",
            "beta0.opt","beta1.opt","beta2.opt","beta3.opt","pvalue.break1.opt",
            "pvalue.break2.opt","pvalue.beta0.opt","pvalue.beta1.opt",
            "BICvalue.3sl.opt","AICvalue.3sl.opt"),
     file="~/OneDrive - Universite de Montreal/Projet_USStates_Mortality/USStates_CVRev/ROutputs/1941-2020/LinearModel_3Slopes_ByCause_ASDR_40-84.RData")


# P-values
alpha <- 0.01
pvalue.break1.opt[ states[which(pvalue.break1.opt[,1,1]>alpha)] ,1,1]
#Heart dis.: AK (0.218), SD (0.016)

pvalue.break1.opt[ states[which(pvalue.break1.opt[,1,2]>alpha)] ,1,2]

pvalue.break1.opt[ states[which(pvalue.break1.opt[,1,3]>alpha)] ,1,3]
#Smok. canc.: AK (0.033), HI (0.108)

pvalue.break1.opt[ states[which(pvalue.break1.opt[,1,4]>alpha)] ,1,4]
#Other canc.: AL (0.021), AK (0.018), AR (0.211), CT (0.024), DE (0.034), ID (0.144),
# IL (0.012), KY (0.020), LA (0.024), ME (0.083), MA (0.061), NE (0.263),
# NV (0.083), NH (0.122), NM (0.213), OH (0.159), TN (0.099), VT (0.068),
# WY (0.017)

pvalue.break1.opt[ states[which(pvalue.break1.opt[,1,6]>alpha)] ,1,6]

pvalue.break1.opt[ states[which(pvalue.break1.opt[,1,7]>alpha)] ,1,7]
#CVDs: AK (0.030)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,1,1]>alpha)] ,1,1]
#Heart dis.: AK (0.153), AR (0.057), FL (0.019), NV (0.077)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,1,2]>alpha)] ,1,2]
#Other CVDs: AK (0.097)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,1,3]>alpha)] ,1,3]
#Smok. canc.: AK (0.051)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,1,4]>alpha)] ,1,4]
#Other canc.: AK (0.097), IN (0.053), MO (0.133), NE (0.339), NH (0.043),
# PA (0.082), RI (0.449), SC (0.045), VA (0.033), WY (0.234)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,1,6]>alpha)] ,1,6]

pvalue.break2.opt[ states[which(pvalue.break2.opt[,1,7]>alpha)] ,1,7]

pvalue.break1.opt[ states[which(pvalue.break1.opt[,2,1]>alpha)] ,2,1]
#Heart dis.: MT (0.108), ND (0.037), SD (0.029), UT (0.023)

pvalue.break1.opt[ states[which(pvalue.break1.opt[,2,2]>alpha)] ,2,2]
#Other CVDs: AK (0.011), ND (0.010)

pvalue.break1.opt[ states[which(pvalue.break1.opt[,2,3]>alpha)] ,2,3]
#Smok. canc.: DC (0.023), HI (0.051), UT (0.015)

pvalue.break1.opt[ states[which(pvalue.break1.opt[,2,4]>alpha)] ,2,4]
#Other canc.: CA (0.037), DE (0.051), DC (0.016), FL (0.092), ID (0.027),
# IA (0.265), KS (0.022), KY (0.077), ME (0.016), MA (0.027),
# NE (0.027), NM (0.020), RI (0.129), SD (0.016), UT (0.019),
# WA (0.105), WI (0.058)

pvalue.break1.opt[ states[which(pvalue.break1.opt[,2,6]>alpha)] ,2,6]
#All COD: AK (0.020)

pvalue.break1.opt[ states[which(pvalue.break1.opt[,2,7]>alpha)] ,2,7]
#CVDs: AK (0.022), UT (0.112)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,2,1]>alpha)] ,2,1]
#Heart dis.: SD (0.021)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,2,2]>alpha)] ,2,2]
#Other CVDs: HI (0.123)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,2,3]>alpha)] ,2,3]
#Smok. canc.: NM (0.019)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,2,4]>alpha)] ,2,4]
#Other canc.: HI (0.010), NH (0.015), NY (0.039), SD (0.061)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,2,6]>alpha)] ,2,6]
#All COD: MT (0.019)

pvalue.break2.opt[ states[which(pvalue.break2.opt[,2,7]>alpha)] ,2,7]
#CVDs: MT (0.010), ND (0.090)