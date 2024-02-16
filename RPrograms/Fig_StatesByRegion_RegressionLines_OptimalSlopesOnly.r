#-----------------------------------------------------------------------------#
# Program: Fig_StatesByRegion_RegressionLines_OptimalSlopesOnly.r							#
# Description: Plotting results for U.S. states by region with regression     #
#              lines. Selection between the two- vs. three-slope model is     #
#			         based on the BIC and p-values of the estimated regression      #
#              parameters.								                                    #
# Name: Nadine Ouellette                                                      #
# Date: February 15, 2024                                                     #
#-----------------------------------------------------------------------------#

par(mfrow=c(3,3), mar=c(2.7,2.7,0.5,0.5), mgp=c(1.75,0.5,0), tck=-0.015,
    cex.lab=1, cex.axis=1)

delta <- 0.01

for(fig in 1:length(us.regions.lab)) {
	id <- state1.reg[fig]
	y <- asdr[all.reg[id],][-length(x)]*1000
	## With COVID-19
	#y <- asdr[all.reg[id],]*1000
	
	id.new <- which(states==all.reg[id])	
	if( (BICvalue.opt.1[id.new,sex.id,cause] < BICvalue.opt.2[id.new,sex.id,cause]) &
	    (pvalue.break.opt[id.new,sex.id,cause] <= delta) ){
		y.hat <- beta0.opt.1[id.new,sex.id,cause] +
		         beta1.opt.1[id.new,sex.id,cause]*(YRS - min(YRS)) +
				     beta2.opt.1[id.new,sex.id,cause]*
		         ((YRS - break.yr[id.new,sex.id,cause])*
		          (YRS > break.yr[id.new,sex.id,cause]))
		#abline.brk1 <- break.yr[id.new,sex.id,cause]
		#abline.brk2 <- NA
		YRS.new <- YRS
	}else if( (pvalue.break.opt[id.new,sex.id,cause] <= delta) &
			      (pvalue.break1.opt[id.new,sex.id,cause] > delta |
			       pvalue.break2.opt[id.new,sex.id,cause] > delta) ){
		y.hat <- beta0.opt.1[id.new,sex.id,cause] +
		         beta1.opt.1[id.new,sex.id,cause]*(YRS - min(YRS)) +
				     beta2.opt.1[id.new,sex.id,cause]*
		         ((YRS - break.yr[id.new,sex.id,cause])*
		          (YRS > break.yr[id.new,sex.id,cause]))		
		#abline.brk1 <- break.yr[id.new,sex.id,cause]
		#abline.brk2 <- NA
		YRS.new <- YRS	
	}else if( (pvalue.break1.opt[id.new,sex.id,cause]<=delta &
	           pvalue.break2.opt[id.new,sex.id,cause]<=delta) ){
		y.hat <- beta0.opt.2[id.new,sex.id,cause] +
		         beta1.opt.2[id.new,sex.id,cause]*(YRS - min(YRS)) +
				     beta2.opt.2[id.new,sex.id,cause]*
		         ((YRS - break1.yr[id.new,sex.id,cause])*
		          (YRS > break1.yr[id.new,sex.id,cause])) +
				     beta3.opt.2[id.new,sex.id,cause]*
		         ((YRS - break2.yr[id.new,sex.id,cause])*
		          (YRS > break2.yr[id.new,sex.id,cause]))		
		#abline.brk1 <- break1.yr[id.new,sex.id,cause]
		#abline.brk2 <- break2.yr[id.new,sex.id,cause]
		YRS.new <- YRS		
	}else{
		y.hat <- NA
		#abline.brk1 <- NA		
		#abline.brk2 <- NA
		YRS.new <- NA		
	}
			 
	if(fig==1){ par(mar=c(1,2.7,1.5,0.5)) }
	if(fig==2 | fig==3){ par(mar=c(1,1,1.5,0.5)) }
	if(fig==4){ par(mar=c(1,2.7,1.5,0.5)) }
	if(fig==5 | fig==6){ par(mar=c(1,1,1.5,0.5)) }
	if(fig==7){ par(mar=c(2.7,2.7,1.5,0.5)) }
	if(fig==8 | fig==9){ par(mar=c(2.7,1,1.5,0.5)) }

	if(fig==1 | fig==4){
	  plot(x[-length(x)], y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax),
	       xlab=NA, ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n",
	       yaxt="n", col=col.reg[id], font.lab=2)
	  ## With COVID-19
	  #plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
	  #ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n", yaxt="n", 
	  #col=col.reg[id], font.lab=2)
	  lines(YRS.new, y.hat, col=col.reg[id], lwd=1.25)
	  #abline(v=abline.brk1, col=col.reg[id], lty=2, lwd=1.5)
	  #abline(v=abline.brk2, col=col.reg[id], lty=2, lwd=1.5)
	  axis(1, at=seq(xtckmin,xtckmax,by=10), labels=F)
	  axis(side=2, at=seq(ymin,ymax,by.y), las=1)
	  axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
	  #print(all.reg.lab[id])
	}
	if(fig==8 | fig==9){
	  plot(x[-length(x)], y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax),
	       xlab=NA, ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
	  ## With COVID-19
	  # plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
	  #ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
	  lines(YRS.new, y.hat, col=col.reg[id], lwd=1.25)
	  #abline(v=abline.brk1, col=col.reg[id], lty=2, lwd=1.5)
	  #abline(v=abline.brk2, col=col.reg[id], lty=2, lwd=1.5)
	  axis(1, at=seq(xtckmin,xtckmax,by=10))
	  axis(side=2, at=seq(ymin,ymax,by.y), las=1, labels=F)
	  axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
	  mtext("Year", side=1, line=1.5, font=2, cex=0.65)
	  #print(all.reg.lab[id])
	}
	if(fig==7){
	  plot(x[-length(x)], y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax),
	       xlab=NA, ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n",
	       yaxt="n", col=col.reg[id], font.lab=2)
	  ## With COVID-19
	  #plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
	  #ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n", yaxt="n",
	  #col=col.reg[id], font.lab=2)
	  lines(YRS.new, y.hat, col=col.reg[id], lwd=1.25)
	  #abline(v=abline.brk1, col=col.reg[id], lty=2, lwd=1.5)
	  #abline(v=abline.brk2, col=col.reg[id], lty=2, lwd=1.5)
	  axis(1, at=seq(xtckmin,xtckmax,by=10))
	  axis(side=2, at=seq(ymin,ymax,by.y), las=1)
	  axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
	  mtext("Year", side=1, line=1.5, font=2, cex=0.65)
	  #print(all.reg.lab[id])
	}
	if(fig==2 | fig==3 | fig==5 | fig==6){
	  plot(x[-length(x)], y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax),
	       xlab=NA, ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
	  ## With COVID-19
	  #plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
	  #ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
	  lines(YRS.new, y.hat, col=col.reg[id], lwd=1.25)
	  #abline(v=abline.brk1, col=col.reg[id], lty=2, lwd=1.5)
	  #abline(v=abline.brk2, col=col.reg[id], lty=2, lwd=1.5)
	  axis(1, at=seq(xtckmin,xtckmax,by=10), labels=F)
	  axis(side=2, at=seq(ymin,ymax,by.y), las=1, labels=F)
	  axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
	  #print(all.reg.lab[id])
	}

	for(c in 1:(ligne[fig])) {
		y <- asdr[all.reg[id+c],][-length(x)]*1000
		## With COVID-19
		# y <- asdr[all.reg[id+c],]*1000

		# Compute y.hat
		id.new <- which(states==all.reg[id+c])	
		if( (BICvalue.opt.1[id.new,sex.id,cause] < BICvalue.opt.2[id.new,sex.id,cause]) &
		    (pvalue.break.opt[id.new,sex.id,cause]<=delta) ){
			y.hat <- beta0.opt.1[id.new,sex.id,cause] +
			         beta1.opt.1[id.new,sex.id,cause]*(YRS - min(YRS)) +
					     beta2.opt.1[id.new,sex.id,cause]*
			         ((YRS - break.yr[id.new,sex.id,cause])*
			          (YRS > break.yr[id.new,sex.id,cause]))
			#abline.brk1 <- break.yr[id.new,sex.id,cause]
			#abline.brk2 <- NA
			YRS.new <- YRS		
		}else if( (pvalue.break.opt[id.new,sex.id,cause]<=delta) &
				      (pvalue.break1.opt[id.new,sex.id,cause]>delta |
				       pvalue.break2.opt[id.new,sex.id,cause]>delta) ){
			y.hat <- beta0.opt.1[id.new,sex.id,cause] +
			         beta1.opt.1[id.new,sex.id,cause]*(YRS - min(YRS)) +
					     beta2.opt.1[id.new,sex.id,cause]*
			         ((YRS - break.yr[id.new,sex.id,cause])*
			          (YRS > break.yr[id.new,sex.id,cause]))		
			#abline.brk1 <- break.yr[id.new,sex.id,cause]
			#abline.brk2 <- NA
			YRS.new <- YRS		
		}else if( (pvalue.break1.opt[id.new,sex.id,cause]<=delta &
		           pvalue.break2.opt[id.new,sex.id,cause]<=delta) ){
			y.hat <- beta0.opt.2[id.new,sex.id,cause] +
			         beta1.opt.2[id.new,sex.id,cause]*(YRS - min(YRS)) +
					     beta2.opt.2[id.new,sex.id,cause]*
			         ((YRS - break1.yr[id.new,sex.id,cause])*
			          (YRS > break1.yr[id.new,sex.id,cause])) +
					     beta3.opt.2[id.new,sex.id,cause]*
			         ((YRS - break2.yr[id.new,sex.id,cause])*
			          (YRS > break2.yr[id.new,sex.id,cause]))		
			#abline.brk1 <- break1.yr[id.new,sex.id,cause]
			#abline.brk2 <- break2.yr[id.new,sex.id,cause]
			YRS.new <- YRS		
		}else{
			y.hat <- NA
			#abline.brk1 <- NA		
			#abline.brk2 <- NA
			YRS.new <- NA		
		}

		#print(all.reg.lab[id+c])
		lines(x[-length(x)], y, lwd=2, col=col.reg[id+c])
		## With COVID-19
		#lines(x, y, lwd=2, col=col.reg[id+c])  # with COVID-19
		lines(YRS.new, y.hat, col=col.reg[id+c], lwd=1.25) ## YRS.new and COVID-19 ???
		#abline(v=abline.brk1, col=col.reg[id+c], lty=2, lwd=1.5)
		#abline(v=abline.brk2, col=col.reg[id+c], lty=2, lwd=1.5)
	}

	state.names <- all.reg.lab[state1.reg[fig]:(state1.reg[(fig+1)]-1)]
	col.names <- col.reg[state1.reg[fig]:(state1.reg[(fig+1)]-1)]
	legend(leg.loc, state.names, lty=1, lwd=2, col=col.names, bg="white",
	       inset=0.01, cex=0.95, bty="n")
	text(lev.reg.x, lev.reg.y, us.regions.lab[fig], font=2, pos=pos.reg, cex=1.5)
	#text(xmax+1.5, lev.reg, us.regions.lab[fig], font=2, pos=2, cex=1.5)
}