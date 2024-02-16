#-----------------------------------------------------------------------------------------#
# Program: Fig_StatesByRegion_RegressionLines.r												                    #
# Description: Code used to plot results for U.S. states by region with regression lines.	#
# Name: Nadine Ouellette                                                                	#
# Date: February 24, 2014                                                              		#
#-----------------------------------------------------------------------------------------#

par(mfrow=c(3,3), mar=c(2.7,2.7,1.5,0.5), mgp=c(1.75,0.5,0), tck=-0.015, cex.lab=1, cex.axis=1)

for(fig in 1:length(us.regions.lab)) {
	id <- state1.reg[fig]
	y <- csasdr[which(dimnames(csasdr)[[1]]==all.reg[id]),]
	y.hat <- beta0.opt[which(states==all.reg[id]),sex.id,cause] + beta1.opt[which(states==all.reg[id]),sex.id,cause]*(YRS - min(YRS)) +
			 beta2.opt[which(states==all.reg[id]),sex.id,cause]*((YRS - break.yr[which(states==all.reg[id]),sex.id,cause])*
			 (YRS > break.yr[which(states==all.reg[id]),sex.id,cause]))

	if(fig==1){ par(mar=c(1,2.7,1.5,0.5)) }
	if(fig==2 | fig==3){ par(mar=c(1,1,1.5,0.5)) }
	if(fig==4){ par(mar=c(1,2.7,1.5,0.5)) }
	if(fig==5 | fig==6){ par(mar=c(1,1,1.5,0.5)) }
	if(fig==7){ par(mar=c(2.7,2.7,1.5,0.5)) }
	if(fig==8 | fig==9){ par(mar=c(2.7,1,1.5,0.5)) }

	if(fig==1 | fig==4){
		plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA, ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n", yaxt="n", 
			col=col.reg[id], font.lab=2)
		lines(YRS, y.hat*100, col=col.reg[id], lwd=1.25)
		abline(v=break.yr[which(states==all.reg[id]),sex.id,cause], col=col.reg[id], lty=2, lwd=1.5)
		axis(1, at=seq(1960,2005,by=10), labels=F) #ici
		axis(side=2, at=seq(ymin,ymax,by.y), las=1)
		axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
		#print(all.reg.lab[id])
	}
	if(fig==8 | fig==9){
		plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA, ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
		lines(YRS, y.hat*100, col=col.reg[id], lwd=1.25)
		abline(v=break.yr[which(states==all.reg[id]),sex.id,cause], col=col.reg[id], lty=2, lwd=1.5)
		axis(1, at=seq(1960,2005,by=10))
		axis(side=2, at=seq(ymin,ymax,by.y), las=1, labels=F)
		axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
		mtext("Year", side=1, line=1.5, font=2, cex=0.65)
		#print(all.reg.lab[id])
	}
	if(fig==7){
		plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA, ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n", yaxt="n",
			col=col.reg[id], font.lab=2)
		lines(YRS, y.hat*100, col=col.reg[id], lwd=1.25)
		abline(v=break.yr[which(states==all.reg[id]),sex.id,cause], col=col.reg[id], lty=2, lwd=1.5)
		axis(1, at=seq(1960,2005,by=10))
		axis(side=2, at=seq(ymin,ymax,by.y), las=1)
		axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
		mtext("Year", side=1, line=1.5, font=2, cex=0.65)
		#print(all.reg.lab[id])
	}
	if(fig==2 | fig==3 | fig==5 | fig==6){
		plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA, ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
		lines(YRS, y.hat*100, col=col.reg[id], lwd=1.25)
		abline(v=break.yr[which(states==all.reg[id]),sex.id,cause], col=col.reg[id], lty=2, lwd=1.5)
		axis(1, at=seq(1960,2005,by=10), labels=F)
		axis(side=2, at=seq(ymin,ymax,by.y), las=1, labels=F)
		axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
		#print(all.reg.lab[id])
	}

	for(c in 1:(ligne[fig])) {
		y <- csasdr[which(dimnames(csasdr)[[1]]==all.reg[id+c]),]
		y.hat <- beta0.opt[which(states==all.reg[id+c]),sex.id,cause] + beta1.opt[which(states==all.reg[id+c]),sex.id,cause]*(YRS - min(YRS)) +
				 beta2.opt[which(states==all.reg[id+c]),sex.id,cause]*((YRS - break.yr[which(states==all.reg[id+c]),sex.id,cause])*
				 (YRS > break.yr[which(states==all.reg[id+c]),sex.id,cause]))
		#print(all.reg.lab[id+c])
		lines(x, y, lwd=2, col=col.reg[id+c])
		lines(YRS, y.hat*100, col=col.reg[id+c], lwd=1.25)
		abline(v=break.yr[which(states==all.reg[id+c]),sex.id,cause], col=col.reg[id+c], lty=2, lwd=1.5)
	}
	
	state.names <- all.reg.lab[state1.reg[fig]:(state1.reg[(fig+1)]-1)]
	col.names <- col.reg[state1.reg[fig]:(state1.reg[(fig+1)]-1)]
	legend(leg.loc, state.names, lty=1, lwd=2, col=col.names, bg="white", inset=0.01, cex=0.95, bty="n")
	text(lev.reg.x, lev.reg.y, us.regions.lab[fig], font=2, pos=pos.reg, cex=1.5)
	#text(xmax+1.5, lev.reg, us.regions.lab[fig], font=2, pos=2, cex=1.5)
}


