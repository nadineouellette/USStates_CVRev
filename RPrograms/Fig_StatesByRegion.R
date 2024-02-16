#-------------------------------------------------------------------------#
# Program: Fig_StatesByRegion.r														                #
# Description: Code used to plot ASDR results for U.S. states by region   #
#              over time.                                                 #
# Name: Nadine Ouellette                                                  #
# Date: October 24, 2023                                                  #
# Modified: February 12, 2024 (new ASDR arrays)                           #
#-------------------------------------------------------------------------#

par(mfrow=c(3,3), mar=c(2.7,2.7,0.5,0.5), mgp=c(1.75,0.5,0), tck=-0.015,
    cex.lab=1, cex.axis=1)

for(fig in 1:length(us.regions.lab)){
  id <- state1.reg[fig]
  y <- asdr[all.reg[id],][-length(x)]*1000
  ## With COVID-19
  #y <- asdr[all.reg[id],]*1000
  
  if(fig==1){ par(mar=c(1,2.7,0.5,0.5)) }
  if(fig==2 | fig==3){ par(mar=c(1,1,0.5,0.5)) }
  if(fig==4){ par(mar=c(1,2.7,0.5,0.5)) }
  if(fig==5 | fig==6){ par(mar=c(1,1,0.5,0.5)) }
  if(fig==7){ par(mar=c(2.7,2.7,0.5,0.5)) }
  if(fig==8 | fig==9){ par(mar=c(2.7,1,0.5,0.5)) }
  
  if(fig==1 | fig==4){
    plot(x[-length(x)], y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax),
         xlab=NA, ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n",
         yaxt="n", col=col.reg[id], font.lab=2)
    ## With COVID-19
    #plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
         #ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n", yaxt="n", 
         #col=col.reg[id], font.lab=2)
    axis(1, at=seq(xtckmin,xtckmax,by=10), labels=F)
    axis(side=2, at=seq(ymin,ymax,by.y), las=1)
    axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
  }
  if(fig==8 | fig==9){
    plot(x[-length(x)], y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax),
         xlab=NA, ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
    ## With COVID-19
    # plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
          #ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
    axis(1, at=seq(xtckmin,xtckmax,by=10))
    axis(side=2, at=seq(ymin,ymax,by.y), las=1, labels=F)
    axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
    mtext("Year", side=1, line=1.5, font=2, cex=0.65)
  }
  if(fig==7){
    plot(x[-length(x)], y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax),
         xlab=NA, ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n",
         yaxt="n", col=col.reg[id], font.lab=2)
    ## With COVID-19
    #plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
         #ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n", yaxt="n",
         #col=col.reg[id], font.lab=2)
    axis(1, at=seq(xtckmin,xtckmax,by=10))
    axis(side=2, at=seq(ymin,ymax,by.y), las=1)
    axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
    mtext("Year", side=1, line=1.5, font=2, cex=0.65)
  }
  if(fig==2 | fig==3 | fig==5 | fig==6){
    plot(x[-length(x)], y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax),
         xlab=NA, ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
    ## With COVID-19
    #plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
         #ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
    axis(1, at=seq(xtckmin,xtckmax,by=10), labels=F)
    axis(side=2, at=seq(ymin,ymax,by.y), las=1, labels=F)
    axis(side=4, at=seq(ymin,ymax,by.y), tck=0.015, labels=F)
  }
  
  for(c in 1:(ligne[fig])) {
    y <- asdr[all.reg[id+c],][-length(x)]*1000
    lines(x[-length(x)], y, lwd=2, col=col.reg[id+c])
    ## With COVID-19
    # y <- asdr[all.reg[id+c],]*1000
    #lines(x, y, lwd=2, col=col.reg[id+c])  # with COVID-19
  }
  
  state.names <- all.reg.lab[state1.reg[fig]:(state1.reg[(fig+1)]-1)]
  col.names <- col.reg[state1.reg[fig]:(state1.reg[(fig+1)]-1)]
  legend(leg.loc, state.names, lty=1, lwd=2, col=col.names, bg="white",
         inset=0.01, cex=0.95, bty="n")
  text(lev.reg.x, lev.reg.y, us.regions.lab[fig], font=2, pos=pos.reg, cex=1.5)
}