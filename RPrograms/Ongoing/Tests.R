#-----------------------------------------------------------------------------#
# Program: Tests.r                                                            #
# Description: Pieces of code to try out and/or test various things!					#
# Name: Nadine Ouellette                                                      #
# Date: February 15 2024                                                      #
#-----------------------------------------------------------------------------#

rm(list=ls(all=TRUE))


# Loading the data
# ----------------
# Note: ASDR array prepared by Magali, using data from the RDC
setwd("~/OneDrive - Universite de Montreal/Projet_USStates_Mortality/USStates_CVRev/ROutputs")
load(file="1941-2020/asdr_by3cod_4084.RData")
ls()
str(asdr.by3cod.4084)
asdr.4084 <- asdr.by3cod.4084
rm(asdr.by3cod.4084)
str(asdr.4084)


# Graphics parameters
# -------------------

# Male or female selection
sex.id <- "m" ## m: Male; f: Female

# Calendar years
x <- as.numeric(dimnames(asdr.4084)[[2]])
xmin <- min(x)
xmax <- max(x) - 1  ## without COVID-19

# Organize states by regions
new.engl <- c("CT", "ME", "MA", "NH", "RI", "VT")
mid.atl <- c("NJ", "NY", "PA")
east.north.cent <- c("IL", "IN", "MI", "OH", "WI")
west.north.cent <- c("IA", "KS", "MN", "MO", "NE", "ND", "SD")
mountain <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY")
pacific <- c("AK", "CA", "HI", "OR", "WA")
south.atl <- c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV")
east.south.cent <- c("AL", "KY", "MS", "TN")
west.south.cent <- c("AR", "LA", "OK", "TX")
all.reg <- c(new.engl, mid.atl, east.north.cent, west.north.cent, mountain,
             pacific, south.atl, east.south.cent, west.south.cent)

new.engl.lab <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire",
                  "Rhode Island", "Vermont")
mid.atl.lab <- c("New Jersey", "New York", "Pennsylvania")
east.north.cent.lab <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin")
west.north.cent.lab <- c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska",
                         "North Dakota", "South Dakota")
mountain.lab <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada",
                  "New Mexico", "Utah", "Wyoming")
pacific.lab <- c("Alaska", "California", "Hawaii", "Oregon", "Washington")
south.atl.lab <- c("Delaware", "Dist. of Columbia", "Florida", "Georgia",
                   "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia")
east.south.cent.lab <- c("Alabama", "Kentucky", "Mississippi", "Tennessee")
west.south.cent.lab <- c("Arkansas", "Louisiana", "Oklahoma", "Texas")
all.reg.lab <- c(new.engl.lab, mid.atl.lab, east.north.cent.lab,
                 west.north.cent.lab, mountain.lab, pacific.lab, south.atl.lab,
                 east.south.cent.lab, west.south.cent.lab)

us.regions.lab <- c("New England", "Middle Atlantic", "East North Central",
                    "West North Central", "Mountain", "Pacific",
                    "South Atlantic", "East South Central", "West South Central")

# Number of states in each region
nb.states.reg <- c(length(new.engl), length(mid.atl), length(east.north.cent),
                   length(west.north.cent), length(mountain), length(pacific),
                   length(south.atl), length(east.south.cent), length(west.south.cent))

# Position of 1st state in each region
state1.reg <- c(1, 1+cumsum(nb.states.reg))

# Number of states in each region minus 1 (for adding "lines" in each graph)
ligne <- nb.states.reg - 1

# Line colors for each state in each group
shades.gray <- gray(0:8/8)
col.palette <- c("black","red","#72D9FF","green","orange","orchid",shades.gray[5],
                 "royalblue",shades.gray[7])
col.reg1 <- col.palette[1:nb.states.reg[1]]
col.reg2 <- col.palette[1:nb.states.reg[2]]
col.reg3 <- col.palette[1:nb.states.reg[3]]
col.reg4 <- col.palette[1:nb.states.reg[4]]
col.reg5 <- col.palette[1:nb.states.reg[5]]
col.reg6 <- col.palette[1:nb.states.reg[6]]
col.reg7 <- col.palette[1:nb.states.reg[7]]
col.reg8 <- col.palette[1:nb.states.reg[8]]
col.reg9 <- col.palette[1:nb.states.reg[9]]
col.reg <- c(col.reg1, col.reg2, col.reg3, col.reg4, col.reg5, col.reg6,
             col.reg7, col.reg8, col.reg9)



# Sex-specific ASDR for all states in a single figure
# ---------------------------------------------------
setwd("~/OneDrive - Universite de Montreal/Projet_USStates_Mortality/USStates_CVRev/Figures/ASDR")

xtckmin <- 1940
xtckmax <- 2020

dev.off()
pdf(paste("ASDR-All_AllCauses_USStates_", ifelse(sex.id=="m", "Males", "Females"), ".pdf", sep=""), width=8.5, height=11)
plot(NA, NA, xlim=c(xmin,xmax), ylim=c(ymin,ymax), yaxs="i", xaxt="n", yaxt="n", 
     xlab=NA, ylab="Rate (per 1,000 population)", font.lab=2)
for(i in all.reg){
  y <- asdr.by3cod.4084[i,,sex.id,"All"][-length(x)]*1000
  #y <- asdr.by3cod.4084[i,,sex.id,"All"]*1000  # with COVID-19
  lines(x[-length(x)], y, lwd=2, col="gray")
}
dev.off()

# Sex-specific ASDR by states on a semi-logarithmic scale
# -------------------------------------------------------
dev.off()
pdf(paste("ASDR-log_AllCauses_USStates_", ifelse(sex.id=="m", "Males", "Females"), ".pdf", sep=""), width=8.5, height=11)
par(mfrow=c(3,3), mar=c(2.7,2.7,0.5,0.5), mgp=c(1.75,0.5,0), tck=-0.015,
    cex.lab=1, cex.axis=1)

for(fig in 1:length(us.regions.lab)) {
  id <- state1.reg[fig]
  y <- asdr.by3cod.4084[which(dimnames(asdr.by3cod.4084)[[1]]==
                              all.reg[id]),,sex.id,"All"]*1000
  
  if(fig==1){ par(mar=c(1,2.7,0.5,0.5)) }
  if(fig==2 | fig==3){ par(mar=c(1,1,0.5,0.5)) }
  if(fig==4){ par(mar=c(1,2.7,0.5,0.5)) }
  if(fig==5 | fig==6){ par(mar=c(1,1,0.5,0.5)) }
  if(fig==7){ par(mar=c(2.7,2.7,0.5,0.5)) }
  if(fig==8 | fig==9){ par(mar=c(2.7,1,0.5,0.5)) }
  
  if(fig==1 | fig==4){
    plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax),
         xlab=NA, ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n", 
         yaxt="n", col=col.reg[id], font.lab=2, log="y")
    axis(1, at=seq(xtckmin,xtckmax,by=10), labels=F)
    axis(side=2, at=seq(ymin,ymax,4), las=1)
    axis(side=4, at=seq(ymin,ymax,4), tck=0.015, labels=F)
  }
  if(fig==8 | fig==9){
    plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
         ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id])
    axis(1, at=seq(xtckmin,xtckmax,by=10), log="y")
    axis(side=2, at=seq(ymin,ymax,4), las=1, labels=F)
    axis(side=4, at=seq(ymin,ymax,4), tck=0.015, labels=F)
    mtext("Year", side=1, line=1.5, font=2, cex=0.65)
  }
  if(fig==7){
    plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
         ylab="Rate (per 1,000 population)", yaxs="i", xaxt="n", yaxt="n",
         col=col.reg[id], font.lab=2, log="y")
    axis(1, at=seq(xtckmin,xtckmax,by=10))
    axis(side=2, at=seq(ymin,ymax,4), las=1)
    axis(side=4, at=seq(ymin,ymax,4), tck=0.015, labels=F)
    mtext("Year", side=1, line=1.5, font=2, cex=0.65)
  }
  if(fig==2 | fig==3 | fig==5 | fig==6){
    plot(x, y, type="l", lwd=2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab=NA,
         ylab=NA, yaxs="i", xaxt="n", yaxt="n", col=col.reg[id], log="y")
    axis(1, at=seq(xtckmin,xtckmax,by=10), labels=F)
    axis(side=2, at=seq(ymin,ymax,4), las=1, labels=F)
    axis(side=4, at=seq(ymin,ymax,4), tck=0.015, labels=F)
  }
  
  for(c in 1:(ligne[fig])) {
    y <- asdr.by3cod.4084[which(dimnames(asdr.by3cod.4084)[[1]]==
                                all.reg[id+c]),,sex.id,"All"]*1000
    lines(x, y, lwd=2, col=col.reg[id+c])
  }
  
  state.names <- all.reg.lab[state1.reg[fig]:(state1.reg[(fig+1)]-1)]
  col.names <- col.reg[state1.reg[fig]:(state1.reg[(fig+1)]-1)]
  legend(leg.loc, state.names, lty=1, lwd=2, col=col.names, bg="white",
         inset=0.01, cex=0.95, bty="n")
  text(xmax+1.5, ymax-1.5, us.regions.lab[fig], font=2, pos=2, cex=1.5)
}
dev.off()