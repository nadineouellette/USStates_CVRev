#-------------------------------------------------------------------------------#
# Program: LinearModel_OptimalSlopesClean_ByCause_ASDR_40-84_Plots.r			#
# Description:	*** By cause of death ***										#
#				For each sex, plot fitted regression lines (ASDR, ages 40-84)	#
#				for all U.S. states by region. The fitted regression lines (two	#
#				or three slopes) were selected according to BIC and p-values	#
#				of estimated regression parameters.								#
# Name: Nadine Ouellette														#
# Date: 12 Feb. 2015															#
# Note: Adapated from 'LinearModel_OptimalSlopes_ByCause_ASDR_40-84_Plots.r')	#
#-------------------------------------------------------------------------------#

rm(list=ls(all=TRUE))

# Loading the data
# ----------------
# Two-slope model (one turning point)
load(file="~/Documents/Projet_USStates_Cancer/Data/ROutputs/LinearModel_ByCause_ASDR_40-84.RData")
ls()
beta0.opt.1 <- beta0.opt
beta1.opt.1 <- beta1.opt
beta2.opt.1 <- beta2.opt
BICvalue.opt.1 <- BICvalue.opt
AICvalue.opt.1 <- AICvalue.opt
rm(beta0.opt,beta1.opt,beta2.opt,BICvalue.opt,AICvalue.opt)

# Three-slope model (two truning points)
load(file="~/Documents/Projet_USStates_Cancer/Data/ROutputs/LinearModel_3Slopes_ByCause_ASDR_40-84.RData")
ls()
beta0.opt.2 <- beta0.opt
beta1.opt.2 <- beta1.opt
beta2.opt.2 <- beta2.opt
beta3.opt.2 <- beta3.opt
BICvalue.opt.2 <- BICvalue.3sl.opt
AICvalue.opt.2 <- AICvalue.3sl.opt
rm(beta0.opt,beta1.opt,beta2.opt,BICvalue.3sl.opt,AICvalue.3sl.opt)
ls()

# Plotting
# --------
# Male or female selection
sex.id <- "male" ## "male" or "female"

x <- as.numeric(dimnames(strat4084)[[2]])
xmin <- min(x)
xmax <- max(x)

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
all.reg <- c(new.engl, mid.atl, east.north.cent, west.north.cent, mountain, pacific, south.atl, east.south.cent, west.south.cent)

new.engl.lab <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")
mid.atl.lab <- c("New Jersey", "New York", "Pennsylvania")
east.north.cent.lab <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin")
west.north.cent.lab <- c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
mountain.lab <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming")
pacific.lab <- c("Alaska", "California", "Hawaii", "Oregon", "Washington")
south.atl.lab <- c("Delaware", "Dist. of Columbia", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia")
east.south.cent.lab <- c("Alabama", "Kentucky", "Mississippi", "Tennessee")
west.south.cent.lab <- c("Arkansas", "Louisiana", "Oklahoma", "Texas")
all.reg.lab <- c(new.engl.lab, mid.atl.lab, east.north.cent.lab, west.north.cent.lab, mountain.lab, pacific.lab, south.atl.lab,
	east.south.cent.lab, west.south.cent.lab)

us.regions.lab <- c("New England", "Middle Atlantic", "East North Central", "West North Central", "Mountain", "Pacific",
	"South Atlantic", "East South Central", "West South Central")

# Number of states in each region
nb.states.reg <- c(length(new.engl), length(mid.atl), length(east.north.cent), length(west.north.cent), length(mountain), length(pacific),
	length(south.atl), length(east.south.cent), length(west.south.cent))

# Position of 1st state in each region
state1.reg <- c(1, 1+cumsum(nb.states.reg))

# Number of states in each region minus 1 (for the "lines" commands in each graph)
ligne <- nb.states.reg - 1
	
# Line colors for each state in each group
shades.gray <- gray(0:8/8)
col.palette <- c("black","red","#72D9FF","green","orange","orchid",shades.gray[5],"royalblue",shades.gray[7])
col.reg1 <- col.palette[1:nb.states.reg[1]]
col.reg2 <- col.palette[1:nb.states.reg[2]]
col.reg3 <- col.palette[1:nb.states.reg[3]]
col.reg4 <- col.palette[1:nb.states.reg[4]]
col.reg5 <- col.palette[1:nb.states.reg[5]]
col.reg6 <- col.palette[1:nb.states.reg[6]]
col.reg7 <- col.palette[1:nb.states.reg[7]]
col.reg8 <- col.palette[1:nb.states.reg[8]]
col.reg9 <- col.palette[1:nb.states.reg[9]]
col.reg <- c(col.reg1, col.reg2, col.reg3, col.reg4, col.reg5, col.reg6, col.reg7, col.reg8, col.reg9)

setwd("~/Documents/Projet_USStates_Cancer/Figures")

# Figures for all causes of death combined
cause <- 6
leg.loc <- "bottomleft" ## Legend location on graphs
csasdr <- strat4084[,,sex.id,cause]
ymin <- ifelse(sex.id=="male", 12, 7)
ymax <- ifelse(sex.id=="male", 40, 22)
by.y <- ifelse(sex.id=="male", 4, 3) ## Distance between tick marks on the y-axis
lev.reg.y <- ifelse(sex.id=="male", 38.5, 21.25) ## y-location of region's names
lev.reg.x <- xmax+1.5 ## x-location of region's names
pos.reg <- 2

dev.off()
pdf(paste("ASDR40-84_AllCOD_USStates_", sex.id, "_RegLine_OptSlopesClean.pdf", sep=""),width=8.5,height=11)
source(file="~/Documents/Projet_USStates_Cancer/Rprograms/Fig_StatesByRegion_RegressionLines_OptimalSlopesClean.r")
title(paste("ASDR 40-84, all causes of death combined, ",sex.id,"s",sep=""),line=-1.15,cex.main=1.5,font.main=2,outer=TRUE)
dev.off()

# Figures for heart diseases
cause <- 1
leg.loc <- "bottomleft" ## Legend location on graphs
csasdr <- strat4084[,,sex.id,cause]
ymin <- ifelse(sex.id=="male", 2, 1)
ymax <- ifelse(sex.id=="male", 18, 10)
by.y <- ifelse(sex.id=="male", 4, 1.5) ## Distance between tick marks on the y-axis
lev.reg.y <- ifelse(sex.id=="male", 17.25, 9.6) ## y-location of region's names
lev.reg.x <- xmax+1.5 ## x-location of region's names
pos.reg <- 2

dev.off()
pdf(paste("ASDR40-84_HeartDis_USStates_", sex.id, "_RegLine_OptSlopesClean.pdf", sep=""),width=8.5,height=11)
source(file="~/Documents/Projet_USStates_Cancer/Rprograms/Fig_StatesByRegion_RegressionLines_OptimalSlopesClean.r")
title(paste("ASDR 40-84, heart diseases, ",sex.id,"s",sep=""),line=-1.15,cex.main=1.5,font.main=2,outer=TRUE)
dev.off()

# Figures for other CVDs
cause <- 2
leg.loc <- "topright" ## Legend location on graphs
csasdr <- strat4084[,,sex.id,cause]
ymin <- ifelse(sex.id=="male", 0.4, 0.3)
ymax <- ifelse(sex.id=="male", 7.6, 5.3)
by.y <- ifelse(sex.id=="male", 1.2, 1) ## Distance between tick marks on the y-axis
lev.reg.y <- ifelse(sex.id=="male", ymin+0.25, ymin+0.175) ## y-location of region's names
lev.reg.x <- xmin-2 ## x-location of region's names
pos.reg <- 4 ## position of region's names

dev.off()
pdf(paste("ASDR40-84_OtherCVDs_USStates_", sex.id, "_RegLine_OptSlopesClean.pdf", sep=""),width=8.5,height=11)
source(file="~/Documents/Projet_USStates_Cancer/Rprograms/Fig_StatesByRegion_RegressionLines_OptimalSlopesClean.r")
title(paste("ASDR 40-84, other CVDs, ",sex.id,"s",sep=""),line=-1.15,cex.main=1.5,font.main=2,outer=TRUE)
dev.off()

# Figures for smoking-related cancers
cause <- 3
leg.loc <- "topleft" ## Legend location on graphs
csasdr <- strat4084[,,sex.id,cause]
ymin <- ifelse(sex.id=="male", 0.6, 0)
ymax <- ifelse(sex.id=="male", 4.2, 1.8)
by.y <- ifelse(sex.id=="male", 1.2, 0.3) ## Distance between tick marks on the y-axis
lev.reg.y <- ifelse(sex.id=="male", ymin+0.125, ymin+0.07) ## y-location of region's names
lev.reg.x <- xmax+1.5 ## x-location of region's names
pos.reg <- 2 ## position of region's names

dev.off()
pdf(paste("ASDR40-84_SmkMN_USStates_", sex.id, "_RegLine_OptSlopesClean.pdf", sep=""),width=8.5,height=11)
source(file="~/Documents/Projet_USStates_Cancer/Rprograms/Fig_StatesByRegion_RegressionLines_OptimalSlopesClean.r")
title(paste("ASDR 40-84, smoking-related cancers, ",sex.id,"s",sep=""),line=-1.15,cex.main=1.5,font.main=2,outer=TRUE)
dev.off()

# Figures for other malignant neoplasms
cause <- 4
leg.loc <- ifelse(sex.id=="male", "topleft", "bottomleft") ## Legend location on graphs
csasdr <- strat4084[,,sex.id,cause]
ymin <- ifelse(sex.id=="male", 2.2, 1.7)
ymax <- ifelse(sex.id=="male", 5.4, 4.2)
by.y <- ifelse(sex.id=="male", 0.8, 0.5) ## Distance between tick marks on the y-axis
lev.reg.y <- ifelse(sex.id=="male", 2.325, 4.075) ## y-location of region's names
lev.reg.x <- xmin-1.5 ## x-location of region's names
pos.reg <- 4 ## position of region's names

dev.off()
pdf(paste("ASDR40-84_OtherMN_USStates_", sex.id, "_RegLine_OptSlopesClean.pdf", sep=""),width=8.5,height=11)
source(file="~/Documents/Projet_USStates_Cancer/Rprograms/Fig_StatesByRegion_RegressionLines_OptimalSlopesClean.r")
title(paste("ASDR 40-84, all other cancers, ",sex.id,"s",sep=""),line=-1.15,cex.main=1.5,font.main=2,outer=TRUE)
dev.off()