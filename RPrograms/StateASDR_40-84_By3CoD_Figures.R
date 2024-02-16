#-----------------------------------------------------------------------------#
# Program: StateASDR_40-84_By3CoD_Figures.r                                   #
# Description:	*** By 3 CoD (CVDs, cancers, other diseases) ***							#
#				        For each sex, plot all-cause age-adjusted death rates         #
#				        (ages 40-84) for all U.S. states by region, since 1941        #
# Name: Nadine Ouellette                                                      #
# Date: October 24, 2023                                                      #
# Modified: February 12, 2024 (new ASDR arrays)                               #
#-----------------------------------------------------------------------------#

rm(list=ls(all=TRUE))

# -----
# PLOTS
# -----

# Load ASDR array prepared by Magali, using data from the RDC
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
sex.id <- "f" ## m: Male; f: Female

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
                   "Maryland", "North Carolina", "South Carolina", "Virginia",
                   "West Virginia")
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


# Plots
# -----
setwd("~/OneDrive - Universite de Montreal/Projet_USStates_Mortality/USStates_CVRev/Figures/ASDR")

xtckmin <- 1940
xtckmax <- 2020
pos.reg <- 2  ## position of region's names


# Cardiovascular diseases
leg.loc <- "bottomleft"
asdr <- asdr.4084[,,sex.id,2]
ymin <- ifelse(sex.id=="m", 2, 1)
ymax <- ifelse(sex.id=="m", 30.75, 19.5)
by.y <- ifelse(sex.id=="m", 4, 3) ## Distance between tick marks on the y-axis
lev.reg.x <- xmax + 1.5 ## x-location of region's names
lev.reg.y <- ifelse(sex.id=="m", 29.5, 18.75) ## y-location of region's names

dev.off()
pdf(paste("ASDR4084_CVDs_USStates_", sex.id, ".pdf", sep=""), width=8.5, height=11)
source(file="~/OneDrive - Universite de Montreal/Projet_USStates_Mortality/USStates_CVRev/Rprograms/Fig_StatesByRegion.r")
#title(paste("ASDR 40-84, MCVs, ",
            #ifelse(sex.id=="m", "Males", "Females"), sep=""),
      #line=-1.15, cex.main=1.5, font.main=2, outer=TRUE)
dev.off()


# Malignant neoplasms
leg.loc <- ifelse(sex.id=="m", "bottomright", "topright")
asdr <- asdr.4084[,,sex.id,1]
ymin <- ifelse(sex.id=="m", 1, 1)
ymax <- ifelse(sex.id=="m", 9.5, 6.5)
by.y <- ifelse(sex.id=="m", 2, 1) ## Distance between tick marks on the y-axis
lev.reg.x <- xmax + 1.5 ## x-location of region's names
lev.reg.y <- ifelse(sex.id=="m", 9.1, 1.2) ## y-location of region's names

dev.off()
pdf(paste("ASDR40-84_MN_USStates_", sex.id, ".pdf", sep=""),width=8.5,height=11)
source(file="~/OneDrive - Universite de Montreal/Projet_USStates_Mortality/USStates_CVRev/Rprograms/Fig_StatesByRegion.r")
#title(paste("ASDR 40-84, Cancers, ",
      #ifelse(sex.id=="m", "Males", "Females"), sep=""),
      #line=-1.15, cex.main=1.5, font.main=2, outer=TRUE)
dev.off()


# All other diseases
leg.loc <- ifelse(sex.id=="m", "topleft", "bottomright")
asdr <- asdr.4084[,,sex.id,3]
ymin <- ifelse(sex.id=="m", 4, 1)
ymax <- ifelse(sex.id=="m", 12, 6.6)
by.y <- ifelse(sex.id=="m", 2, 1) ## Distance between tick marks on the y-axis
lev.reg.x <- xmax + 1.5 ## x-location of region's names
lev.reg.y <- ifelse(sex.id=="m", 11.65, 6.35) ## y-location of region's names

dev.off()
pdf(paste("ASDR40-84_Other_USStates_", sex.id, ".pdf", sep=""),width=8.5,height=11)
source(file="~/OneDrive - Universite de Montreal/Projet_USStates_Mortality/USStates_CVRev/Rprograms/Fig_StatesByRegion.r")
#title(paste("ASDR 40-84, Cancers, ",
      #ifelse(sex.id=="m", "Males", "Females"), sep=""),
      #line=-1.15, cex.main=1.5, font.main=2, outer=TRUE)
dev.off()