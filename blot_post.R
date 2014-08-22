##
##		Code for blog post on variable variance decomposition
##		Andreas Beger
##		15 May 2014
##

# You won't be able to replicate the first figure with the "spduration" R 
# package, which is not available on CRAN.

##
##		First plot, white noise and GDP example
##

# Load data
load("data/model2.rda")
load("data/thai_data.rda")

# Email me if you really want this. Not on CRAN.
library(spduration)

# Calculate conditional hazard and generate white noise
thai.data$ch <- predict(model2, data=thai.data, stat="conditional hazard")
thai.data$noise <- rpois(nrow(thai.data), lambda=5)

# Plot:
png("thailand.png", width=800, height=800)
cex <- 1.5
# Plotting layout and parameters
layout(matrix(c(1, 2, 3), 3, 1), heights=c(8, 4, 4))
par(mar=c(1.5, 3, 2, 2), cex=cex, las=1, cex.main=1, font.main=1)
	
# Conditional hazard plot
plot(thai.data$date, thai.data$ch, type="l", col="darkblue", lwd=cex*2, 
	bty="n", xlab="", ylab="", ylim=c(0, 0.2))
grid(nx=NA, ny=NULL)
title(main="Conditional Hazard", adj=0)
	
# Mark observed event
fail.months <- thai.data$date[thai.data$failure==1]
abline(v=fail.months, col="darkred", lwd=2*cex)
fail.months.formatted <- format(fail.months, format="%Y-%m")
text(thai.data$date[thai.data$failure==1]+30, y=0.1, adj=c(0, 0), 
	labels=paste0("Event in\n", fail.months.formatted))
	
# Add noise
plot(thai.data$date, thai.data$noise, type="l", col="darkblue", lwd=2*cex, 
	bty="n", xlab="", ylab="", ylim=c(0, max(thai.data$noise)+1), xaxt="n")
title(main="White noise", adj=0)
grid(nx=NA, ny=NULL)

# Add GDP
plot(thai.data$date, thai.data$NY.GDP.PCAP.KD.l1, type="l", col="darkblue", lwd=2*cex, 
	bty="n", xlab="", ylab="", xaxt="n")
title(main="GDP per capita", adj=0)
grid(nx=NA, ny=NULL)

dev.off()


##
##		Second plot, variable decomposition
##

# Load data
load(file="data/df.rda")

library(plyr)

# Source functions
source("functions.R")

# varDecomp(thai.df$ccode, thai.df$AUTOC.l1) - will decompose variance given two
#	vectors.

labels <- c("Amnesty score", "PolIV Autocracy", "PolIV Democracy", "GDP", 
	"Population", "Oil prices", "Mil. Expd.", "Excl. Groups", "Excl. Pop.",
	"Cellphones per 1k", "CPI", "ICEWS total events", "Anti-G Protests", 
	"Intra-gov. conflict", "Gov. to Dis. mat. conflict", "knn4, anti-g. prot.",
	"Centdist, anti-g. prot.", "Gower econ, anti-g. prot.", "Gower pol, anti-g. prot.")

p <- plotVarDecomp(df, "ccode", labels=labels)

ggsave(file="varplot.png", p, width=6, height=6, dpi=300)







