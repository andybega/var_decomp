## 	Data subsetting for variance decomposition example
##	Do not run.

load("data/irc-data.rda")
load("data/irc_data_mod.rda")

df <- irc.data[, c("ccode", "Amnesty.l1", "AUTOC.l1", "DEMOC.l1", 
	"NY.GDP.MKTP.KD.l1", "SP.POP.TOTL.l1", "Eur.Brent.Price.l1",
	"MS.MIL.XPND.GD.ZS.l1", "excl_groups_count.l1",
	"exclpop.l1", "IT.CEL.SETS.P2.l1", "FP.CPI.TOTL.ZG.l1", "i.ctry.total.events",
	"i.protest.tGOV.l1", "i.conf.GOVtGOV.l1", "i.matl.conf.GOVtDIS.l1",
	"W.knn4.std.protest.tGOV.l1", "W.centdist.std.protest.tGOV.l1",
	"W.gower.econ.protest.tGOV.l1", "W.gower.pol.protest.tGOV.l1")]

save(df, file="data/df.rda")

thai.data <- irc.data[irc.data$country=="Thailand", ]
save(thai.data, file="data/thai_data.rda")

load("data/model_estimates.rda")
save(model2, file="data/model2.rda")

rm(list=ls())