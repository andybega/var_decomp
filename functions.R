##
##		Functiosn for decomposing and plotting variable variance
##		Andreas Beger
##		15 May 2014
##
varDecomp <- function(group, var) {
	# Decompose variance
	# group - vector denoting group membership
	# var   - variable to decompose
	# Returns vector with total, within, and between variance

	# Calculate raw deviations
	df <- data.frame(group=group, var=var)
	df$group.mean <- ave(df$var, df$group)
	df$within <- with(df, var - group.mean)
	df$between <- with(df, group.mean - mean(var))

	# Calculate variance given df with var, within and between dev.
	v.total   <- mean((var - mean(var))^2)
	v.within  <- mean(df$within^2)
	v.between <- mean(df$between^2)
	res <- c(total=v.total, within=v.within, between=v.between)
	res
}

varDecompDF <- function(data, group, var.names=NULL) {
	# Decompose between and total variance for variabes in data by group
	# data - a data frame that includes columns group and var.names
	# group - string name for column to group by
	# var.names - character vector of variables to plot, if empty all
	#             columns but group will be plotted
	if (is.null(var.names)) {
		numeric.cols <- sapply(data, is.numeric)
		not.group <- !is.element(colnames(data), group)
		var.names <- colnames(data)[numeric.cols & not.group]
	}

	# Decompose variance for var.names in data over group
	var.df <- lapply(var.names, function(x) varDecomp(group=data[, group], 
		var=data[, x]))
	var.df <- do.call("rbind", var.df)
	var.df <- data.frame(variable=var.names, var.df)
	var.df
}

plot.varDecomp <- function(group, var) {
	# Plot between and within variance for a single variable.
	library(reshape2)
	library(ggplot2)

	df <- devDecomp(group, var)
	df.long <- melt(df, "group", c("within", "between"))

	ss <- varDecomp(group, var)
	
	mf_labeller <- function(var, value){
    	value <- as.character(value)
    	if (var=="variable") { 
        	value[value=="within"] <- paste("Var[within]/Var[total] == ", 
        		sprintf("%.2f", ss["within"]/ss["total"]))
        	value[value=="between"]   <- paste("Var[between]/Var[total] ==", 
        		sprintf("%.2f", ss["between"]/ss["total"]))
    	}
    llply(as.character(value), function(x) parse(text = x))
	}

	p <- ggplot(data=df.long) + geom_histogram(aes(x=value)) + 
		facet_grid(. ~ variable, labeller=mf_labeller) + theme_bw() +
		xlab("Absolute difference") +
		theme(axis.title.y=element_blank())
	p
}

#plot.varDecomp(thai.data$ccode, thai.data$AUTOC.l1)
#plot.varDecomp(thai.data$ccode, thai.data$i.conf.GOVtGOV.l1)

plotVarDecomp <- function(data, group, var.names=NULL, labels=NULL, 
	log.y=TRUE) {
	# Plot between and total variance for variabes in data by group
	# data - a data frame that includes columns group and var.names
	# group - string name for column to group by
	# var.names - character vector of variables to plot, if empty all
	#             columns but group will be plotted
	# log.y - Log the x-scale (total variance)?
	
	var.df <- varDecompDF(data=data, group=group, var.names=var.names)
	# Proportion between groups
	var.df$between.p <- var.df$between/var.df$total
	
	if (is.null(labels)) {
		var.df$label <- var.df$variable
	} else {
		var.df$label <- labels
	}

	p <- ggplot(data=var.df, aes(x=total, y=between.p, label=label)) +
		geom_abline(intercept=0.5, slope=0, size=0.5, col="cyan", alpha=0.5) +
		geom_point(color="red", size=1, alpha=0.7) +
		geom_text(size=4, hjust=0.5, vjust=0.5, alpha=0.7) +
		scale_y_continuous(limits=c(0, 1)) +
		xlab("Total variance") + ylab("Between/Total") +
		theme_bw() +
		theme(plot.title = element_text(vjust=2))
	
	if (log.y==TRUE) {
		# Minimum for x axis
		x.min <- min(var.df$total) * 0.1
		x.max <- max(var.df$total) * 10
		p <- p + scale_x_log10(limits=c(x.min, x.max))
	} else {
		x.min <- 0
		x.max <- max(var.df$total) * 1.1
		p <- p + scale_x_continuous(limits=c(x.min, x.max))
	}

	return(p)
}
