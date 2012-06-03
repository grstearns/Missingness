#
source('test.setup.r')
#

source('perform.imputations.r')
source('print.summary.charts.r')


summarize.imputation <- function(model.formula, imputed.frame, summary.base)
{
	# run the model
	summary.imputed <- summary(lm(eval(parse(text=model.formula)), imputed.frame))

	
	# if( !is.null(new.rownames) )
	# {
		# if(is.null(names(new.rownames))) {
			# rownames(summary.imputed$coefficients) <- new.rownames
		# } else {
			# for(name in names(new.rownames)) { rownames(summary.imputed$coefficients)[rownames(summary.imputed$coefficients)==name] <- new.rownames[name] }
		# }
	# }
	
	# This version will spit out NAs if there are singularities
	#coef.holder <- array(dim=dim(summary.base$coefficients), dimnames=dimnames(summary.base$coefficients))
	
	# This version will hide singularities as 0
	coef.holder <- array(0, dim=dim(summary.base$coefficients), dimnames=dimnames(summary.base$coefficients))
	
	for(name in (dimnames(summary.imputed$coefficients)[[1]]))
	{
		coef.holder[name,] <- summary.imputed$coefficients[name,]
	}
	
	summary.imputed$coefficients <- coef.holder
	
	return (summary.imputed)
}



summarize.imputations <- function(model.formula, imputed.frames, summary.base, graph.file.prefix)
{
	coefs		<- c()
	coef.deltas	<- c()
	r.squared	<- c()
	r.adj.squared <- c()
	model.count <- length(imputed.frames)
	
	n <- c()

	for(imputed.frame in imputed.frames)
	{
		imputation.summary <- summarize.imputation(model.formula, imputed.frame, summary.base)
		
		coefs		<- c(coefs,		imputation.summary$coefficients)
		
		# Delta = Imputed Coef - Original Coef
		# coef.deltas definition
		coef.deltas <- c(coef.deltas, imputation.summary$coefficients - summary.base$coefficients)
		
		r.squared	<- c(r.squared,	imputation.summary$r.squared)
		r.adj.squared <- c(r.squared,	imputation.summary$adj.r.squared)
		
		n <- c(n, length(imputed.frame[[1]]))
	}
	
	r.squared.deltas	<- r.squared - summary.base$adj.r.squared

	dim(coefs)			<- c(6,4,model.count)
	dim(coef.deltas)	<- c(6,4,model.count)

	# This will strip out NAs from singularities
	#coefs[is.na(coefs)] <- 0
	
	
	
	for(j in 1:6) {
		if(dimnames(summary.base$coefficients)[[1]][j] == 'age1'			) { dimnames(summary.base$coefficients)[[1]][j] <- 'Age'               }
		if(dimnames(summary.base$coefficients)[[1]][j] == 'p25TRUE'		) { dimnames(summary.base$coefficients)[[1]][j] <- 'Divorce'           }
		
		if(dimnames(summary.base$coefficients)[[1]][j] == 'usrUrbanTRUE'	) { dimnames(summary.base$coefficients)[[1]][j] <- 'Urban'             }
		if(dimnames(summary.base$coefficients)[[1]][j] == 'usrSuburbanTRUE') { dimnames(summary.base$coefficients)[[1]][j] <- 'Suburban'          }
		
		if(dimnames(summary.base$coefficients)[[1]][j] == 'usroriginalnonemissingUrban'	) { dimnames(summary.base$coefficients)[[1]][j] <- 'Urban'             }
		if(dimnames(summary.base$coefficients)[[1]][j] == 'usroriginalnonemissingSuburban') { dimnames(summary.base$coefficients)[[1]][j] <- 'Suburban'          }

		
		if(dimnames(summary.base$coefficients)[[1]][j] == 'y1v19'			) { dimnames(summary.base$coefficients)[[1]][j] <- 'School Importance' }
	}
	
	
	
	dimnames(coefs)			<- dimnames(summary.base$coefficients)
	dimnames(coef.deltas)	<- dimnames(summary.base$coefficients)
	
	
	ret <- list()
	
	
	coef.delta.means	<- apply(coef.deltas,	c(1,2), mean)
	# Mean Delta of any variable ( mean( imputed[n] - original ) )
	
	coef.means			<- apply(coefs,			c(1,2), mean)
	# Mean of any variable
	
	
	
	#########
	
	# (mean(Imputed Coef) - Original Coef) / Original
	ret$coefficient.mean.percent.change <- ( coef.means - summary.base$coefficients ) / summary.base$coefficients
	
	# mean(Imputed Coef - Original Coef)
	ret$coefficient.mean.delta <- coef.delta.means
	
	# mean(Imputed Coef)
	ret$coefficient.mean <- coef.means
	
	
	# mean(Imp Coef - Orig Coef) / std.err(Imp Coef - Orig Coef)
	ret$t.coefficient <- coef.delta.means / apply(coef.deltas, c(1,2), std.err)
	
	# ((mean(Imp Coef) - Orig Coef) / Orig ) / std.err(Imp Coef)
	ret$t.coefficient.percent.change <- ret$coefficient.mean.percent.change / apply(coefs, c(1,2), std.err)
	
	dimnames(ret$coefficient.mean.percent.change) <- dimnames(summary.base$coefficients)
	dimnames(ret$coefficient.mean.delta			) <- dimnames(summary.base$coefficients)
	dimnames(ret$coefficient.mean				) <- dimnames(summary.base$coefficients)
	dimnames(ret$t.coefficient					) <- dimnames(summary.base$coefficients)
	dimnames(ret$t.coefficient.percent.change	) <- dimnames(summary.base$coefficients)
    
	ret$bias.r.squared	<- ( mean(r.squared) - summary.base$adj.r.squared ) / summary.base$adj.r.squared
	ret$t.r.squared		<- mean(r.squared.deltas) / std.err(r.squared.deltas)
	
	#########



	est.mean <- coef.means[,1]
	# Average Estimate

	coef.std.err <- apply(coef.deltas, c(1,2), std.err)
	est.std.err <- coef.std.err[,1]
	# Std Err of Estimates

	t.coef.mean <- coef.delta.means / apply(coef.deltas, c(1,2), std.err)
	t.est.mean <- t.coef.mean[,1]
	# T of Estimate
	
	t.est.pct.chg <- ( t.est.mean - summary.base$coefficients[,3] / summary.base$coefficients[,3] ) *100
	# Percent Change in the T of the Est
	
	t.mean <- coef.means[,3]		
	# Average T

	coef.mean.pct.chg <- ( ( coef.means - summary.base$coefficients ) / summary.base$coefficients ) * 100
	# Percent Change in any variable
	
	t.pct.chg <- coef.mean.pct.chg[,3]
	# Percent Change in T
	
	Std.Err.pct.chg <- coef.mean.pct.chg[,2]
	# Percent Change in Std Err


	coef.min <- apply(coefs, c(1,2), min)
	coef.max <- apply(coefs, c(1,2), max)

	p.mean <- coef.means[,4]
	# Average P

	std.err.mean <- coef.means[,2]
	# Average Std Err

	est.min <- coef.min[,1]
	est.max <- coef.max[,1]
	# EST Min/Max

	stderr.min <- coef.min[,2]
	stderr.max <- coef.max[,2]
	# Std Err Min/Max

	t.min <- coef.min[,3]
	t.max <- coef.max[,3]
	# T Min/Max

	p.min <- coef.min[,4]
	p.max <- coef.max[,4]
	# P Min/Max

	r.avg <- mean(r.squared)
	r.adj.avg <- mean(r.adj.squared)
	r.min <- min(r.squared)
	r.max <- max(r.squared)
	
	
	


	empty.lines <- rep("", times=length(summary.base$coefficients[,1]))

	cols <- c('B', 'Std. Error of B', 't of B', 'Average Standard Error', 'Average t-value', 'Average p-value', 'Min', 'Max')
	
	rows <- dimnames(summary.base$coefficients)[[1]]
	# rows <- c('(Intercept)', 'Urban', 'Suburban', 'Age', 'Divorce', 'School Importance')
	rows <- c(rbind(rows, empty.lines), paste(text="Mean n =", mean(n), "  R² =", round(r.avg, 2), "Adj. R²=", round(r.adj.avg,2)))
	
	table.names <- list(rows, cols)


	ret$Complete.Table <- array(
		c(
			rbind(
				# B
				round(est.mean, 5),
				sapply(round(summary.base$coefficients[,1], 5), function(x){ paste("(",x,")", sep="")})
			),"",
			rbind(
				# Std. Error of the Estimates
				round(est.std.err, 5),
				empty.lines
			),"",
			rbind(
				# t of B
				round(t.est.mean, 5),
				empty.lines
			),"",
			rbind(
				# Average Standard Error
				round(std.err.mean, 5),
				sapply(round(Std.Err.pct.chg, 5), function(x){ paste("(",x,")", sep="")})
			),"",
			rbind(
				# Average t-value
				round(t.mean, 5),
				sapply(round(t.pct.chg, 5), function(x){ paste("(",x,")", sep="")})
			),"",
			rbind(
				# Average p-value
				round(p.mean, 3),
				empty.lines
			),"",
			rbind(
				# Min
				round(apply(coefs[,1,], 1,  min), 5),
				empty.lines
			),paste("(",round(r.min, 2),")", sep=""),
			rbind(
				# Max
				round(apply(coefs[,1,], 1,  max), 5),
				empty.lines
			),paste("(",round(r.max, 2),")", sep="")
		), 
		dim = c(13, 8),
		dimnames = table.names
	)


	
	print.summary.charts(summary.base$coefficients, coef.deltas, graph.file.prefix)
	#print.summary.charts(summary.base$coefficients, coefs, graph.file.prefix)
	
	return(ret)
}

summarize.base <- function(original.frame, model.formula, Transform.Logicals=function(x){x}, Transform.Categoricals=function(x){x})
{
	# ####### / REGULAR!!!!
	# original.transformed <- Transform.Logicals( Transform.Categoricals(original.frame ) )
	
	# ## TODO: why doesn't this work?
	# #summary(lm( as.formula(model.formula)), original.transformed)
	
	# summary.base <- summary(lm(eval(parse(text=model.formula)), original.transformed))
	# ###### / REGULAR!!!!
	
	# ####### EXPERIMENT 1 !!!!
	# summary.base <- summary(lm(eval(parse(text=model.formula)), original.frame))
	# ####### / EXPERIMENT 1 !!!!
	
	####### EXPERIMENT 2 !!!!
	original.transformed <- Transform.Logicals( original.frame ) 
		
	summary.base <- summary(lm(eval(parse(text=model.formula)), original.transformed))

	# if( !is.null(new.rownames) )
	# {
		# if(is.null(names(new.rownames))) {
			# rownames(sumry$coefficients) <- new.rownames
		# } else {
			# for(name in names(new.rownames)) { rownames(sumry$coefficients)[rownames(sumry$coefficients)==name] <- new.rownames[name] }		}
	# }
	
	# return(sumry)
}


Impute.And.Summarize <- function(
							original.frame, 
							variable.to.replace, 
							input.set,
							technique, 
							graph.file.prefix, 
							impute.file.prefix		= graph.file.prefix,
							Transform.Categoricals	= function(x){x},
							Transform.Logicals		= function(x){x},
							model.formula			= "LnVarietyScore2~usroriginalnonemissing+age1+p25+y1v19",
							# new.rownames			= NULL,
							summary.provided.by.technique = FALSE,
							output.imputation		= TRUE )
{
	summary.base <- summarize.base(original.frame, model.formula, Transform.Logicals, Transform.Categoricals)
	
	
	
	tstart <- proc.time()
	imputed.frames <- perform.imputations(
						original.frame,
						variable.to.replace,
						input.set,
						technique,
						Transform.Categoricals, 
						Transform.Logicals,
						model.formula
					)
	
	tend <- proc.time() - tstart
	
	if( output.imputation ) {	save.imputations( imputed.frames, tend['elapsed'], impute.file.prefix )	}

	summarize.imputations(model.formula, imputed.frames, summary.base, graph.file.prefix)
}


summarize.csvs <- function(summary.base, model.formula, graph.file.prefix, directory="")
{
	cd <- getwd()

	if(directory=="") {
		directory <- getwd()
	}
	
	setwd(directory)
	files	<- dir(pattern="^1?[0-9]?[0-9].csv$")
	frames	<- lapply(files, read.csv)
	setwd(cd)
	
	summarize.imputations( model.formula, frames, summary.base, graph.file.prefix )
	
}
