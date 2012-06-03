##### THIS FILE TEST A COMPLETE RUN OF THE SUMMARIZE CSV BUT DOES NOT SAVE TO DISK


### Setup
setwd('c:/missingness')


source('modelsetup.r')
source('SummarizeImputation.r')

cores			<- as.integer(commandArgs(TRUE)[1])
current.core	<- as.integer(commandArgs(TRUE)[2])
mtd				<- tolower(commandArgs(TRUE)[3])

if(is.na(cores)) {
	cores <- 1
}
if(is.na(current.core)) {
	current.core <- 1
}
if(is.na(mtd)) {
	mtd <- "lwd"
}




## ARGUMENS

### write.from.csvs(7, 'lwd')
i <- 7
# i<-4
mtd <- 'rf'




base.dir	<- getwd()
results.dir	<- paste(base.dir, "/results", sep="")
method.dir	<- paste(results.dir, mtd, sep="/")

dir.create(results.dir)
dir.create(method.dir)


write.summaries <- function( summ, missing.file )
{
	write.csv( summ, file=paste(method.dir, "/", missing.file, " Complete.Table",	".csv", sep=""))
}




transform.categoricals.bool <- function( x )
{
	# Split factors into dummy variables
	x$usrUrban		<- as.logical(x$usroriginalnonemissing=='Urban'		)
	x$usrSuburban	<- as.logical(x$usroriginalnonemissing=='Suburban'	)
	x$usrRural		<- as.logical(x$usroriginalnonemissing=='Rural'		)
	
	x <- subset(x, select=-usroriginalnonemissing)

	return (x)
}

transform.categoricals.numeric <- function(x)
{
	x <- transform.categoricals.bool(x)

	# Numeric from boolean
	x$usrUrban		<- as.integer(x$usrUrban	)
	x$usrSuburban	<- as.integer(x$usrSuburban	)
	x$usrRural		<- as.integer(x$usrRural	)
	
	return (x)
}

transform.categoricals.bool.factor <- function(x)
{
	x <- transform.categoricals.bool(x)
	
	x$usrUrban		<- as.factor(x$usrUrban		)
	x$usrSuburban	<- as.factor(x$usrSuburban	)
	x$usrRural		<- as.factor(x$usrRural		)

	return (x)
}

transform.logicals.factor <- function(x) {
	x$p25 <- as.factor(x$p25)
	
	return(x)
}

transform.logicals.numeric <- function(x) {
	x$p25 <- as.integer(x$p25)
	
	return(x)
}



### Execute()
#p
missing.file	<- missing.files[[i]]
variable.to.replace	<- missing.variables[[i]]
method	<- mtd                   
impute.style	<- Impute.And.Summarize  


outp = paste( method.dir, missing.file, "", sep="/")
original.frame					= not.missing
input.set						= read.csv(paste(missing.file, ".csv", sep=""))
impute.file.prefix				= paste(outp, "imputations/", sep="")
graph.file.prefix				= paste(outp, "graphs/", sep="")

# model.formula					= "LnVarietyScore2~usrUrban+usrSuburban+age1+p25+y1v19"
model.formula			= "LnVarietyScore2~usroriginalnonemissing+age1+p25+y1v19"

Transform.Logicals				= function(x){x}
Transform.Categoricals			= function(x){x}
output.imputation				= TRUE
summary.provided.by.technique	= FALSE
#/p



	if(method == "lwd")
	{
		## Listwise
		#Logicals.As<-"bool"; 	Categoricals.As <- "bool"
		#technique			<- function(x, model.formula) { na.omit(x) }
		
		technique <- function(x, model.formula) { na.omit(x) }
		
		# Transform.Categoricals <- transform.categoricals.bool # Trying it without transformations
	}

	if(method == "hot")
	{	
		## Hot Deck
		#Logicals.As<-"bool"; 	Categoricals.As <- "bool"
		#technique			<- function(x, model.formula){ a <- rrp.impute( x, k=5); a$new.data } 

		technique <- function(x, model.formula){ (rrp.impute( x, k=5))$new.data }
		
		# Transform.Categoricals <- transform.categoricals.bool
	}	

	# The RF library can't deal with booleans, so we have to transform them into factors
	if(method == "rf")
	{	
		## Random Forest
		#Logicals.As<-"Factor"; 	Categoricals.As <- "FactorBool"
		technique			<- function(x, model.formula){ rfImpute(eval(parse(text=model.formula)), x, ntree=500) } 

		# technique <- function(x, model.formula){ rfImpute(model.formula, x, ntree=500) }  # THIS WILL RUIN YOUR DAY
		
		# Transform.Categoricals	<- transform.categoricals.bool.factor
		Transform.Logicals		<- transform.logicals.factor
	}	

	if(method == "mi")
	{		
		## Multiple Imputation
		#Logicals.As<-"bool"; 	Categoricals.As <- "bool"
		#technique			<- 	function(x, model.formula){ mi.data.frame( mi(x, mi.info(x, .999), n.imp=5, n.iter = 30, add.noise=FALSE ) ) }

		technique <- function(x, model.formula){ mi.data.frame( mi(x, mi.info(x, .999), n.imp=5, n.iter = 30, add.noise=FALSE ) ) }

		# Transform.Categoricals <- transform.categoricals.bool
	}	
	
	if(method == "em")
	{		
		## EM
		#Logicals.As<-"bool"; 	Categoricals.As <- "bool"
		#technique			<- function(x, model.formula) { amelia(x, 1)$imputations[[1]] }
		
		technique <- function(x, model.formula) { amelia(x, 1)$imputations[[1]] }
		
		# Transform.Categoricals <- transform.categoricals.bool
	}

	if(method == "pw")
	{
		summary.provided.by.technique	<- TRUE

		technique	<- function(x, model.formula) {
							r.matrix = cor(x, use="pairwise.complete.obs") 
							#r.matrix = cor(bound.frame, use="pairwise.complete.obs")
							#r.matrix[c('usroriginalnonemissing','age1','p25','y1v19'),c('usroriginalnonemissing','age1','p25','y1v19')]
							formula.variables <- strsplit(model.formula, "~")[[1]]
							predictor.variables <- strsplit(formula.variables[2], "\\+")[[1]]
							criteria.variables <- formula.variables[1]
							predictor.matrix <- r.matrix[predictor.variables,predictor.variables]
							criteria.matrix <- r.matrix[predictor.variables,criteria.variables]
							model.mat <- solve(predictor.matrix, criteria.matrix)
							
							
							### Still in development
							
							##model.mat <- model.mat[-5] #5
							#model.mat <- model.mat[-4] #4
							##model.mat <- model.mat[-3] #3
							##model.mat <- model.mat[-2] #2
							##model.mat <- model.mat[-1] #1
                            #
							#matrix.x <- matrix(c(1,1,1,1, model.mat), nrow=4, ncol=2)
							##matrix.x <- matrix(c(1,1,1,1,1, model.mat), nrow=5, ncol=2)
							#matrix.xt <- t(matrix.x)
                            #
							#solve(matrix.xt %*% matrix.x)
		}
		
		Transform.Categoricals <- transform.categoricals.bool
	}
	

### Wrap.CSV.Summarize()
	summary.base <- summarize.base(original.frame, model.formula, Transform.Logicals, Transform.Categoricals)

	
	#summarize.csvs(summary.base, model.formula, graph.file.prefix, impute.file.prefix);
	
### summarize.csvs <- function(summary.base, model.formula, graph.file.prefix, directory="")
#p
directory <- impute.file.prefix
#/p

	cd <- getwd()

	if(directory=="") {
		directory <- getwd()
	}
	
	setwd(directory)
	files	<- dir(pattern="^1?[0-9]?[0-9].csv$")
	frames	<- lapply(files, read.csv)
	setwd(cd)
	
	# summarize.imputations( model.formula, frames, summary.base, graph.file.prefix )
	

# summarize.imputations <- function(model.formula, imputed.frames, summary.base, graph.file.prefix)
# {

#p
imputed.frames <- frames
#/p



	coefs		<- c()
	coef.deltas	<- c()
	r.squared	<- c()
	r.adj.squared <- c()
	model.count <- length(imputed.frames)
	
	n <- c()

	for(imputed.frame in imputed.frames)
	{
		imputation.summary <- summarize.imputation(model.formula, imputed.frame, summary.base)
		
		# Delta = Imputed Coef - Original Coef
		coefs		<- c(coefs,		imputation.summary$coefficients)
		
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



	

	#print.summary.charts(summary.base$coefficients, coef.deltas, graph.file.prefix)
	
# print.summary.charts <- function(original.coefs, coef.deltas, graph.file.prefix="")
# {






#p
# original.coefs <- summary.base$coefficients
# #/p
	
	# # TODO: Find a more vectorized way to do this
	# sorted.deltas	<- array(0, dim(coef.deltas))
	# sorted.coefs	<- array(0, dim(coef.deltas))
	
	# for(j in 1:6) {
		# if(dimnames(original.coefs)[[1]][j] == 'age1'			) { dimnames(original.coefs)[[1]][j] <- 'Age'               }
		# if(dimnames(original.coefs)[[1]][j] == 'p25TRUE'		) { dimnames(original.coefs)[[1]][j] <- 'Divorce'           }
		# if(dimnames(original.coefs)[[1]][j] == 'usrUrbanTRUE'	) { dimnames(original.coefs)[[1]][j] <- 'Urban'             }
		# if(dimnames(original.coefs)[[1]][j] == 'usrSuburbanTRUE') { dimnames(original.coefs)[[1]][j] <- 'Suburban'          }
		# if(dimnames(original.coefs)[[1]][j] == 'y1v19'			) { dimnames(original.coefs)[[1]][j] <- 'School Importance' }
	# }
	
	# dimnames(sorted.deltas) <- dimnames(original.coefs)
	
	# for(i in 1:4){
		# for( j in 1:6 ) {
		
		
			# sorted.deltas[j,i,]	<- sort(coef.deltas[j,i,])
			# sorted.coefs[j,i,]	<- original.coefs[j,i] - sorted.deltas[j,i,]
			
			# ## Don't care about Intercept
			# #if(j > 1)
			# #{
			# #	coefname	<- dimnames(original.coefs)[[2]][i]
			# #	varname		<- dimnames(original.coefs)[[1]][j]
			# #	if(coefname == "Pr(>|t|)") { coefname <- "P" }
			# #	
			# #	
			# #	#y.axis.label <- paste("Difference In", coefname, "-", varname)
			# #	#
			# #	#png(filename = paste(graph.file.prefix, y.axis.label, ".png", sep=""))
			# #	#barplot(sorted.deltas[j,i,], ylab=y.axis.label, xlab='Itteration (Sorted)', axes=TRUE)
			# #	#abline(h = mean(sorted.deltas[j,i,]), lty="dotted", col="blue")
			# #	#
			# #	#dev.off()
            # #
			# #	y.axis.label <- paste(coefname, "-", varname)
            # #
			# #	png(filename = paste(graph.file.prefix, y.axis.label, ".png", sep=""))
			# #	barplot(sorted.coefs[j,i,], ylab=y.axis.label, xlab='Itteration (Sorted)', axes=TRUE)
			# #	abline(h = mean(sorted.coefs[j,i,]), lty="dotted", col="blue")
			# #	abline(h = original.coefs[j,i], lty="solid", col="red")
			# #	
			# #	dev.off()
            # #
			# #}
		# }
	# }
	
	# ## Plot non-intercept, non-T values (t >> *)
	# #chart <- barchart(sorted.deltas[(2:6),c(1,2,4),], stack=FALSE, xlab="Coefficient Delta")
	
	# # For LWD Only.  We don't care about the coeffcients other than the estimate
	# chart <- barchart(sorted.deltas[(2:6),1,], stack=FALSE, xlab="Estimate Delta")

	# png(filename=paste(graph.file.prefix, "Coefficient Delta", ".png", sep=""))
	# print(chart)
	# dev.off()
	
	# # Plot non-intercept T values
	# chart <- barchart(sorted.deltas[(2:6),3,], stack=FALSE, xlab="T Delta")
	# png(filename=paste(graph.file.prefix, "T Delta", ".png", sep=""))
	# print(chart)
	# dev.off()
	