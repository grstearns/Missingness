
## Since MI works differently, we need a more complex function which will do all the work and provide the summary itself
#Multiple.Imputation <- function(x, model.formula) {
#	result <- mi(x, mi.info(x, .999), 5, 30 )
#
#	imputed.model <- lm.mi(eval(parse(text=model.formula)), result)
#
#	summary.computed <- c(coef(imputed.model), se.coef(imputed.model), (coef(imputed.model) / se.coef(imputed.model)), (1-pt(abs(coef(imputed.model) / se.coef(imputed.model)), df=964))*2)
#	
#	variable.names <- dimnames(summary.base$coefficients)
#	variable.names[[1]] <- names(coef(imputed.model))
#	newcoefs <- array(summary.computed, c(6,4))
#	dimnames(newcoefs) <- variable.names
#	
#	ret <- list()
#	ret$coefficients <- newcoefs
#	ret$adj.r.squared <- 
#	
#	return(ret)
#}

Summarize.Imputation <- function(original.frame, variable.to.remove, input.set, technique, graph.file.prefix, impute.file.prefix=graph.file.prefix, summary.provided.by.technique=FALSE, Categoricals.As="Factor", Logicals.As="logical", model.formula="LnVarietyScore2~usroriginalnonemissing+age1+p25+y1v19")
{


	tstart <- proc.time()
	
	
	### REMOVE ME
	## Function inputs
	#source('modelsetup.r')
	#i <- 11
	#
	#original.frame		<- not.missing
	#variable.to.remove	<- missing.variables[[i]]
	#input.set			<- read.csv(paste(missing.files[[i]], ".csv", sep=""))
	#
	#summary.provided.by.technique <- FALSE
	#
	#model.formula <- "LnVarietyScore2~usroriginalnonemissing+age1+p25+y1v19"
	#
	##Logicals.As="bool"
	##Logicals.As="factor"
	##Logicals.As="int"
	#
	##Categoricals.As <- "Factor"
	##Categoricals.As <- "bool"
	##Categoricals.As <- "FactorBool"
	##Categoricals.As <- "int"
	#
	#
	### Listwise
	##Logicals.As<-"bool"; 	Categoricals.As <- "bool"
	##technique			<- function(x, model.formula) { na.omit(x) }
	#
	### Hot Deck
	##Logicals.As<-"bool"; 	Categoricals.As <- "bool"
	##technique			<- function(x, model.formula){ a <- rrp.impute( x, k=5); a$new.data } 
	#
	#### Random Forest
	##Logicals.As<-"Factor"; 	Categoricals.As <- "FactorBool"
	##technique			<- function(x, model.formula){ rfImpute(eval(parse(text=model.formula)), x, ntree=500) } 
	#
	### Multiple Imputation
	##Logicals.As<-"bool"; 	Categoricals.As <- "bool"
	##technique			<- 	function(x, model.formula){ mi.data.frame( mi(x, mi.info(x, .999), n.imp=5, n.iter = 30, add.noise=FALSE ) ) }
    #
	### EM
	##Logicals.As<-"bool"; 	Categoricals.As <- "bool"
	##technique			<- function(x, model.formula) { amelia(x, 1)$imputations[[1]] }
	#
	### Pairwise
	### PW:
	##technique			<- function(x, model.formula) {
	##						r.matrix = cor(x, use="pairwise.complete.obs") 
	##						#r.matrix = cor(bound.frame, use="pairwise.complete.obs")
	##						#r.matrix[c('usroriginalnonemissing','age1','p25','y1v19'),c('usroriginalnonemissing','age1','p25','y1v19')]
	##						formula.variables <- strsplit(model.formula, "~")[[1]]
	##						predictor.variables <- strsplit(formula.variables[2], "\\+")[[1]]
	##						criteria.variables <- formula.variables[1]
	##						predictor.matrix <- r.matrix[predictor.variables,predictor.variables]
	##						criteria.matrix <- r.matrix[predictor.variables,criteria.variables]
	##						model.mat <- solve(predictor.matrix, criteria.matrix)
	##						}
	#
	##graph.file.prefix	<- paste("LWD", missing.files[[i]])
	##graph.file.prefix	<- paste("HOT", missing.files[[i]])
	##graph.file.prefix	<- paste("RF", missing.files[[i]])
	##graph.file.prefix	<- paste("MI", missing.files[[i]])
	### END REMOVE

	
	
	# default level
	categorical.mode <- "factor"
	
	if( tolower(Categoricals.As) != "factor" )
	{
		categorical.mode <- "bool"
		
		# need this to bind against later
		original.frame.temp <- original.frame
		
		# Split factors into dummy variables
		original.frame$usrUrban		<- as.logical(original.frame$usroriginalnonemissing=='Urban'	)
		original.frame$usrSuburban	<- as.logical(original.frame$usroriginalnonemissing=='Suburban'	)
		original.frame$usrRural		<- as.logical(original.frame$usroriginalnonemissing=='Rural'	)
		original.frame <- subset(original.frame, select=-usroriginalnonemissing)

		
		# We have to pull one out to make it co-linnear
		#model.formula <- "LnVarietyScore2~usrRural+usrUrban+usrSuburban+age1+p25+y1v19"
		model.formula <- "LnVarietyScore2~usrUrban+usrSuburban+age1+p25+y1v19"

		if( tolower(Categoricals.As) == "int" || tolower(Categoricals.As) == "integer" || tolower(Categoricals.As) == "numeric" )
		{
			categorical.mode <- "num"
			
			# Numeric from boolean
			original.frame$usrUrban		<- as.integer(original.frame$usrUrban	)
			original.frame$usrSuburban	<- as.integer(original.frame$usrSuburban)
			original.frame$usrRural		<- as.integer(original.frame$usrRural	)
		} else if( tolower(Categoricals.As) == "factorbool" || tolower(Categoricals.As) == "boolfactor" || tolower(Categoricals.As) == "combo" )
		{
			categorical.mode <- "factorbool"
			
			# Factor from boolean
			original.frame$usrUrban		<- as.factor(original.frame$usrUrban	)
			original.frame$usrSuburban	<- as.factor(original.frame$usrSuburban	)
			original.frame$usrRural		<- as.factor(original.frame$usrRural	)
		}
	}
	
	if( tolower(Logicals.As) == "level" || tolower(Logicals.As) == "factor" )
	{
		original.frame$p25 <- as.factor(original.frame$p25)
		
	} else if( tolower(Logicals.As) == "int" || tolower(Logicals.As) == "integer" || tolower(Logicals.As) == "numeric" )
	{
		original.frame$p25 <- as.integer(original.frame$p25)
	}
	
	summary.base <- summary(lm(eval(parse(text=model.formula)), original.frame))
	

	model.count <- length(input.set)

	number.of.nas	<- c()
	observations	<- c() #this really shouldn't change... but...
	
	coefs		<- c()
	coef.deltas	<- c()
	
	r.squared	<- c()
	r.squared.deltas <- c()

	vector.index <- 1
	
##### YOU MUST CHOOSE (Debug options)

### Standard Loop
	for(missing.vector in input.set) {
### End Standard Loop

### Subset Loop
#	for(j in 1:10) {
#	missing.vector <- input.set[[j]]
#	model.count <- 10
### END Subset Loop

### Single Itteration
## Loop over data sets
#	j <- 57
#	missing.vector <- input.set[[j]]
### END Single Itteration

#### END YOU MUST CHOOSE

		variable.symbol		<- paste(variable.to.remove, sep="", collapse=",")
		symbol.to.remove	<- paste("-", "c(", variable.symbol, ")", sep="")
		
		if( categorical.mode != "factor" ) {
			bound.frame <- subset(original.frame.temp, select = eval(parse(text=symbol.to.remove)))
		} else {
			bound.frame <- subset(original.frame, select = eval(parse(text=symbol.to.remove)))
		}
		
		bound.frame <- cbind(bound.frame, eval(parse(text=paste("data.frame(", variable.to.remove, "= missing.vector)" ))))
		
		
		number.of.nas <- c(number.of.nas, summary(bound.frame[[variable.to.remove]])["NA's"])
		observations <- c(observations, length(bound.frame[[variable.to.remove]]))
		
		
		###### DUPLICATE CODE!!! ######
		if( categorical.mode != "factor" )
		{
			# Split factors into dummy variables
			bound.frame$usrUrban	<- as.logical(bound.frame$usroriginalnonemissing=='Urban'	)
			bound.frame$usrSuburban	<- as.logical(bound.frame$usroriginalnonemissing=='Suburban')
			bound.frame$usrRural	<- as.logical(bound.frame$usroriginalnonemissing=='Rural'	)
			bound.frame <- subset(bound.frame, select=-usroriginalnonemissing)

			if( categorical.mode == "num" )
			{
				# Numeric from boolean
				bound.frame$usrUrban	<- as.integer(bound.frame$usrUrban	)
				bound.frame$usrSuburban	<- as.integer(bound.frame$usrSuburban)
				bound.frame$usrRural	<- as.integer(bound.frame$usrRural	)
				
			} else if( categorical.mode == "factorbool"  )
			{
				bound.frame$usrUrban	<- as.factor(bound.frame$usrUrban	)
				bound.frame$usrSuburban	<- as.factor(bound.frame$usrSuburban)
				bound.frame$usrRural	<- as.factor(bound.frame$usrRural	)
			}
		}
		
		if( tolower(Logicals.As) == "level" || tolower(Logicals.As) == "factor" )
		{
			bound.frame$p25 <- as.factor(bound.frame$p25)
		} else if( tolower(Logicals.As) == "int" || tolower(Logicals.As) == "integer" || tolower(Logicals.As) == "numeric" )
		{
			bound.frame$p25 <- as.integer(bound.frame$p25)
		}
		###### END DUPE #####

		
		
		if(!summary.provided.by.technique)
		{
			# perform the imputation technique
			# technique is a function passed in that takes a frame and imputes the $missing vector
			# the size of the returned frame may be different
			imputed.frame <- technique(bound.frame, model.formula)
			#write.csv( imputed.frame, paste(getwd(), "/results/", graph.file.prefix, " ", vector.index, ".csv", sep=""))
			write.csv( imputed.frame, paste(impute.file.prefix, vector.index, ".csv", sep=""))

			# run the model
			summary.imputed <- summary(lm(eval(parse(text=model.formula)), imputed.frame))

			# This version will spit out NAs if there are singularities
			#coef.holder <- array(dim=dim(summary.base$coefficients), dimnames=dimnames(summary.base$coefficients))
		} else {
			summary.imputed <- technique(bound.frame)
		}
		
		# This version will hide singularities as 0
		coef.holder <- array(0, dim=dim(summary.base$coefficients), dimnames=dimnames(summary.base$coefficients))
		
		for(name in (dimnames(summary.imputed$coefficients)[[1]]))
		{
			coef.holder[name,] <- summary.imputed$coefficients[name,]
		}
		
		# Delta = Original Coef - Imputed Coef
		coef.deltas <- c(coef.deltas, summary.base$coefficients - coef.holder)
		coefs <- c(coefs, coef.holder)

		r.squared <- c(r.squared, summary.imputed$adj.r.squared)
		
		#(proc.time()-tstart)
		vector.index <- vector.index + 1
	}
	
	# This will strip out NAs from singularities
	#coefs[is.na(coefs)] <- 0
	
	dim(coefs) <- c(6,4,model.count)
	dim(coef.deltas) <- c(6,4,model.count)
	
	dimnames(coefs) <- dimnames(summary.base$coefficients)
	dimnames(coef.deltas) <- dimnames(summary.base$coefficients)
	
	#coef.deltas <- coefs - summary.base$coefficients
	r.squared.deltas <- r.squared - summary.base$adj.r.squared

	
	####### Extract a function that takes a matrix of n * lm summaries
	## Coef Deltas
	
	# TODO: Find a more vectorized way to do this
	sorted.deltas <- array(0, c(6,4,model.count))
	
	dimnames(sorted.deltas) <- dimnames(summary.base$coefficients)
	
	for(i in 1:4){
		for( j in 1:6 ) {
			sorted.deltas[j,i,] <- sort(coef.deltas[j,i,])
			
			# Don't care about Intercept
			if(j > 1)
			{
				coefname <- dimnames(summary.base$coefficients)[[2]][i]
				varname <- dimnames(summary.base$coefficients)[[1]][j]
				if(coefname == "Pr(>|t|)") { coefname <- "P" }
				
				y.axis.label <- paste("Difference In", coefname, "-", varname)
				
				png(filename=paste(graph.file.prefix, y.axis.label, ".png", sep=""))
				barplot(sorted.deltas[j,i,], ylab=y.axis.label, xlab='Sorted Itteration', axes=TRUE)
				dev.off()
			}
		}
	}
	
	# Plot non-intercept, non-T values (t >> *)
	chart <- barchart(sorted.deltas[(2:6),c(1,2,4),], stack=FALSE, xlab="Coefficient Delta")
	#png(filename=paste(getwd(), "/results/", graph.file.prefix, " 1", ".png", sep=""))
	png(filename=paste(graph.file.prefix, "Coefficient Delta", ".png", sep=""))
	print(chart)
	dev.off()
	
	# Plot non-intercept T values
	chart <- barchart(sorted.deltas[(2:6),3,], stack=FALSE, xlab="T Delta")
	#png(filename=paste(getwd(), "/results/", graph.file.prefix, " 2", ".png", sep=""))
	png(filename=paste(graph.file.prefix, "T Delta", ".png", sep=""))
	print(chart)
	dev.off()

	ret <- list()
	
	coef.delta.means <- apply(coef.deltas, c(1,2), mean)
	coef.means <- apply(coefs, c(1,2), mean)
	
	# Bias = Original Coef - mean(Imputed Coef) / Original
	ret$bias.coefficients <- (summary.base$coefficients - apply(coefs, c(1,2), mean)) / summary.base$coefficients
	
	std.err.change <- apply(coef.deltas, c(1,2), std.err)
	ret$t.coefficient.change <- coef.delta.means / std.err.change
	
	ret$bias.r.squared <- (summary.base$adj.r.squared - mean(r.squared)) / summary.base$adj.r.squared
	ret$t.r.squared <- mean(r.squared.deltas) / std.err(r.squared.deltas)
	
	
	dimnames(ret$t.coefficient.change) <- dimnames(summary.base$coefficients)

	ret$percent.NAs <- mean( number.of.nas / observations )
	
	ret$runtime <- (proc.time()-tstart)[3]

	print(ret)
}