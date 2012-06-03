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
	mtd <- "rf"
}

base.dir	<- getwd()
results.dir	<- paste(base.dir, "/results", sep="")
method.dir	<- paste(results.dir, mtd, sep="/")

dir.create(results.dir)
dir.create(method.dir)




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




##### ADDED

transform.categoricals.numeric2 <-
function(x)
{
x$sex<- as.integer(x$sex)
x$usroriginalnonemissing <- as.integer(x$usroriginalnonemissing)
 
return(x)
}


untransform.categoricals.numeric2 <- function(x)
{
	x$sex[x$sex==2] <- "Male"
	x$sex[x$sex==1] <- "Female"
	levels(x$sex) <- c("Female", "Male")
	x$sex <- factor(x$sex)

	x$usroriginalnonemissing[x$usroriginalnonemissing==3] <- "Urban"
	x$usroriginalnonemissing[x$usroriginalnonemissing==2] <- "Suburban"
	x$usroriginalnonemissing[x$usroriginalnonemissing==1] <- "Rural"
	levels(x$usroriginalnonemissing) <- c("Urban", "Suburban", "Rural")
	x$usroriginalnonemissing <- factor(x$usroriginalnonemissing)

	return(x)
}
#####



	###### SETUP
mtd <- "em"
i <- 7
	##### /SETUP


##
# # execute
##

missing.file             			<- missing.files[[i]]
variable.to.replace 			<- missing.variables[[i]]
method                     			<- mtd
# impute.style             			<- Wrap.CSV.Summarize


# execute <- function(missing.file, variable.to.replace, method, impute.style,


							outp = paste( method.dir, missing.file, "", sep="/")
							original.frame					= not.missing
							input.set						= read.csv(paste(missing.file, ".csv", sep=""))
							impute.file.prefix				= paste(outp, "imputations/", sep="")
							graph.file.prefix				= paste(outp, "graphs/", sep="")
							# model.formula					= "LnVarietyScore2~usrUrban+usrSuburban+age1+p25+y1v19",
							 model.formula					= "LnVarietyScore2~usroriginalnonemissing+age1+p25+y1v19" # not doing categorical transformations
							Transform.Categoricals=function(x){x}
							Untransform.Categoricals=function(x){x}
							Transform.Logicals=function(x){x}
							Untransform.Logicals=function(x){x}
							output.imputation				= TRUE
							summary.provided.by.technique	= FALSE
						# )
# {	
	dir.create(outp)
	dir.create(impute.file.prefix)
	dir.create(graph.file.prefix)
	
	# new.rownames		<- c('Age', 'Divorce', 'Urban', 'Suburban', 'School_Importance')
	# names(new.rownames) <- c('age1', 'p25TRUE', 'usrUrbanTRUE', 'usrSuburbanTRUE', 'y1v19')	


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
		
		Transform.Categoricals <- transform.categoricals.numeric2
		
		Untransform.Categoricals <- untransform.categoricals.numeric2
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
	
	# impute.style(
				# original.frame, 
				# variable.to.replace, 
				# input.set,
				# technique, 
				# graph.file.prefix, 
				# impute.file.prefix,
				# Transform.Categoricals=function(x){x}, 
				# Untransform.Categoricals=function(x){x},
				# Transform.Logicals=function(x){x},
				# Untransform.Logicals=function(x){x},
				# model.formula,
				# # new.rownames,
				# summary.provided.by.technique,
				# output.imputation)
				
				
				
# Impute.And.Summarize <- function(
							# original.frame, 
							# variable.to.replace, 
							# input.set,
							# technique, 
							# graph.file.prefix, 
							# impute.file.prefix		= graph.file.prefix,
							# Transform.Categoricals=function(x){x}, 
							# Untransform.Categoricals=function(x){x},
							# Transform.Logicals=function(x){x},
							# Untransform.Logicals=function(x){x},
							# model.formula			= "LnVarietyScore2~usroriginalnonemissing+age1+p25+y1v19",
							# # new.rownames			= NULL,
							# summary.provided.by.technique = FALSE,
							# output.imputation		= TRUE )
# {
	summary.base <- summarize.base(original.frame, model.formula, Transform.Logicals, Transform.Categoricals)
	
	
	
	tstart <- proc.time()
	# imputed.frames <- perform.imputations(
						# original.frame,
						# variable.to.replace,
						# input.set,
						# technique,
						# Transform.Categoricals=function(x){x}, 
						# Untransform.Categoricals=function(x){x},
						# Transform.Logicals=function(x){x},
						# Untransform.Logicals=function(x){x},
						# model.formula
					# )
					
	#############
	
	# perform.imputations <- function(
						# original.frame,
						# variable.to.replace,
						# input.set,
						# technique,
						# Transform.Categoricals=function(x){x}, 
						# Untransform.Categoricals=function(x){x},
						# Transform.Logicals=function(x){x},
						# Untransform.Logicals=function(x){x},
						# model.formula
					# )
# {
	result <- c()

	for(missing.vector in input.set)
	{
	
	# ##### one at a time
	# missing.vector <- input.set[[7]]
	# #####
	
		bound.frame <- assemble.frame(original.frame, missing.vector, variable.to.replace)
		
		# ####### REGULAR!!!!
		# bound.frame <- Transform.Logicals( Transform.Categoricals( bound.frame ))
		# ####### / REGULAR!!!!
		
		# ####### EXPERIMENT 1 !!!!
		# # bound.frame <- Transform.Logicals( Transform.Categoricals( bound.frame ))
		# ####### / EXPERIMENT 1 !!!!
		
		# ####### EXPERIMENT 2 !!!!
		# bound.frame <- Transform.Logicals(  bound.frame )
		# ####### / EXPERIMENT 2 !!!!
		
		
		####### EXPERIMENT 3 !!!!
		bound.frame <- Transform.Categoricals(  bound.frame )
		bound.frame <- Transform.Logicals(  bound.frame )
		####### / EXPERIMENT 3 !!!!
		
		imputed.frame <- technique(bound.frame, model.formula)
		
		####### EXPERIMENT 3 !!!!
		imputed.frame <- Untransform.Categoricals(  imputed.frame )
		imputed.frame <- Untransform.Logicals(  imputed.frame )
		####### / EXPERIMENT 3 !!!!
		
		
		result <- c(result, list(imputed.frame))
	
	}
	
	# return (result)
# }

### return
imputed.frames <- result
### /return

	#############
	
	tend <- proc.time() - tstart
	
	# if( output.imputation ) {	save.imputations( imputed.frames, tend['elapsed'], impute.file.prefix )	}

	# summarize.imputations(model.formula, imputed.frames, summary.base, graph.file.prefix)
# }
# }





