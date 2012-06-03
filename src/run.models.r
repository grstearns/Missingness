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


execute <- function(missing.file, variable.to.replace, method, impute.style,
							outp = paste( method.dir, missing.file, "", sep="/"),
							original.frame					= not.missing,
							input.set						= read.csv(paste(missing.file, ".csv", sep="")),
							impute.file.prefix				= paste(outp, "imputations/", sep=""),
							graph.file.prefix				= paste(outp, "graphs/", sep=""),
							# model.formula					= "LnVarietyScore2~usrUrban+usrSuburban+age1+p25+y1v19",
							 model.formula					= "LnVarietyScore2~usroriginalnonemissing+age1+p25+y1v19", # not doing categorical transformations
							Transform.Logicals				= function(x){x},
							Transform.Categoricals			= function(x){x},
							output.imputation				= TRUE,
							summary.provided.by.technique	= FALSE
						)
{	
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
	
	impute.style(
				original.frame, 
				variable.to.replace, 
				input.set,
				technique, 
				graph.file.prefix, 
				impute.file.prefix,
				Transform.Categoricals,
				Transform.Logicals,
				model.formula,
				# new.rownames,
				summary.provided.by.technique,
				output.imputation)
}

write.summaries.old <- function(summ, missing.file)
{
	dir.create( paste( method.dir, "/stats/", sep="" ) )
	write.csv( summ$coefficient.mean.percent.change	, file=paste(method.dir, "/stats/", missing.file, " coefficient.mean.percent.change",	".csv", sep=""))
	write.csv( summ$coefficient.mean.delta						, file=paste(method.dir, "/stats/", missing.file, " coefficient.mean.delta",			".csv", sep=""))
	write.csv( summ$coefficient.mean									, file=paste(method.dir, "/stats/", missing.file, " coefficient.mean",				".csv", sep=""))
	write.csv( summ$t.coefficient										, file=paste(method.dir, "/stats/", missing.file, " t.coefficient.means",				".csv", sep=""))
	write.csv( summ$t.coefficient.percent.change			, file=paste(method.dir, "/stats/", missing.file, " t.coefficient.percent.change",	".csv", sep=""))
	write.csv( summ$bias.r.squared									, file=paste(method.dir, "/stats/", missing.file, " r.squared.mean.percent.change",	".csv", sep=""))
	write.csv( summ$t.r.squared										, file=paste(method.dir, "/stats/", missing.file, " t.r.squared",				".csv", sep=""))
}

write.summaries <- function( summ, missing.file )
{
	dir.create( paste( method.dir, "/stats/", sep="" ) )
	write.summaries.old( summ, missing.file )
	write.csv( summ$Complete.Table , file=paste(method.dir, "/stats/", missing.file, " Complete.Table",	".csv", sep=""))
}

Summarize.Only <- function(path=method.dir)
{
	summary.base <- summarize.base(original.frame, model.formula, Transform.Logicals, Transform.Categoricals)

	for(directory in list.dirs(path))
	{
		summarize.csvs(summary.base, model.formula, graph.file.prefix, path)
	}
}

test.imputations <- function(i=7, mtd='lwd')
{
	execute(
		missing.files[[i]], 
		missing.variables[[i]], 
		mtd,
		wrap.imputations)
}

test.full <- function(i=7, mtd='lwd')
{
	execute(
		missing.files[[i]], 
		missing.variables[[i]], 
		mtd,
		Impute.And.Summarize)
}

wrap.imputations <- function(
							original.frame, 
							variable.to.replace, 
							input.set,
							technique, 
							graph.file.prefix		= "", 
							impute.file.prefix		= graph.file.prefix,
							Transform.Categoricals	= function(x){x},
							Transform.Logicals		= function(x){x},
							model.formula			= "LnVarietyScore2~usroriginalnonemissing+age1+p25+y1v19",
							summary.provided.by.technique = FALSE,
							output.imputation		= TRUE )
{
	perform.imputations (
						original.frame,
						variable.to.replace,
						input.set,
						technique,
						Transform.Categoricals, 
						Transform.Logicals,
						model.formula
					)
}

Wrap.CSV.Summarize <- function(
							original.frame, 
							variable.to.replace, 
							input.set,
							technique, 
							graph.file.prefix		= "", 
							impute.file.prefix		= graph.file.prefix,
							Transform.Categoricals	= function(x){x},
							Transform.Logicals		= function(x){x},
							model.formula			= "LnVarietyScore2~usroriginalnonemissing+age1+p25+y1v19",
							summary.provided.by.technique = FALSE,
							output.imputation		= TRUE )
{
	summary.base <- summarize.base(original.frame, model.formula, Transform.Logicals, Transform.Categoricals)

	summarize.csvs(summary.base, model.formula, graph.file.prefix, impute.file.prefix);
}


test.csv.summarize <- function(i=7, mtd='lwd')
{
	execute(
		missing.files[[i]], 
		missing.variables[[i]], 
		mtd,
		wrap.imputations)
}

dump.imputation.input <- function(
				original.frame, 
				variable.to.replace, 
				input.set,
				technique, 
				graph.file.prefix, 
				impute.file.prefix,
				Transform.Categoricals,
				Transform.Logicals,
				model.formula,
				summary.provided.by.technique,
				output.imputation)
{
	ret <- list()
	
	ret$original.frame               	<- original.frame               
	ret$variable.to.replace          	<- variable.to.replace          
	ret$input.set                    	<- input.set                    
	ret$technique                    	<- technique                    
	ret$graph.file.prefix            	<- graph.file.prefix            
	ret$impute.file.prefix           	<- impute.file.prefix           
	ret$Transform.Categoricals       	<- Transform.Categoricals       
	ret$Transform.Logicals           	<- Transform.Logicals           
	ret$model.formula                	<- model.formula                
	ret$summary.provided.by.technique	<- summary.provided.by.technique
	ret$output.imputation            	<- output.imputation
	
	return(ret)
}

dump.test <- function(i=7, mtd='lwd')
{
	execute(
		missing.files[[i]], 
		missing.variables[[i]], 
		mtd,
		dump.imputation.input)
}


write.execution <- function(i, mtd) {
	write.summaries(
		execute(
			missing.files[[i]], 
			missing.variables[[i]], 
			mtd,
			Impute.And.Summarize),
		missing.files[[i]]
	)
}

write.from.csvs <- function(i, mtd) {
	write.summaries(
		execute(
			missing.files[[i]], 
			missing.variables[[i]], 
			mtd,
			Wrap.CSV.Summarize),
		missing.files[[i]]
	)
}

write.test <- function(i=7, mtd='lwd')
{
	write.execution( i, mtd )
}










### Process exsisting CSVs
# for(i in 1:length(missing.files))	{ write.from.csvs( i, mtd ) } 

# Remember: find and replace for *.Complete.Table.csv 
# Find: "(
# Replace: ="(








### Standard loop over all missing files
if(cores == 1) {
	for(i in 1:length(missing.files))	{ write.execution( i, mtd ) }
}


### Half loop over all missing files
if(cores == 2) {
	if(current.core == 1){
		for(i in 1:13)	{ write.execution( i, mtd ) }
	}
	if(current.core == 2){
		for(i in 14:27)	{ write.execution( i, mtd ) }
	}
}


### Quarter Loop
if(cores == 4) {
	if(current.core == 1){
		for(i in 1:7)	{ write.execution( i, mtd ) }
	}
	if(current.core == 2){
		for(i in 8:14)	{ write.execution( i, mtd ) }
	}
	if(current.core == 3){
		for(i in 15:21)	{ write.execution( i, mtd ) }
	}
	if(current.core == 4){
		for(i in 22:27)	{ write.execution( i, mtd ) }
	}
}


### Eighth Loop
if(cores == 8)
{
	if(current.core == 1){
		for(i in 1:3)	{ write.execution( i, mtd ) }
	}
	if(current.core == 2){
		for(i in 4:7)	{ write.execution( i, mtd ) }
	}
	if(current.core == 3){
		for(i in 8:10)	{ write.execution( i, mtd ) }
	}
	if(current.core == 4){
		for(i in 11:13)	{ write.execution( i, mtd ) }
	}
	if(current.core == 5){
		for(i in 14:17)	{ write.execution( i, mtd ) }
	}
	if(current.core == 6){
		for(i in 18:20)	{ write.execution( i, mtd ) }
	}
	if(current.core == 7){
		for(i in 21:23)	{ write.execution( i, mtd ) }
	}
	if(current.core == 8){
		for(i in 24:27)	{ write.execution( i, mtd ) }
	}
}


### 16th Loop
if(cores == 16 )
{
	if(current.core == 1){
		for(i in 15)		{ write.execution( i, mtd ) }
	}   
	if(current.core == 2){
		for(i in c(1, 26))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 3){
		for(i in c(2, 25))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 4){
		for(i in c(3, 23))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 5){
		for(i in 24)		{ write.execution( i, mtd ) }
	}   
	if(current.core == 6){
		for(i in c(4, 22))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 7){
		for(i in c(5, 20))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 8){
		for(i in c(6, 19))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 9){
		for(i in 21)		{ write.execution( i, mtd ) }
	}   
	if(current.core == 10){
		for(i in c(7, 17))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 11){
		for(i in c(8, 16))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 12){
		for(i in 18)		{ write.execution( i, mtd ) }
	}   
	if(current.core == 13){
		for(i in c(9, 14))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 14){
		for(i in c(10, 13))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 15){
		for(i in c(11, 12))	{ write.execution( i, mtd ) }
	}   
	if(current.core == 16){
		for(i in 27)		{ write.execution( i, mtd ) }
	}
}
 

### End 16th





#Check.Missingness <- function(x) {
#	input.set <- read.csv(paste(x, ".csv", sep=""))
#	summaries <- sapply(input.set, summary)
#	lengths <- sapply(input.set, length)
#	(summaries["NA's",]/lengths)*100
#}
#Get.Missingness.Level <- function(x) {
#	fred <- strsplit(x, "\\.")
#	as.numeric(fred[[1]][length(fred[[1]])])
#}
#nominal <- sapply(missing.files, Get.Missingness.Level)
#actual <- sapply(missing.files, Check.Missingness)
#nominal - actual
