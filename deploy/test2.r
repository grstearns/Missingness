base.dir	<- 'c:/missingness'
setwd(base.dir)

source('modelsetup.r')
source('SummarizeImputation.r')


	i<-7
	
cores <- as.integer(commandArgs(TRUE)[1])
current.core <- as.integer(commandArgs(TRUE)[2])
mtd <- tolower(commandArgs(TRUE)[3])

if(is.na(cores)) {
	cores <- 1
}
if(is.na(current.core)) {
	current.core <- 1
}
if(is.na(mtd)) {
	mtd <- "lwd"
}

#base.dir	<- getwd()
results.dir	<- paste(base.dir, "/results", sep="")
method.dir	<- paste(results.dir, mtd, sep="/")

dir.create(method.dir)

##############################
	
	missing.file <- missing.files[[i]]
	outp <- paste( method.dir, missing.file, "", sep="/")
	dir.create(outp)
			
		output <- Summarize.Imputation( 
			original.frame		= not.missing,
			variable.to.remove	= missing.variables[[i]],
			input.set			= read.csv(paste(missing.file, ".csv", sep="")), 
			technique			= function(x, model.formula){ na.omit(x) }, 
			impute.file.prefix	= outp,
			graph.file.prefix	= outp,
			Categoricals.As		= "bool", 
			Logicals.As			= "bool", 
			model.formula		= "LnVarietyScore2~usrUrban+usrSuburban+age1+p25+y1v19",
			summary.provided.by.technique = FALSE
		)

	print(output)
################################
