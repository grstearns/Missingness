source('modelsetup.r')
source('SummarizeImputation.r')

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

base.dir	<- getwd()
#base.dir	<- 'c:/missingness'
results.dir	<- paste(base.dir, "/results", sep="")
method.dir	<- paste(results.dir, mtd, sep="/")

dir.create(method.dir)


execute <- function(i, method)
{
	missing.file <- missing.files[[i]]
	outp <- paste( method.dir, missing.file, "/", sep="/")
	dir.create(outp)
	
	if(method == "lwd")
	{
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

		write.csv( output$bias.coefficients    , file=paste(method.dir, "/", missing.file, " bias.coefficients", ".csv", sep=""))
		write.csv( output$t.coefficient.change , file=paste(method.dir, "/", missing.file, " t.coefficient"    , ".csv", sep=""))
		write.csv( output$bias.r.squared       , file=paste(method.dir, "/", missing.file, " bias.r.squared"   , ".csv", sep=""))
		write.csv( output$t.r.squared          , file=paste(method.dir, "/", missing.file, " t.r.squared"      , ".csv", sep=""))
		write.csv( output$runtime	           , file=paste(method.dir, "/", missing.file, " runtime"          , ".csv", sep=""))
	}	

	if(method == "hot")
	{
		output <- Summarize.Imputation(not.missing, missing.variables[[i]], read.csv(paste(missing.file, ".csv", sep="")), function(x){a <- rrp.impute( x, k=5); a$new.data }, paste("HOT", missing.file) )
		
		write.csv( output$bias.coefficients    , file=paste(method.dir, "/", missing.file, " bias.coefficients", ".csv", sep=""))
		write.csv( output$t.coefficient.change , file=paste(method.dir, "/", missing.file, " t.coefficient"    , ".csv", sep=""))
		write.csv( output$bias.r.squared       , file=paste(method.dir, "/", missing.file, " bias.r.squared"   , ".csv", sep=""))
		write.csv( output$t.r.squared          , file=paste(method.dir, "/", missing.file, " t.r.squared"      , ".csv", sep=""))
		write.csv( output$runtime	           , file=paste(method.dir, "/", missing.file, " runtime"          , ".csv", sep=""))
	}	

	if(method == "rf")
	{
		output <- Summarize.Imputation(not.missing, missing.variables[[i]], read.csv(paste(missing.file, ".csv", sep="")), function(x){ rfImpute(LnVarietyScore2~usroriginalnonemissing+age1+p25+y1v19, x, ntree=500) }, paste("RF", missing.file) )	
		write.csv( output$bias.coefficients    , file=paste(method.dir, "/", missing.file, " bias.coefficients", ".csv", sep=""))
		write.csv( output$t.coefficient.change , file=paste(method.dir, "/", missing.file, " t.coefficient"    , ".csv", sep=""))
		write.csv( output$bias.r.squared       , file=paste(method.dir, "/", missing.file, " bias.r.squared"   , ".csv", sep=""))
		write.csv( output$t.r.squared          , file=paste(method.dir, "/", missing.file, " t.r.squared"      , ".csv", sep=""))
		write.csv( output$runtime	           , file=paste(method.dir, "/", missing.file, " runtime"          , ".csv", sep=""))
	}	

	if(method == "mi")
	{
		output <- Summarize.Imputation(
			original.frame		= not.missing,
			variable.to.remove	= missing.variables[[i]],
			input.set			= read.csv(paste(missing.file, ".csv", sep="")), 
			technique			= function(x, model.formula){ mi.data.frame( mi(x, mi.info(x, .999), n.imp=5, n.iter = 30, add.noise=FALSE ) ) }, 
			graph.file.prefix	= paste("MI", missing.file), 
			summary.provided.by.technique = FALSE, 
			Categoricals.As		= "bool", 
			Logicals.As			= "bool", 
			model.formula		= "LnVarietyScore2~usrUrban+usrSuburban+age1+p25+y1v19"
		)
		write.csv( output$bias.coefficients    , file=paste(method.dir, "/", missing.file, " bias.coefficients", ".csv", sep=""))
		write.csv( output$t.coefficient.change , file=paste(method.dir, "/", missing.file, " t.coefficient"    , ".csv", sep=""))
		write.csv( output$bias.r.squared       , file=paste(method.dir, "/", missing.file, " bias.r.squared"   , ".csv", sep=""))
		write.csv( output$t.r.squared          , file=paste(method.dir, "/", missing.file, " t.r.squared"      , ".csv", sep=""))
		write.csv( output$runtime	           , file=paste(method.dir, "/", missing.file, " runtime"          , ".csv", sep=""))
	}	
	
	if(method == "em")
	{
	}

	#if(method == "")
	#{
	#}
	
}




### Standard loop over all missing files

if(cores == 1) {
	for(i in 1:length(missing.files)) { execute(i, mtd) }
}

### End Std Loop


### Standard loop over all missing files

if(cores == 2) {
	if(current.core == 1){
		for(i in 1:13  ) { execute(i, mtd) }
	}
	if(current.core == 2){
		for(i in 14:27  ) { execute(i, mtd) }
	}
}

### End Std Loop


### Quarter Loop

if(cores == 4) {
	if(current.core == 1){
		for(i in 1:7  ) { execute(i, mtd) }
	}
	if(current.core == 2){
		for(i in 8:14  ) { execute(i, mtd) }
	}
	if(current.core == 3){
		for(i in 15:21 ) { execute(i, mtd) }
	}
	if(current.core == 4){
		for(i in 22:27) { execute(i, mtd) }
	}
}

### End Qtr Loop



### Eighth Loop

if(cores == 8)
{
	if(current.core == 1){
		for(i in 1:3  ) { execute(i, mtd) }
	}
	if(current.core == 2){
		for(i in 4:7  ) { execute(i, mtd) }
	}
	if(current.core == 3){
		for(i in 8:10 ) { execute(i, mtd) }
	}
	if(current.core == 4){
		for(i in 11:13) { execute(i, mtd) }
	}
	if(current.core == 5){
		for(i in 14:17) { execute(i, mtd) }
	}
	if(current.core == 6){
		for(i in 18:20) { execute(i, mtd) }
	}
	if(current.core == 7){
		for(i in 21:23) { execute(i, mtd) }
	}
	if(current.core == 8){
		for(i in 24:27) { execute(i, mtd) }
	}
}

### End Eighth



### 16th Loop

if(cores == 16 )
{
	if(current.core == 1){
		for(i in 1:1  ) { execute(i, mtd) }
	}
	if(current.core == 2){
		for(i in 2:3  ) { execute(i, mtd) }
	}
	if(current.core == 3){
		for(i in 4:5  ) { execute(i, mtd) }
	}
	if(current.core == 4){
		for(i in 6:7  ) { execute(i, mtd) }
	}
	if(current.core == 5){
		for(i in 8:8  ) { execute(i, mtd) }
	}
	if(current.core == 6){
		for(i in 9:10 ) { execute(i, mtd) }
	}
	if(current.core == 7){
		for(i in 11:12) { execute(i, mtd) }
	}
	if(current.core == 8){
		for(i in 13:13) { execute(i, mtd) }
	}
	if(current.core == 9){
		for(i in 14:15) { execute(i, mtd) }
	}
	if(current.core == 10){
		for(i in 16:17) { execute(i, mtd) }
	}
	if(current.core == 11){
		for(i in 18:19) { execute(i, mtd) }
	}
	if(current.core == 12){
		for(i in 20:20) { execute(i, mtd) }
	}
	if(current.core == 13){
		for(i in 21:22) { execute(i, mtd) }
	}
	if(current.core == 14){
		for(i in 23:24) { execute(i, mtd) }
	}
	if(current.core == 15){
		for(i in 25:25) { execute(i, mtd) }
	}
	if(current.core == 16){
		for(i in 26:27) { execute(i, mtd) }
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
