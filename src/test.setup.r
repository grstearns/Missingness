source('modelsetup.r')

s	<- 1
mtd	<- 'lwd'
missing.file					<- missing.files[[s]]
variable.to.replace				<- missing.variables[[s]]

base.dir	<- getwd()
results.dir	<- paste(base.dir, "/results", sep="")
method.dir	<- paste(results.dir, mtd, sep="/")

dir.create(method.dir)

######################################################
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
}

transform.logicals.numeric <- function(x) {
	x$p25 <- as.integer(x$p25)
}


#######################################################
outp <- paste( method.dir, missing.file, "/", sep="/")
dir.create(outp)

original.frame					<- not.missing
input.set						<- read.csv(paste(missing.file, ".csv", sep=""))
impute.file.prefix				<- outp
graph.file.prefix				<- outp
model.formula					<- "LnVarietyScore2~usrUrban+usrSuburban+age1+p25+y1v19"
output.imputation				<- TRUE
Transform.Logicals				<- function(x){x}
Transform.Categoricals			<- function(x){x}
summary.provided.by.technique	<- FALSE
technique						<- function(x, model.formula) { na.omit(x) }
Transform.Categoricals			<- transform.categoricals.bool


#######################################################

directory<-'C:\\missingness\\Results\\lwd\\missing.age.mar.sex.05'
original.frame<-not.missing
