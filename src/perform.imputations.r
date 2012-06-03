assemble.frame <- function(original.frame, replacement.vector, variable.to.replace)
{
	variable.symbol		<- paste(variable.to.replace, sep="", collapse=",")
	symbol.to.remove	<- paste("-", "c(", variable.symbol, ")", sep="")
	
	x <- subset(original.frame, select = eval(parse(text=symbol.to.remove)))
	
	return( cbind(x, eval(parse(text=paste("data.frame(", variable.to.replace, "= replacement.vector)" )))) )
}

perform.imputations <- function(
						original.frame,
						variable.to.replace,
						input.set,
						technique,
						Transform.Categoricals=function(x){x}, 
						Transform.Logicals=function(x){x},
						model.formula
					)
{
	result <- c()

	for(missing.vector in input.set)
	{
		bound.frame <- assemble.frame(original.frame, missing.vector, variable.to.replace)
		
		# ####### REGULAR!!!!
		# bound.frame <- Transform.Logicals( Transform.Categoricals( bound.frame ))
		# ####### / REGULAR!!!!
		
		# ####### EXPERIMENT 1 !!!!
		# # bound.frame <- Transform.Logicals( Transform.Categoricals( bound.frame ))
		# ####### / EXPERIMENT 1 !!!!
		
		####### EXPERIMENT 2 !!!!
		bound.frame <- Transform.Logicals(  bound.frame )
		####### / EXPERIMENT 2 !!!!
		imputed.frame <- technique(bound.frame, model.formula)
		
		result <- c(result, list(imputed.frame))
	}
	
	return (result)
}




save.imputations <- function( imputed.frames, run.time=0, impute.file.prefix="" )
{
	if(run.time > 0)
	{
		write.csv( run.time, file=paste(impute.file.prefix, " runtime",	".csv", sep=""))
	}
	
	for(index in c(1:length(imputed.frames)))
	{
		write.csv( imputed.frames[index], paste(impute.file.prefix, index, ".csv", sep=""))
	}
}




#test.imputation.subset <- function(input.set, off=1, len=10)
#{
#	result <- c()
#	
#	for(j in off:len+off) {
#		missing.vector <- input.set[[j]]
#		result <- c(result, perform.imputation(missing.vector, variable.to.replace, categorical.mode))
#	}
#	
#	return (result)
#}

#test.imputation.itteration <- function(input.set, j)
#{
#	test.imputation.subset(input.set, j, j)
#}