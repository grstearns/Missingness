print.summary.charts <- function(original.coefs, coef.deltas, graph.file.prefix="")
{
	
	# TODO: Find a more vectorized way to do this
	sorted.deltas	<- array(0, dim(coef.deltas))
	sorted.coefs	<- array(0, dim(coef.deltas))
	
	for(j in 1:6) {
		if(dimnames(original.coefs)[[1]][j] == 'age1'			) { dimnames(original.coefs)[[1]][j] <- 'Age'               }
		if(dimnames(original.coefs)[[1]][j] == 'p25TRUE'		) { dimnames(original.coefs)[[1]][j] <- 'Divorce'           }
		
		if(dimnames(original.coefs)[[1]][j] == 'usrUrbanTRUE'	) { dimnames(original.coefs)[[1]][j] <- 'Urban'             }
		if(dimnames(original.coefs)[[1]][j] == 'usrSuburbanTRUE') { dimnames(original.coefs)[[1]][j] <- 'Suburban'          }
		
		if(dimnames(original.coefs)[[1]][j] == 'usroriginalnonemissingUrban'	) { dimnames(original.coefs)[[1]][j] <- 'Urban'             }
		if(dimnames(original.coefs)[[1]][j] == 'usroriginalnonemissingSuburban') { dimnames(original.coefs)[[1]][j] <- 'Suburban'          }

		
		if(dimnames(original.coefs)[[1]][j] == 'y1v19'			) { dimnames(original.coefs)[[1]][j] <- 'School Importance' }
	}
	
	dimnames(sorted.deltas) <- dimnames(original.coefs)
	
	for(i in 1:4){
		for( j in 1:6 ) {
		
		
			sorted.deltas[j,i,]	<- sort(coef.deltas[j,i,])
			sorted.coefs[j,i,]	<- original.coefs[j,i] - sorted.deltas[j,i,]
			
			## Don't care about Intercept
			#if(j > 1)
			#{
			#	coefname	<- dimnames(original.coefs)[[2]][i]
			#	varname		<- dimnames(original.coefs)[[1]][j]
			#	if(coefname == "Pr(>|t|)") { coefname <- "P" }
			#	
			#	
			#	#y.axis.label <- paste("Difference In", coefname, "-", varname)
			#	#
			#	#png(filename = paste(graph.file.prefix, y.axis.label, ".png", sep=""))
			#	#barplot(sorted.deltas[j,i,], ylab=y.axis.label, xlab='Itteration (Sorted)', axes=TRUE)
			#	#abline(h = mean(sorted.deltas[j,i,]), lty="dotted", col="blue")
			#	#
			#	#dev.off()
            #
			#	y.axis.label <- paste(coefname, "-", varname)
            #
			#	png(filename = paste(graph.file.prefix, y.axis.label, ".png", sep=""))
			#	barplot(sorted.coefs[j,i,], ylab=y.axis.label, xlab='Itteration (Sorted)', axes=TRUE)
			#	abline(h = mean(sorted.coefs[j,i,]), lty="dotted", col="blue")
			#	abline(h = original.coefs[j,i], lty="solid", col="red")
			#	
			#	dev.off()
            #
			#}
		}
	}
	
	## Plot non-intercept, non-T values (t >> *)
	chart <- barchart(sorted.deltas[(2:6),c(1,2,4),], stack=FALSE, xlab="Coefficient Delta")
	png(filename=paste(graph.file.prefix, "Coefficient Delta", ".png", sep=""))
	print(chart)
	dev.off()
	
	# For LWD Only.  We don't care about the coeffcients other than the estimate
	chart <- barchart(sorted.deltas[(2:6),1,], stack=FALSE, xlab="Estimate Delta")
	png(filename=paste(graph.file.prefix, "Estimate Delta", ".png", sep=""))
	print(chart)
	dev.off()
	
	# Plot non-intercept T values
	chart <- barchart(sorted.deltas[(2:6),3,], stack=FALSE, xlab="T Delta")
	png(filename=paste(graph.file.prefix, "T Delta", ".png", sep=""))
	print(chart)
	dev.off()
	
}
