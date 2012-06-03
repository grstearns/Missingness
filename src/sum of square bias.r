techniques <- c('mi', 'hot', 'rf', 'lwd')
missing_variables <- c('age', 'usr', 'y1v19')
missing_levels <- c('05', '25', '50')
missing_types <- c('mar.sex', 'mcar', 'mnar')
n <- length(missing_variables) * length(missing_levels) * length(missing_types) * length(techniques)
# set <- data.frame(technique=character(n), mis.variable=character(n),mis.type=character(n), mis.level=character(n), sum.bias.sq=numeric(n),sqrt.sbs=numeric(n))
set <- data.frame(technique=character(0), mis.variable=character(0),mis.type=character(0), mis.level=character(0), sum.bias.sq=numeric(0),sqrt.sbs=numeric(0), mean.range=numeric(0))

# mtx <- array()
# length(mtx) <- n
# dim(mtx) <- c(length(techniques), length(missing_variables),length(missing_types), length(missing_levels))
# dimnames(mtx) <- list(techniques, missing_variables, missing_types,missing_levels)

#debug
# technique <- techniques[[1]]
# type <- missing_types[[1]]
# level <- missing_levels[[1]]
# variable <- missing_variables[[1]]

for(technique in techniques) {
	for(type in missing_types) {
		for(level in missing_levels) {
			for(variable in missing_variables)
			{
				file_prefix <- paste('missing', variable, type, level, sep='.')
				file_suffix <- 'coefficient.mean.percent.change.csv'
				#file_suffix <- 'coefficient.mean.delta.csv'
				file_name <- paste(file_prefix, file_suffix)
				file_path <- paste('results', technique, 'stats', file_name, sep='/')
				
				tabl <- read.csv(file_path)
				
				sum.sq.est <- sum(tabl[,2]^2) # sum the squares of all Estimates

				# mtx[technique, variable, type, level] <- sum.sq.est
				
				
				file_suffix <- 'Complete.Table.csv'
				file_name <- paste(file_prefix, file_suffix)
				file_path <- paste('results', technique, 'stats', file_name, sep='/')
				
				tabl <- read.csv(file_path)
				
				max.est <- as.double(as.character(tabl[1:9,'Max'][tabl[1:9,'Max']!='']))
				min.est <- as.double(as.character(tabl[1:9,'Min'][tabl[1:9,'Min']!='']))
				mean.range <- mean(max.est - min.est)
				
				
				newrow <- data.frame(technique=technique, mis.variable=variable,mis.type=type, mis.level=level, sum.bias.sq=sum.sq.est, sqrt.sbs=sqrt(sum.sq.est), mean.rng=mean.range)
				set <- rbind(set, newrow)
			}
		}
	}
}

write.csv(set, "sum of square bias.csv")


# SSEStuff<-read.csv("sum of square bias.csv")
# attach(SSEStuff)

