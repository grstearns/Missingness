#import our libs
library("lattice")
library("rrp")
library("randomForest")
library("mi")
library("Amelia")


# Standard Error function
std.err <- function(x) sqrt(var(x)/length(x))


# Files with generated missingness
missing.files <- c(
'missing.age.mar.sex.05',
'missing.age.mar.sex.25',
'missing.age.mar.sex.50',
'missing.age.mcar.05',
'missing.age.mcar.25',
'missing.age.mcar.50',
'missing.age.mnar.05',
'missing.age.mnar.25',
'missing.age.mnar.50',
'missing.usr.mar.sex.05',
'missing.usr.mar.sex.25',
'missing.usr.mar.sex.50',
'missing.usr.mcar.05',
'missing.usr.mcar.25',
'missing.usr.mcar.50',
'missing.usr.mnar.05',
'missing.usr.mnar.25',
'missing.usr.mnar.50',
'missing.y1v19.mar.sex.05',
'missing.y1v19.mar.sex.25',
'missing.y1v19.mar.sex.50',
'missing.y1v19.mcar.05',
'missing.y1v19.mcar.25',
'missing.y1v19.mcar.50',
'missing.y1v19.mnar.05',
'missing.y1v19.mnar.25',
'missing.y1v19.mnar.50')

missing.variables <- c(
'age1',
'age1',
'age1',
'age1',
'age1',
'age1',
'age1',
'age1',
'age1',
'usroriginalnonemissing',
'usroriginalnonemissing',
'usroriginalnonemissing',
'usroriginalnonemissing',
'usroriginalnonemissing',
'usroriginalnonemissing',
'usroriginalnonemissing',
'usroriginalnonemissing',
'usroriginalnonemissing',
'y1v19',
'y1v19',
'y1v19',
'y1v19',
'y1v19',
'y1v19',
'y1v19',
'y1v19',
'y1v19'
)

matrix(c(missing.files, missing.variables), ncol=2)

# not.missing <- read.csv("newDataClean-1.csv")	
not.missing <- read.csv("not.missing.csv")
	
# These guys are real troublemakers
not.missing <- subset(not.missing, select = -c(VarietyScore, VarietyScore2, X, X.1))
# MTP singularizes and can be removed
not.missing <- subset(not.missing, select = -c(MotorTheftPrev, AttackPrev, y1v117 ))



not.missing <- subset(not.missing, select = -c(MissingUSRDummy ))

not.missing <- subset(not.missing, select = -c(HDRural, HDRuralMNAR, HDSuburban, HDSuburbanMNAR, HDUrban, HDUrbanMNAR, HotDeckUSRMAR, HotDeckUSRMNAR, Rural, RuralMAR, RuralMNAR, Suburban, SuburbanMAR, Urban, UrbanMAR, UrbanMNAR, usr1MAR, usrNMNAR))



not.missing$p25 <- as.logical(not.missing$p25-1)
