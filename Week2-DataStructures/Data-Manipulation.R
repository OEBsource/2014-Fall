#Week 3 - Data Manipulation

#SetWD

#Check files
list.files()

#Import data
#missing data in the csv file
Lizards<-read.csv("anolisData.csv", header=TRUE, na.strings="?")

#Check our data
head(Lizards)
tail(Lizards)

#Check number of rows and columns
dim(Lizards)
ncol(Lizards)
nrow(Lizards)

#$ sign
Lizards$SVL
Lizards$attitude

#Indexing - []
#DATA[row,column]
#DATA[ ,7] - All rows, 7th column
Lizards[ ,2]
Lizards$SVL

#Row
Lizards[1, ]

#Selecting multiple rows and columns
Lizards[1:3, ]
Lizards[1:10, ]

Lizards[,1:10]

#Odd rows
c(1,3,5)
1:10
Lizards[c(1,3,5), ]

#Delete rows or columns
Lizards[ ,-1]
Lizards[-1:-10, ]

Lizards[ , c(-1,-3,-5)]
#Look at column names
names(Lizards)
Lizards[c(-1,-3,-5), ]

#Combine row and columns
Lizards[1:3,1:3]

#Assigning to a new name
Lizards10 <- Lizards[1:10, ]
LizardsDel10 <- Lizards [-11:-100, ]

#Playing with our new datasets
mean(Lizards)
?mean

class(Lizards)
class(Lizards$SVL)
class(Lizards[ ,2])

mean(Lizards$SVL)
Lizards

mean(Lizards$Awesomeness)
#Is it because not numeric?
class(Lizards$Awesomeness)

#This column has NAs - we need to tell the mean function this.
mean(Lizards$Awesomeness, na.rm=TRUE)
?median
median(Lizards$Awesomeness, na.rm=TRUE)

#Using the apply function
#Allows you to apply one function to a lot of data at once
#MARGIN (the second entry) - do you want to apply this to rows (1), or columns (2)
#Here we apply to mean - every argument after mean is associated with mean.
?apply
apply(Lizards[,2:9], 2, mean, na.rm=TRUE)
apply(Lizards[,2:9], 2, mean, na.rm=FALSE)

#Mean of rows 1 to 10
apply(Lizards[1:10,2:9], 1, mean, na.rm=TRUE)
#Median of columns 2:9
apply(Lizards[,2:9], 2, median, na.rm=TRUE)

#Transformations
#log
log(Lizards$SVL)

#Add to the lizrads datafile
Lizards$SVLlog <- log(Lizards$SVL)
head(Lizards)

#Overwrite SVL variable
Lizards$SVL <- log(Lizards$SVL)
head(Lizards)

#Save a file
#Note, R will not tell if you are overwriting the orginal file
?write.csv
write.csv(Lizards, "LizardsLog.csv")