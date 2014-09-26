#SOURCE Fall 2014 - Week 1

#Gather my working directory
getwd()

#See what files are in my working directory
list.files()

HeightWeight <- read.table("HandW.txt")
HeightWeight

#Help Pages
?read.table

HeightWeightH <- read.table("HandW.txt", header=TRUE)
HeightWeightH

#CSV files
Lizards <- read.csv("anolisData.csv")

Lizards

?read.csv
#Changing the assignment of missing data - R sees a '?'
#but can only read NAs.
LizardsNA <- read.csv("anolisData.csv", na.strings="?")
LizardsNA


#Assignments '<-'
X <- 5
X

Hello <- 2
Hello

5+2

X + Hello

#Combine values
Y <- c(2,5,9)
Y

X+Y
Hello+Y

#Checking data
head(Lizards)
?head

#Changing to view first 10 lines
head(Lizards, n=10)
head(x=Lizards, n=10)

#Structure of data
str(Lizards)
str(LizardsNA)

class(LizardsNA)
class(Hello)
class(Y)

list.files()

###
Carn<-read.table("carni.txt", header=TRUE, na.string="no")
head(Carn, 4)

str(Carn)

plot(Carn$range, Carn$size)

attach(Carn)
plot(range, size)
detach(Carn)
range

Y
range(Y)

ls()