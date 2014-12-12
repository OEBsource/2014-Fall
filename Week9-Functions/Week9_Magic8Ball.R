##Final Week##
#Function writing

GiveMe10<-function(){
  X<-10
  X
}

GiveMe10()

#
AddTheseNumbers<-function(x, y){
  Z<-x+y
  Z
}

AddTheseNumbers(5,9)

MultiplyTheseNumbers<-function(x, y){
  Z<-x*y
  Z
}
MultiplyTheseNumbers(2,6)

#
stdErr <- function(x) {
  #Finds missing data, then removes it and places data into a placeholder.
  se.no.NA <- x[!is.na(x)]
  
  #The standard error function.
  #length() tells you how many values are in a vector.
  se <- sd(se.no.NA)/sqrt(length(se.no.NA))
  
  se
}

TestData1<-c(1,5,3,7,8,9,1,NA)
stdErr(TestData1)


#You can even use your own functions inside another function you wrote
FunctionErr <- function(x, y) {
  
  MTEr<-MultiplyTheseNumbers(x, y)
  ATEr<-MultiplyTheseNumbers(x, y)
    Output<-stdErr(c(MTEr, ATEr))
  Output
}

TestDataA<-c(1,2,3,4)
TestDataB<-c(5,6,7,8)

FunctionErr(TestDataA,TestDataB)

#Magic 8-ball
Magic8Ball<-function(){

Num<-round(runif(1, 1, 100),0)


if(Num>=0&Num<=20){
  
  print("Sure thing")
}

if(Num>=21&Num<=40){
  
  print("No chance")
}

if(Num>=41&Num<=60){
  
  print("Hmm... maybe")
}

if(Num>=61&Num<=80){
  
  print("Try again")
}

if(Num>=81&Num<=100){
  
  print("Trust your instincts!")
}

}

Magic8Ball()