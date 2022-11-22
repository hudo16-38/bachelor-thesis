library(foreign)
library(psych)
FILE.NAME <- "C:/Users/roman/Desktop/school/BS/bp/SAAVSspokoj.sav"
setwd("C:/Users/roman/Desktop/school/BS/bp")

#load data
data <- read.spss(FILE.NAME, to.data.frame=TRUE, reencode="utf-8")

#column names
col.names <- colnames(data)

#programmes
programmes <- unique(data$odbor)

converter <- function(x){
  if(any(x == c("Rozhodne súhlasím","Takmer všetci učitelia", "Takmer všetky predmety" ))){
    return(5)
  }
  
  if(any(x == c("Skôr súhlasím", "Väčšina učiteľov", "Väčšina predmetov"))){
    return(4)
  }
  
  if(any(x == c("Skôr nesúhlasím", "Menšina učiteľov", "Menšina predmetov"))){
    return(2)
  }
  
  if(any(x == c("Rozhodne nesúhlasím", "Takmer žiadni učitelia", "Takmer žiadne predmety"))){
    return(1)
  }
  if(x == "Netýka sa ma to"){
    return(3)
  } 
  return(x)
}

#conversion of data to numbers
new.data <- as.data.frame(apply(data, c(1,2), converter))


#retyping of char columns to numeric type
for(col in col.names[7:length(col.names)]){
  new.data[,col] = as.numeric(new.data[,col])
}


#TAKING DIMENSIONS
flexibility <- subset(new.data, select=7:11)

pred <- subset(new.data, select = 12:18)

teachers.teaching <- subset(new.data, select=19:25)

teachers.approach <- subset(new.data, select = 26:32)

rating <- subset(new.data, select = 33:37)

skills <- subset(new.data, select=38:43)

student.support <- subset(new.data, select=44:48)

totals <- subset(new.data, select=49:56)
#END OF DIMENSIONS

#tau-equivalent alpha
flexibility.alpha <- alpha(flexibility)
pred.alpha <- alpha(pred)
teachers.teaching.alpha <- alpha(teachers.teaching)
teachers.approach.alpha <- alpha(teachers.approach)
rating.alpha <- alpha(rating)
skills.alpha <- alpha(skills)
student.support.alpha <- alpha(student.support)

print("tau-equivalent alpha for flexibility")
print(flexibility.alpha$total)
print(strrep("-",20))

print("tau-equivalent alpha for pred")
print(pred.alpha$total)
print(strrep("-",20))

print("tau-equivalent alpha for teachers teaching")
print(teachers.teaching.alpha$total)
print(strrep("-",20))

print("tau-equivalent alpha for teachers approach")
print(teachers.approach.alpha$total)
print(strrep("-",20))

print("tau-equivalent alpha for rating")
print(rating.alpha$total)
print(strrep("-",20))

print("tau-equivalent alpha for skills")
print(skills.alpha$total)
print(strrep("-",20))

