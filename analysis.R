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


#print(flexibility.alpha)
#print(pred.alpha)
#print(teachers.teaching.alpha)
#print(teachers.approach.alpha)
#print(rating.alpha)
#print(skills.alpha)

alphas <- data.frame("index"=c(
    "flexibility.alpha",
    "pred.alpha",
    "teachers.teaching.alpha",
    "teachers.approach.alpha",
    "rating.alpha",
    "skills.alpha"
   ))

for(name in names(flexibility.alpha$total)){
  alphas[name] = c(NA)
}
alphas[1,2:10] = round(flexibility.alpha$total,4)
alphas[2,2:10] = round(pred.alpha$total,4)
alphas[3,2:10] = round(teachers.teaching.alpha$total,4)
alphas[4,2:10] = round(teachers.approach.alpha$total,4)
alphas[5,2:10] = round(rating.alpha$total,4)
alphas[6,2:10] = round(skills.alpha$total,4)

poly.flex <- polychoric(flexibility)
poly.flex.alpha <- alpha(poly.flex)

poly.pred <- polychoric(pred)
poly.pred.alpha <- alpha(poly.pred)

poly.teachers.teaching <- polychoric(teachers.teaching)
poly.teachers.teaching.alpha <- alpha(poly.teachers.teaching )

poly.teachers.approach <- polychoric(teachers.approach)
poly.teachers.approach.alpha <- alpha(teachers.approach)

poly.rating <- polychoric(rating)
poly.rating.alpha <- alpha(poly.rating)

poly.skills <- polychoric(skills)
poly.skills.alpha <- alpha(poly.skills)
