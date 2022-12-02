library(foreign)
library(psych)
FILE.NAME <- "C:/Users/roman/Desktop/school/BS/bp/SAAVSspokoj.sav"
setwd("C:/Users/roman/Desktop/school/BS/bp")
alpha = psych::alpha

#load data
data <- read.spss(FILE.NAME, to.data.frame=TRUE, reencode="utf-8")

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
  return(gsub(' ','',x))
}

#conversion of data to numbers
new.data <- as.data.frame(apply(data, c(1,2), converter))
drop = c("VSa", "fakulta1")
new.data = new.data[, !(names(new.data) %in% drop)]

#column names
col.names <- colnames(new.data)

#retyping of char columns to numeric type
for(col in col.names[5:length(col.names)]){
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
poly.flex.alpha <- alpha(poly.flex$rho)

poly.pred <- polychoric(pred)
poly.pred.alpha <- alpha(poly.pred$rho)

poly.teachers.teaching <- polychoric(teachers.teaching)
poly.teachers.teaching.alpha <- alpha(poly.teachers.teaching$rho)

poly.teachers.approach <- polychoric(teachers.approach)
poly.teachers.approach.alpha <- alpha(poly.teachers.approach$rho)

poly.rating <- polychoric(rating)
poly.rating.alpha <- alpha(poly.rating$rho)

poly.skills <- polychoric(skills)
poly.skills.alpha <- alpha(poly.skills$rho)

#dataframe for alphas gotten from polychoric matrices
poly.alphas <- data.frame("index"=c(
  "poly.flexibility.alpha",
  "poly.pred.alpha",
  "poly.teachers.teaching.alpha",
  "poly.teachers.approach.alpha",
  "poly.rating.alpha",
  "poly.skills.alpha"
))

for(name in names(flexibility.alpha$total)){
  poly.alphas[name] = c(NA)
}

poly.alphas[1,2:10] = round(poly.flex.alpha$total,4)
poly.alphas[2,2:10] = round(poly.pred.alpha$total,4)
poly.alphas[3,2:10] = round(poly.teachers.teaching.alpha$total,4)
poly.alphas[4,2:10] = round(poly.teachers.approach.alpha$total,4)
poly.alphas[5,2:10] = round(poly.rating.alpha$total,4)
poly.alphas[6,2:10] = round(poly.skills.alpha$total,4)


#universities
euba = totals[new.data$VS == "EU",]
uk = totals[new.data$VS == "UK",]
ukf = totals[new.data$VS == "UKF",]

#faculties
fmfi = new.data[new.data$fakulta == "FMFI",]
f = factor(fmfi$odbor)
plot(fmfi$total, col = f )
legend(1, legend = f)

prez = data.frame("index"=c(
  "flexibility",
  "pred",
  "teachers.teaching",
  "teachers.approach",
  "rating",
  "skills"
))
prez["standard"] = alphas["std.alpha"]
prez["polychoric"] = poly.alphas["std.alpha"]
