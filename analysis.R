library(foreign)
library(psych)
library(mokken)
library(mirt)
library(lme4)
library(robustlmm)

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
flexibility <- subset(new.data, select=5:9)

subjects <- subset(new.data, select = 10:16)

teachers.teaching <- subset(new.data, select=17:23)

teachers.approach <- subset(new.data, select = 24:30)

rating <- subset(new.data, select = 31:35)

skills <- subset(new.data, select=36:41)

student.support <- subset(new.data, select=42:46)

totals <- subset(new.data, select=47:54)
#END OF DIMENSIONS

#tau-equivalent alpha

flexibility.alpha <- alpha(flexibility)
subjects.alpha <- alpha(subjects)
teachers.teaching.alpha <- alpha(teachers.teaching)
teachers.approach.alpha <- alpha(teachers.approach)
rating.alpha <- alpha(rating)
skills.alpha <- alpha(skills)
student.support.alpha <- alpha(student.support)


alphas <- data.frame("index"=c(
    "flexibility.alpha",
    "subjects.alpha",
    "teachers.teaching.alpha",
    "teachers.approach.alpha",
    "rating.alpha",
    "skills.alpha",
    "student.support"
   ))

for(name in names(flexibility.alpha$total)){
  alphas[name] = c(NA)
}
alphas[1,2:10] = round(flexibility.alpha$total,4)
alphas[2,2:10] = round(subjects.alpha$total,4)
alphas[3,2:10] = round(teachers.teaching.alpha$total,4)
alphas[4,2:10] = round(teachers.approach.alpha$total,4)
alphas[5,2:10] = round(rating.alpha$total,4)
alphas[6,2:10] = round(skills.alpha$total,4)
alphas[7, 2:10] = round(student.support.alpha$total,4)

poly.flex <- polychoric(flexibility)
poly.flex.alpha <- alpha(poly.flex$rho)

poly.subjects <- polychoric(subjects)
poly.subjects.alpha <- alpha(poly.subjects$rho)

poly.teachers.teaching <- polychoric(teachers.teaching)
poly.teachers.teaching.alpha <- alpha(poly.teachers.teaching$rho)

poly.teachers.approach <- polychoric(teachers.approach)
poly.teachers.approach.alpha <- alpha(poly.teachers.approach$rho)

poly.rating <- polychoric(rating)
poly.rating.alpha <- alpha(poly.rating$rho)

poly.skills <- polychoric(skills)
poly.skills.alpha <- alpha(poly.skills$rho)

poly.student.support <- polychoric(student.support)
poly.student.support.alpha <- alpha(poly.student.support$rho)

#dataframe for alphas gotten from polychoric matrices
poly.alphas <- data.frame("index"=c(
  "poly.flexibility.alpha",
  "poly.subjects.alpha",
  "poly.teachers.teaching.alpha",
  "poly.teachers.approach.alpha",
  "poly.rating.alpha",
  "poly.skills.alpha",
  "poly.student.support.alpha"
))

for(name in names(flexibility.alpha$total)){
  poly.alphas[name] = c(NA)
}

poly.alphas[1,2:10] = round(poly.flex.alpha$total,4)
poly.alphas[2,2:10] = round(poly.subjects.alpha$total,4)
poly.alphas[3,2:10] = round(poly.teachers.teaching.alpha$total,4)
poly.alphas[4,2:10] = round(poly.teachers.approach.alpha$total,4)
poly.alphas[5,2:10] = round(poly.rating.alpha$total,4)
poly.alphas[6,2:10] = round(poly.skills.alpha$total,4)
poly.alphas[7, 2:10] = round(poly.student.support.alpha$total,4)


#scaling
coefH(flexibility)
coefH(subjects)
coefH(teachers.teaching)
coefH(teachers.approach)
coefH(rating)
coefH(skills)
coefH(student.support)

to.plot = FALSE

mon1 = check.monotonicity(flexibility)
summary(mon1)

mon2 = check.monotonicity(subjects)
summary(mon2)

mon3 = check.monotonicity(teachers.teaching)
summary(mon3)

mon4 = check.monotonicity(teachers.approach)
summary(mon4)

mon5 = check.monotonicity(rating)
summary(mon5)

mon6 = check.monotonicity(skills)
summary(mon6)

mon7 = check.monotonicity(student.support)
summary(mon7)

if(to.plot){
  for(mon in c(mon1, mon2, mon3, mon4, mon5, mon6, mon7)){
    plot(mon)
  }
}



m1 = mirt(flexibility, 1, itemtype="graded")
m2 = mirt(subjects, 1, itemtype="graded")
m3 = mirt(teachers.teaching, 1, itemtype="graded")
m4 = mirt(teachers.approach, 1, itemtype="graded")
m5 = mirt(rating, 1, itemtype="graded")
m6 = mirt(skills, 1, itemtype="graded")
m7 = mirt(student.support, 1, itemtype="graded")

for(m in c(m1, m2, m3, m4, m5, m6, m7)){
  M2(m,type="C2")
}

#jednoduche viacurovnove modely

l1 = lmer(flex~1+(1|VS/fakulta/odbor),data=new.data)
l2 = lmer(pred~1+(1|VS/fakulta/odbor),data=new.data)
l3 = lmer(ucitVZD~1+(1|VS/fakulta/odbor),data=new.data)
l4 = lmer(ucitPRIST~1+(1|VS/fakulta/odbor),data=new.data)
l5 = lmer(hodnot~1+(1|VS/fakulta/odbor),data=new.data)
l6 = lmer(zruc~1+(1|VS/fakulta/odbor),data=new.data)
l7 = lmer(podpor~1+(1|VS/fakulta/odbor),data=new.data)


#robustne viacurovnove modely
l1r = rlmer(flex~1+(1|VS/fakulta/odbor),data=new.data)
l2r = rlmer(pred~1+(1|VS/fakulta/odbor),data=new.data)
l3r = rlmer(ucitVZD~1+(1|VS/fakulta/odbor),data=new.data)
l4r = rlmer(ucitPRIST~1+(1|VS/fakulta/odbor),data=new.data)
l5r = rlmer(hodnot~1+(1|VS/fakulta/odbor),data=new.data)
l6r = rlmer(zruc~1+(1|VS/fakulta/odbor),data=new.data)
l7r = rlmer(podpor~1+(1|VS/fakulta/odbor),data=new.data)


#lepsi odhad
l1rA = rlmer(flex~1+(1|VS/fakulta/odbor),data=new.data, rho.sigma.b = psi2propII(smoothPsi, k = 2.28))
l2rA = rlmer(pred~1+(1|VS/fakulta/odbor),data=new.data, rho.sigma.b = psi2propII(smoothPsi, k = 2.28))
l3rA = rlmer(ucitVZD~1+(1|VS/fakulta/odbor),data=new.data, rho.sigma.b = psi2propII(smoothPsi, k = 2.28))
l4rA = rlmer(ucitPRIST~1+(1|VS/fakulta/odbor),data=new.data, rho.sigma.b = psi2propII(smoothPsi, k = 2.28))
l5rA = rlmer(hodnot~1+(1|VS/fakulta/odbor),data=new.data, rho.sigma.b = psi2propII(smoothPsi, k = 2.28))
l6rA = rlmer(zruc~1+(1|VS/fakulta/odbor),data=new.data, rho.sigma.b = psi2propII(smoothPsi, k = 2.28))
l7rA = rlmer(podpor~1+(1|VS/fakulta/odbor),data=new.data, rho.sigma.b = psi2propII(smoothPsi, k = 2.28))



compare(l1,l1r,l1rA)

ranef(l1)
ranef(l1r)
ranef(l1rA)


#veci do prezentacie



prez = data.frame("index"=c(
  "flexibility",
  "subjects",
  "teachers.teaching",
  "teachers.approach",
  "rating",
  "skills",
  "student.support"
))
prez["standard"] = alphas["std.alpha"]
prez["polychoric"] = poly.alphas["std.alpha"]
