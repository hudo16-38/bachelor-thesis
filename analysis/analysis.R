library(foreign)
library(psych)
library(mokken)
library(mirt)
library(lme4)
library(robustlmm)
library(eRm)


FILE.NAME <- "C:/Users/roman/Desktop/school/BS/bp/analysis/SAAVSspokoj.sav"
setwd("C:/Users/roman/Desktop/school/BS/bp/analysis")
alpha = psych::alpha

#load data
data <- read.spss(FILE.NAME, to.data.frame=TRUE, reencode="utf-8")

programy <- unique(data$odbor)

konverter <- function(x){
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

#konverzia slovnych odpovedi na cisla
nove.data = as.data.frame(apply(data, c(1,2), konverter))

#nazvy stlpcov
stlpce <- colnames(nove.data)

#vyhodenie duplicitnych stlpcov
drop = c("VSa", "fakulta1")
nove.data = nove.data[, !stlpce %in% drop]

#nove nazvy stlpcov
stlpce = colnames(nove.data)

#konverzia dat na numericky typ
for(col in stlpce[5:length(stlpce)]){
  nove.data[,col] = as.numeric(nove.data[,col])
}


#vyber jednotlivych dimenzii
flexibilita <- subset(nove.data, select=5:9)

predmety <- subset(nove.data, select = 10:16)

ucitelia.vyucba <- subset(nove.data, select=17:23)

ucitelia.pristup <- subset(nove.data, select = 24:30)

hodnotenie <- subset(nove.data, select = 31:35)

zrucnosti <- subset(nove.data, select=36:41)

studenti.podpora <- subset(nove.data, select=42:46)

total <- subset(nove.data, select=47:53)
#############################################################

#Cronbachovo alfa

flexibilita.alpha <- alpha(flexibilita)
predmety.alpha <- alpha(predmety)
ucitelia.vyucba.alpha <- alpha(ucitelia.vyucba)
ucitelia.pristup.alpha <- alpha(ucitelia.pristup)
hodnotenie.alpha <- alpha(hodnotenie)
zrucnosti.alpha <- alpha(zrucnosti)
studenti.podpora.alpha <- alpha(studenti.podpora)


flexibilita.alpha
predmety.alpha
ucitelia.vyucba.alpha
ucitelia.pristup.alpha
hodnotenie.alpha
zrucnosti.alpha
studenti.podpora.alpha

#Polychoricka alfa
poly.flex = polychoric(flexibilita)
poly.flex.alpha = alpha(poly.flex$rho)

poly.predmety = polychoric(predmety)
poly.predmety.alpha = alpha(poly.predmety$rho)

poly.ucitelia.vyucba = polychoric(ucitelia.vyucba)
poly.ucitelia.vyucba.alpha = alpha(poly.ucitelia.vyucba$rho)

poly.ucitelia.pristup = polychoric(ucitelia.pristup)
poly.ucitelia.pristup.alpha = alpha(poly.ucitelia.pristup$rho)

poly.hodnotenie = polychoric(hodnotenie)
poly.hodnotenie.alpha = alpha(poly.hodnotenie$rho)

poly.zrucnosti = polychoric(zrucnosti)
poly.zrucnosti.alpha = alpha(poly.zrucnosti$rho)

poly.studenti.podpora <- polychoric(studenti.podpora)
poly.studenti.podpora.alpha <- alpha(poly.studenti.podpora$rho)

poly.flex.alpha
poly.predmety.alpha
poly.ucitelia.vyucba.alpha
poly.ucitelia.pristup.alpha
poly.hodnotenie.alpha
poly.zrucnosti.alpha
poly.studenti.podpora.alpha
#######################################################################
#koeficienty skalovatelnost
coefH(flexibilita)
coefH(predmety)
coefH(ucitelia.vyucba)
coefH(ucitelia.pristup)
coefH(hodnotenie)
coefH(zrucnosti)
coefH(studenti.podpora)


#overenie monotonnosti
mon1 = check.monotonicity(flexibilita)
summary(mon1)
plot(mon1)

mon2 = check.monotonicity(predmety)
summary(mon2)
plot(mon2)

mon3 = check.monotonicity(ucitelia.vyucba)
summary(mon3)
plot(mon3)

mon4 = check.monotonicity(ucitelia.pristup)
summary(mon4)
plot(mon4)

mon5 = check.monotonicity(hodnotenie)
summary(mon5)
plot(mon5)

mon6 = check.monotonicity(zrucnosti)
summary(mon6)
plot(mon6)

mon7 = check.monotonicity(studenti.podpora)
summary(mon7)
plot(mon7)



#overenie lokalnej nezavisloti
check.flexibilta = check.ca(flexibilita, TRUE)
check.predmety = check.ca(predmety, TRUE)
check.ucitelia.vyucba = check.ca(ucitelia.vyucba, TRUE)
check.ucitelia.pristup = check.ca(ucitelia.pristup, TRUE)
check.hodnotenie = check.ca(hodnotenie, TRUE)
check.zrucnosti = check.ca(zrucnosti, TRUE)
check.studenti.podpora = check.ca(studenti.podpora, TRUE)

check.flexibilta
check.predmety
check.ucitelia.vyucba
check.ucitelia.pristup
check.hodnotenie
check.zrucnosti
check.studenti.podpora


#overenie dimenzionality
lowerbound = c(0.3, 0.35, 0.4, 0.45, 0.5)
  
flexibilita.aisp = aisp(flexibilita, search="ga", lowerbound = lowerbound)
predmety.aisp = aisp(predmety, search="ga", lowerbound = lowerbound)
ucitelia.vyucba.aisp = aisp(ucitelia.vyucba, search="ga", lowerbound = lowerbound)
ucitelia.pristup.aisp = aisp(ucitelia.pristup, search="ga", lowerbound = lowerbound)
hodnotenie.aisp = aisp(hodnotenie, search="ga", lowerbound = lowerbound)
zrucnosti.aisp = aisp(zrucnosti, search="ga", lowerbound = lowerbound)
studenti.podpora.aisp =aisp(studenti.podpora, search="ga", lowerbound = lowerbound)

flexibilita.aisp
predmety.aisp
ucitelia.vyucba.aisp
ucitelia.pristup.aisp
hodnotenie.aisp
zrucnosti.aisp
studenti.podpora.aisp
############################################################
#uprava dimenzii

ucitelia.pristup <- subset(nove.data, select = c(25, 27:30))
predmety <- subset(nove.data, select = 10:13)

ucitelia.pristup.alpha = alpha(ucitelia.pristup)
predmety.alpha = alpha(predmety)

poly.ucitelia.pristup = polychoric(ucitelia.pristup)
poly.ucitelia.pristup.alpha = alpha(poly.ucitelia.pristup$rho)

poly.predmety = polychoric(predmety)
poly.predmety.alpha = alpha(poly.predmety$rho)

ucitelia.pristup.alpha
predmety.alpha
poly.ucitelia.pristup.alpha
poly.predmety.alpha

check.ucitelia.pristup = check.ca(ucitelia.pristup, TRUE)
check.predmety = check.ca(predmety, TRUE)

check.predmety
check.ucitelia.pristup

mon2 = check.monotonicity(predmety)
summary(mon2)
plot(mon2)


mon4 = check.monotonicity(ucitelia.pristup)
summary(mon4)
plot(mon4)

############################################################
m1 = mirt(flexibilita, 1, itemtype="graded")
m2 = mirt(predmety, 1, itemtype="graded")
m3 = mirt(ucitelia.vyucba, 1, itemtype="graded")
m4 = mirt(ucitelia.pristup, 1, itemtype="graded")
m5 = mirt(hodnotenie, 1, itemtype="graded")
m6 = mirt(zrucnosti, 1, itemtype="graded")
m7 = mirt(studenti.podpora, 1, itemtype="graded")

M2(m1,type="C2")
M2(m2,type="C2")
M2(m3,type="C2")
M2(m4,type="C2")
M2(m5,type="C2")
M2(m6,type="C2")
M2(m7,type="C2")



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
l8rA = rlmer(total~1+(1|VS/fakulta/odbor),data=new.data, rho.sigma.b = psi2propII(smoothPsi, k = 2.28))


compare(l1,l1r,l1rA)


