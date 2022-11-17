library(foreign)
library(psych)
FILE.NAME <- "C:/Users/roman/Desktop/school/BS/bp/SAAVSspokoj.sav"

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

#conversion of data
new.data <- as.data.frame(apply(data, c(1,2), converter))


#TAKING DIMENSIONS

#flexibility
flex <- subset(new.data, select=7:11)

#pred
pred <- subset(new.data, select = 12:18)

#ucitVZD
ucitVZD <- subset(new.data, select=19:25)

#ucitPRSIST
ucitPRIST <- subset(new.data, select = 26:32)

#hodnot
hodnot <- subset(new.data, select = 33:37)

#zruc
zruc <- subset(new.data, select=38:43)

#podpor
podpor <- subset(new.data, select=44:48)
#END OF DIMENSIONS

